use swc_common::DUMMY_SP;
// use swc_atoms::{js_word, Atom, JsWord};
use swc_common::{
  //   comments::{Comment, CommentKind, Comments},
  //   errors::HANDLER,
  //   iter::IdentifyLast,
  //   sync::Lrc,
  util::take::Take,
  //   FileName, Mark, SourceMap,
  Span,
  Spanned,
};
use swc_ecma_ast::*;
use swc_ecma_utils::prepend_stmt;
use swc_ecma_visit::{
  as_folder, noop_visit_mut_type, Fold, VisitMut, VisitMutWith,
};

struct JsxString {
  next_index: usize,
  templates: Vec<(usize, Vec<String>)>,
}

impl Default for JsxString {
  fn default() -> Self {
    Self {
      next_index: 0,
      templates: vec![],
    }
  }
}

fn create_tpl_binding_name(index: usize) -> String {
  format!("$$_tpl_{index}")
}

fn normalize_dom_attr_name(name: &str) -> String {
  match name {
    "htmlFor" => "for".to_string(),
    "className" => "class".to_string(),
    _ => name.to_string(),
  }
}

fn serialize_jsx_element_to_string_vec(
  el: JSXElement,
) -> (Vec<String>, Vec<Box<Expr>>) {
  let name = match el.opening.name {
    JSXElementName::Ident(ident) => ident.sym.to_string(),
    _ => todo!(),
  };

  let mut strings: Vec<String> = vec![String::from("<")];
  let mut dynamic_exprs: Vec<Box<Expr>> = vec![];

  strings.last_mut().unwrap().push_str(name.as_str());

  if !el.opening.attrs.is_empty() {
    for attr in el.opening.attrs.iter() {
      let mut is_dynamic = false;
      let mut serialized_attr = String::new();
      let mut name = "".to_string();
      // <button class="btn">
      match attr {
        JSXAttrOrSpread::JSXAttr(jsx_attr) => {
          match &jsx_attr.name {
            JSXAttrName::Ident(ident) => {
              name = ident.sym.to_string();
              let serialized = normalize_dom_attr_name(&name);
              serialized_attr.push_str(serialized.as_str());
            }
            JSXAttrName::JSXNamespacedName(_) => todo!(),
          };
          let Some(attr_value) = &jsx_attr.value else {
            // <input required />
            strings.last_mut().unwrap().push_str(" ");
            strings.last_mut().unwrap().push_str(&serialized_attr);
            continue;
          };
          match attr_value {
            JSXAttrValue::Lit(lit) => match lit {
              Lit::Str(string_lit) => {
                serialized_attr.push_str("=\"");
                serialized_attr.push_str(string_lit.value.to_string().as_str());
              }
              Lit::Bool(_) => todo!(),
              Lit::Null(_) => todo!(),
              Lit::Num(_) => todo!(),
              Lit::BigInt(_) => todo!(),
              Lit::Regex(_) => todo!(),
              Lit::JSXText(_) => todo!(),
            },
            JSXAttrValue::JSXExprContainer(jsx_expr_container) => {
              strings.push("".to_string());
              is_dynamic = true;
              // eprintln!("jsx_expr_container {:#?}", jsx_expr_container);
              match &jsx_expr_container.expr {
                JSXExpr::JSXEmptyExpr(_) => todo!(),
                JSXExpr::Expr(expr) => {
                  let obj_expr = Box::new(Expr::Object(ObjectLit {
                    span: DUMMY_SP,
                    props: vec![PropOrSpread::Prop(Box::new(Prop::KeyValue(
                      KeyValueProp {
                        key: PropName::Str(Str {
                          span: DUMMY_SP,
                          value: name.into(),
                          raw: None,
                        }),
                        value: expr.clone(),
                      },
                    )))],
                  }));
                  dynamic_exprs.push(obj_expr);
                }
              }
            }
            JSXAttrValue::JSXElement(_) => todo!(),
            JSXAttrValue::JSXFragment(_) => todo!(),
          }
        }
        JSXAttrOrSpread::SpreadElement(_) => todo!(),
      };
      serialized_attr.push_str("\"");
      if !is_dynamic {
        strings.last_mut().unwrap().push_str(" ");
        strings
          .last_mut()
          .unwrap()
          .push_str(&serialized_attr.as_str());
      }
    }
  }

  // TODO: hoist out or make it as an option to the transform
  // See: https://developer.mozilla.org/en-US/docs/Glossary/Void_element
  let void_elements = vec![
    "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta",
    "param", "source", "track", "wbr",
  ];

  if void_elements.iter().any(|&item| item == name) {
    strings.last_mut().unwrap().push_str(" />");
    return (strings, dynamic_exprs);
  }

  strings.last_mut().unwrap().push_str(">");

  for child in el.children.iter() {
    match child {
      JSXElementChild::JSXText(jsx_text) => {
        strings
          .last_mut()
          .unwrap()
          .push_str(jsx_text.value.to_string().as_str());
      }
      JSXElementChild::JSXExprContainer(jsx_expr_container) => {
        match &jsx_expr_container.expr {
          JSXExpr::JSXEmptyExpr(_jsx_empty_expr) => todo!(),
          JSXExpr::Expr(expr) => match &**expr {
            Expr::Ident(ident) => {
              strings.push("".to_string());
              dynamic_exprs.push(Box::new(Expr::Ident(ident.clone())));
            }
            _ => todo!(),
          },
        }
      }
      JSXElementChild::JSXSpreadChild(_) => todo!(),
      JSXElementChild::JSXElement(_) => todo!(),
      JSXElementChild::JSXFragment(_) => todo!(),
    }
  }

  let closing_tag = format!("</{}>", name);
  strings.last_mut().unwrap().push_str(closing_tag.as_str());

  (strings, dynamic_exprs)
}

impl JsxString {
  fn generate_template_join(
    &mut self,
    template_index: usize,
    el: JSXElement,
  ) -> Expr {
    let name = create_tpl_binding_name(template_index);
    let span = el.span();

    let (static_strs, dynamic_exprs) = serialize_jsx_element_to_string_vec(el);

    self.templates.push((template_index, static_strs));

    let mut args: Vec<ExprOrSpread> =
      Vec::with_capacity(1 + std::cmp::max(1, dynamic_exprs.len()));
    args.push(ExprOrSpread {
      spread: None,
      expr: Box::new(Expr::Ident(Ident::new(name.into(), DUMMY_SP))),
    });
    if dynamic_exprs.is_empty() {
      args.push(ExprOrSpread {
        spread: None,
        expr: Box::new(Expr::Lit(Lit::Null(Null { span: DUMMY_SP }))),
      });
    } else {
      for dynamic_expr in dynamic_exprs.into_iter() {
        args.push(ExprOrSpread {
          spread: None,
          expr: dynamic_expr,
        });
      }
    }

    // renderFunction($$_tpl_1, null);
    Expr::Call(CallExpr {
      span,
      callee: Callee::Expr(Box::new(Expr::Ident(Ident::new(
        "renderFunction".into(),
        DUMMY_SP,
      )))),
      args,
      type_args: Default::default(),
    })
  }
}

impl VisitMut for JsxString {
  noop_visit_mut_type!();

  fn visit_mut_module(&mut self, module: &mut Module) {
    // eprintln!("ast {:#?}", module);
    module.visit_mut_children_with(self);
    for (idx, strings) in self.templates.iter().rev() {
      let elems: Vec<Option<ExprOrSpread>> = strings
        .iter()
        .map(|el| {
          Some(ExprOrSpread {
            spread: None,
            expr: Box::new(Expr::Lit(Lit::Str(Str {
              span: DUMMY_SP,
              value: el.as_str().into(),
              raw: None,
            }))),
          })
        })
        .collect();

      prepend_stmt(
        &mut module.body,
        ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
          span: DUMMY_SP,
          kind: VarDeclKind::Const,
          declare: false,
          decls: vec![VarDeclarator {
            span: DUMMY_SP,
            name: Pat::Ident(BindingIdent {
              id: Ident::new(create_tpl_binding_name(*idx).into(), DUMMY_SP),
              type_ann: None,
            }),
            init: Some(Box::new(Expr::Array(ArrayLit {
              span: DUMMY_SP,
              elems,
            }))),
            definite: false,
          }],
        })))),
      )
    }
  }

  fn visit_mut_expr(&mut self, expr: &mut Expr) {
    if let Expr::JSXElement(el) = expr {
      // TODO:
      // 1. create a `_tpl_<name>` which is an array literal
      // 2. transform the element into a list of string literals and
      //    push them to the `_tpl_<name>` literal node
      // 3. change the `expr` to be `_tpl_<name>.join("");`
      self.next_index += 1;
      *expr = self.generate_template_join(self.next_index, *el.take());
    } else if let Expr::JSXFragment(frag) = expr {
      // Empty fragments can be replaced with null. This is a minor
      // optimization, because Fragments are a special node type that
      // would need to be rendered. Most developers think of this as
      // rendering "nothing", which is true visually, but would still
      // render a Fragment for nothing
      // Case: <></>
      // TODO: This optimization is only valid if we're the topmost
      // jsx element
      if frag.children.is_empty() {
        *expr = Expr::Lit(Lit::Null(Null {
          span: frag.span.clone(),
        }));
        return;
      }

      todo!();
    } else if let Expr::Paren(ParenExpr {
      expr: inner_expr, ..
    }) = expr
    {
      if let Expr::JSXElement(_el) = &mut **inner_expr {
        todo!();
      } else if let Expr::JSXFragment(_frag) = &mut **inner_expr {
        todo!();
      }
    }

    expr.visit_mut_children_with(self);
  }
}

#[cfg(test)]
mod tests {
  use crate::swc::ast::Module;
  use crate::swc::codegen::text_writer::JsWriter;
  use crate::swc::codegen::Node;
  use crate::swc::common::FileName;
  use crate::swc::common::SourceMap;
  use crate::swc::parser::Parser;
  use crate::swc::parser::StringInput;
  use crate::swc::parser::Syntax;
  use crate::swc::parser::TsConfig;
  use crate::swc::visit::Fold;
  use crate::swc::visit::FoldWith;
  use crate::ModuleSpecifier;
  use crate::ES_VERSION;
  use pretty_assertions::assert_eq;
  use std::rc::Rc;

  use super::*;

  #[test]
  fn basic_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div>Hello!</div>;
const b = <div>Hello {name}!</div>;
const c = <button class="btn" onClick={onClick}>Hello {name}!</button>;
"#,
      r#"const $$_tpl_1 = [
  "<div>Hello!</div>"
];
const $$_tpl_2 = [
  "<div>Hello ",
  "!</div>"
];
const $$_tpl_3 = [
  '<button class="btn"',
  ">Hello ",
  "!</button>"
];
const a = renderFunction($$_tpl_1, null);
const b = renderFunction($$_tpl_2, name);
const c = renderFunction($$_tpl_3, {
  "onClick": onClick
}, name);"#,
    );
  }

  #[test]
  fn convert_self_closing_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div />;"#,
      r#"const $$_tpl_1 = [
  "<div></div>"
];
const a = renderFunction($$_tpl_1, null);"#,
    );

    // Void elements
    test_transform(
      JsxString::default(),
      r#"const a = <br></br>;"#,
      r#"const $$_tpl_1 = [
  "<br />"
];
const a = renderFunction($$_tpl_1, null);"#,
    );
  }

  #[test]
  fn normalize_attr_name_test() {
    let mappings: Vec<(String, String)> = vec![
      ("htmlFor".to_string(), "for".to_string()),
      ("className".to_string(), "class".to_string()),
    ];

    for mapping in mappings.iter() {
      test_transform(
        JsxString::default(),
        format!("const a = <label {}=\"foo\">label</label>", &mapping.0)
          .as_str(),
        format!(
          "const $$_tpl_1 = [\n  '<label {}=\"foo\">label</label>'\n];\nconst a = renderFunction($$_tpl_1, null);",
          &mapping.1
        )
        .as_str(),
      );
    }
  }

  #[test]
  fn boolean_attr_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <input type="checkbox" checked />;"#,
      r#"const $$_tpl_1 = [
  '<input type="checkbox" checked />'
];
const a = renderFunction($$_tpl_1, null);"#,
    );
  }

  #[test]
  fn empty_fragment_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <></>;"#,
      r#"const a = null;"#,
    );
  }

  #[ignore]
  #[test]
  fn fragment_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <>foo</>;"#,
      r#"const $$_tpl_1 = [
  'foo'
];
const a = renderFunction($$_tpl_1, null);"#,
    );
  }

  #[ignore]
  #[test]
  fn nested_elements_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div>foo<p>bar</p></div>;"#,
      r#"const $$_tpl_1 = [
  <div>foo<p>bar</p></div>
];
const a = renderFunction($$_tpl_1, null);"#,
    );
  }

  // TODO: What to do with keys?
  // TODO: What to do with components?
  //       1. Convert to function calls, this would make it insanely fast,
  //          but not sure how to handle that with islands.
  //       2. Call with normal transform (may be best for compat with Fresh)
  // TODO: Should we go with function calls for dynamic attributes instead
  //       of an object for DOM nodes? Would allow us to skip an allocation.
  //       { onClick: onClick } -> someFn("onClick", onClick)
  // TODO: HTMLEscape attribute names + text children
  // TODO: What to do with "dangerouslySetInnerHTML"?
  // TODO: Fresh specific: <Head> opt out? Or maybe move Fresh users to a
  //       different pattern
  // TODO: Fresh specific: what about island detection?

  //   #[test]
  //   fn basic_test_with_imports() {
  //     test_transform(
  //       JsxString::default(),
  //       r#"import * as assert from "https://deno.land/std/assert/mod.ts";
  // const a = <div>Hello!</div>;
  // const b = <div>Hello!</div>;"#,
  //       r#"const $$_tpl_1 = ["<div>Hello!</div>"];
  // const $$_tpl_2 = ["<div>Hello!</div>"];
  // import * as assert from "https://deno.land/std/assert/mod.ts";
  // const a = $$_tpl_1.join("");
  // const b = $$_tpl_2.join("");"#,
  //     );
  //   }

  #[track_caller]
  fn test_transform(
    transform: impl VisitMut,
    src: &str,
    expected_output: &str,
  ) {
    let (source_map, module) = parse(src);
    let mut transform_folder = as_folder(transform);
    let output = print(source_map, module.fold_with(&mut transform_folder));
    assert_eq!(output, format!("{}\n", expected_output));
  }

  fn parse(src: &str) -> (Rc<SourceMap>, Module) {
    let source_map = Rc::new(SourceMap::default());
    let source_file = source_map.new_source_file(
      FileName::Url(ModuleSpecifier::parse("file:///test.ts").unwrap()),
      src.to_string(),
    );
    let input = StringInput::from(&*source_file);
    let syntax = Syntax::Typescript(TsConfig {
      tsx: true,
      ..Default::default()
    });
    let mut parser = Parser::new(syntax, input, None);
    (source_map, parser.parse_module().unwrap())
  }

  fn print(source_map: Rc<SourceMap>, module: Module) -> String {
    let mut buf = vec![];
    {
      let mut writer =
        Box::new(JsWriter::new(source_map.clone(), "\n", &mut buf, None));
      writer.set_indent_str("  "); // two spaces
      let mut emitter = crate::swc::codegen::Emitter {
        cfg: crate::swc_codegen_config(),
        comments: None,
        cm: source_map,
        wr: writer,
      };
      module.emit_with(&mut emitter).unwrap();
    }
    String::from_utf8(buf).unwrap()
  }
}
