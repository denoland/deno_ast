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
  templates: Vec<(usize, (Vec<String>, Vec<String>))>,
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

fn serialize_jsx_element_to_string_vec(
  el: JSXElement,
) -> (Vec<String>, Vec<String>) {
  let name = match el.opening.name {
    JSXElementName::Ident(ident) => ident.sym.to_string(),
    _ => todo!(),
  };

  let mut s = String::from("<");
  s.push_str(name.as_str());

  if !el.opening.attrs.is_empty() {
    for attr in el.opening.attrs.iter() {
      let mut is_dynamic = false;
      let mut serialized_attr = String::new();
      // <button class="btn">
      match attr {
        JSXAttrOrSpread::JSXAttr(jsx_attr) => {
          match &jsx_attr.name {
            JSXAttrName::Ident(ident) => {
              serialized_attr.push_str(ident.sym.to_string().as_str());
              serialized_attr.push_str("=\"");
            }
            JSXAttrName::JSXNamespacedName(_) => todo!(),
          };
          let Some(attr_value) = &jsx_attr.value else {
            continue;
          };
          match attr_value {
            JSXAttrValue::Lit(lit) => match lit {
              Lit::Str(string_lit) => {
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
              is_dynamic = true;
              eprintln!("jsx_expr_container {:#?}", jsx_expr_container);
            }
            JSXAttrValue::JSXElement(_) => todo!(),
            JSXAttrValue::JSXFragment(_) => todo!(),
          }
        }
        JSXAttrOrSpread::SpreadElement(_) => todo!(),
      };
      serialized_attr.push_str("\"");
      if !is_dynamic {
        s.push_str(" ");
        s.push_str(&serialized_attr.as_str());
      }
    }
  }

  if el.opening.self_closing {
    s.push_str(" />");
    return (vec![s], vec![]);
  }

  let mut strings: Vec<String> = vec![];
  let mut dynamic_exprs: Vec<String> = vec![];

  s.push_str(">");
  strings.push(s);

  for child in el.children.iter() {
    match child {
      JSXElementChild::JSXText(jsx_text) => {
        strings.push(jsx_text.value.to_string());
      }
      JSXElementChild::JSXExprContainer(jsx_expr_container) => {
        match &jsx_expr_container.expr {
          JSXExpr::JSXEmptyExpr(_jsx_empty_expr) => todo!(),
          JSXExpr::Expr(expr) => match &**expr {
            Expr::Ident(ident) => {
              dynamic_exprs.push(ident.sym.to_string());
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
  strings.push(closing_tag);

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

    self
      .templates
      .push((template_index, serialize_jsx_element_to_string_vec(el)));

    // $$_tpl_1.join("");
    Expr::Call(CallExpr {
      span,
      callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
        span: DUMMY_SP,
        obj: Box::new(Expr::Ident(Ident::new(name.into(), DUMMY_SP))),
        prop: MemberProp::Ident(Ident::new("join".into(), DUMMY_SP)),
      }))),
      args: vec![ExprOrSpread {
        spread: None,
        expr: Box::new(Expr::Lit(Lit::Str(Str {
          span: DUMMY_SP,
          value: "".into(),
          raw: None,
        }))),
      }],
      type_args: Default::default(),
    })
  }
}

impl VisitMut for JsxString {
  noop_visit_mut_type!();

  fn visit_mut_module(&mut self, module: &mut Module) {
    eprintln!("ast {:#?}", module);
    module.visit_mut_children_with(self);
    for (idx, (strings, dynamic_exprs)) in self.templates.iter().rev() {
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
    } else if let Expr::JSXFragment(_frag) = expr {
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
      r#"const $$_tpl_1 = ["<div>Hello!</div>"];
const $$_tpl_2 = ["<div>Hello", "!</div>"];
const $$_tpl_3 = ["<button class=\"btn\"", ">Hello ", "!</button>];
const a = $$_tpl_1.join("");
const b = renderFunction($$_tpl_2, name);
const c = renderFunction($$_tpl_3, { onClick }, name);"#,
    );
  }

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
