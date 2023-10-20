use swc_common::DUMMY_SP;
// use swc_atoms::{js_word, Atom, JsWord};
use swc_common::{
  //   comments::{Comment, CommentKind, Comments},
  //   errors::HANDLER,
  //   iter::IdentifyLast,
  //   sync::Lrc,
  util::take::Take,
  //   FileName, Mark, SourceMap,
  // Span,
  Spanned,
};
use swc_ecma_ast::*;
use swc_ecma_utils::prepend_stmt;
use swc_ecma_visit::{as_folder, noop_visit_mut_type, VisitMut, VisitMutWith};

struct JsxString {
  next_index: usize,
  templates: Vec<(usize, Vec<String>)>,
  development: bool,
  import_source: String,
  import_jsx: Option<Ident>,
  import_jsxs: Option<Ident>,
  import_fragment: Option<Ident>,
  import_jsx_ssr: Option<Ident>,
  import_jsx_attr: Option<Ident>,
}

impl Default for JsxString {
  fn default() -> Self {
    Self {
      next_index: 0,
      templates: vec![],
      development: false,
      import_source: "react".to_string(),
      import_jsx: None,
      import_jsxs: None,
      import_fragment: None,
      import_jsx_ssr: None,
      import_jsx_attr: None,
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
    // xlink:href was removed from SVG and isn't needed
    "xlinkHref" => "href".to_string(),
    _ => name.to_string(),
  }
}

// See: https://developer.mozilla.org/en-US/docs/Glossary/Void_element
fn is_void_element(name: &str) -> bool {
  match name {
    "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input"
    | "link" | "meta" | "param" | "source" | "track" | "wbr" => true,
    _ => false,
  }
}

fn null_arg() -> ExprOrSpread {
  ExprOrSpread {
    spread: None,
    expr: Box::new(Expr::Lit(Lit::Null(Null { span: DUMMY_SP }))),
  }
}

fn get_attr_name(jsx_attr: &JSXAttr) -> String {
  match &jsx_attr.name {
    // Case: <button class="btn">
    JSXAttrName::Ident(ident) => normalize_dom_attr_name(&ident.sym.as_ref()),
    // Case (svg only): <a xlink:href="#">...</a>
    JSXAttrName::JSXNamespacedName(_namespace_name) => {
      // TODO: Only support "xlink:href", but convert it to "href"
      todo!()
    }
  }
}

impl JsxString {
  fn serialize_jsx_element_to_string_vec(
    &mut self,
    el: JSXElement,
    strings: &mut Vec<String>,
    dynamic_exprs: &mut Vec<Expr>,
  ) {
    let ident = match el.opening.name {
      // Case: <div />
      JSXElementName::Ident(ident) => ident,
      _ => todo!(),
    };

    let name = ident.sym.to_string();

    // Components are serialized differently, because it is framework
    // specific.
    // Components are detected by checking if the character of the
    // opening identifier is an uppercase character.
    // Case: <Foo bar="123" />
    if name.chars().next().unwrap().is_ascii_uppercase() {
      let jsx_ident = match &self.import_jsx {
        Some(ident) => ident.clone(),
        None => {
          let jsx = if self.development { "_jsxDEV" } else { "_jsx" };
          let ident = Ident::new(jsx.into(), DUMMY_SP);
          self.import_jsx = Some(ident.clone());
          ident
        }
      };

      let mut args: Vec<ExprOrSpread> = vec![];
      args.push(ExprOrSpread {
        spread: None,
        expr: Box::new(Expr::Ident(ident)),
      });

      // Serialize component attributes
      // Case: <Foo />
      // Case: <Foo foo="1" bar={2} />
      // Case: <Foo baz={<div />} />
      if el.opening.attrs.is_empty() {
        args.push(null_arg())
      } else {
        let mut props: Vec<PropOrSpread> = vec![];
        for attr in el.opening.attrs.iter() {
          match attr {
            JSXAttrOrSpread::JSXAttr(jsx_attr) => {
              let attr_name = get_attr_name(jsx_attr);

              let prop_name = PropName::Str(Str {
                span: DUMMY_SP,
                value: attr_name.into(),
                raw: None,
              });

              // Case: <Foo required />
              let Some(attr_value) = &jsx_attr.value else {
                props.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
                  KeyValueProp {
                    key: prop_name,
                    value: Box::new(Expr::Lit(Lit::Bool(Bool {
                      span: DUMMY_SP,
                      value: true,
                    }))),
                  },
                ))));
                continue;
              };

              // Case: <div class="btn">
              // Case: <div class={"foo"}>
              // Case: <div class={2}>
              // Case: <div class={true}>
              // Case: <div class={null}>
              match attr_value {
                JSXAttrValue::Lit(lit) => {
                  props.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
                    KeyValueProp {
                      key: prop_name,
                      value: Box::new(Expr::Lit(lit.clone())),
                    },
                  ))));
                }
                JSXAttrValue::JSXExprContainer(jsx_expr_container) => {
                  match &jsx_expr_container.expr {
                    // This is treated as a syntax error in attributes
                    JSXExpr::JSXEmptyExpr(_) => continue,
                    JSXExpr::Expr(expr) => {
                      props.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
                        KeyValueProp {
                          key: prop_name,
                          value: expr.clone(),
                        },
                      ))));
                    }
                  }
                }
                // Cannot occur on DOM elements
                JSXAttrValue::JSXElement(_) => todo!(),
                // Cannot occur on DOM elements
                JSXAttrValue::JSXFragment(_) => todo!(),
              }
            }
            JSXAttrOrSpread::SpreadElement(_) => todo!(),
          }
        }

        let obj_expr = Box::new(Expr::Object(ObjectLit {
          span: DUMMY_SP,
          props: props,
        }));
        args.push(ExprOrSpread {
          spread: None,
          expr: obj_expr,
        });
      }

      // TODO: support raw function call option: <Foo /> -> Foo()
      let expr = Expr::Call(CallExpr {
        span: DUMMY_SP,
        callee: Callee::Expr(Box::new(Expr::Ident(jsx_ident))),
        args: args,
        type_args: None,
      });
      strings.push("".to_string());
      dynamic_exprs.push(expr);
      return;
    }

    if strings.is_empty() {
      strings.push("<".to_string());
    } else {
      strings.last_mut().unwrap().push_str("<");
    }
    strings.last_mut().unwrap().push_str(name.as_str());

    if !el.opening.attrs.is_empty() {
      for attr in el.opening.attrs.iter() {
        let mut is_dynamic = false;
        let mut serialized_attr = String::new();
        // Case: <button class="btn">
        match attr {
          JSXAttrOrSpread::JSXAttr(jsx_attr) => {
            let attr_name = get_attr_name(jsx_attr);

            serialized_attr.push_str(attr_name.as_str());

            // Case: <input required />
            let Some(attr_value) = &jsx_attr.value else {
              strings.last_mut().unwrap().push_str(" ");
              strings.last_mut().unwrap().push_str(&serialized_attr);
              continue;
            };

            // Case: <div class="btn">
            // Case: <div class={"foo"}>
            // Case: <div class={2}>
            // Case: <div class={true}>
            // Case: <div class={null}>
            match attr_value {
              JSXAttrValue::Lit(lit) => match lit {
                Lit::Str(string_lit) => {
                  serialized_attr.push_str("=\"");
                  serialized_attr
                    .push_str(string_lit.value.to_string().as_str());
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
                  // This is treated as a syntax error in attributes
                  JSXExpr::JSXEmptyExpr(_) => todo!(),
                  JSXExpr::Expr(expr) => {
                    let obj_expr = Expr::Object(ObjectLit {
                      span: DUMMY_SP,
                      props: vec![PropOrSpread::Prop(Box::new(
                        Prop::KeyValue(KeyValueProp {
                          key: PropName::Str(Str {
                            span: DUMMY_SP,
                            value: attr_name.into(),
                            raw: None,
                          }),
                          value: expr.clone(),
                        }),
                      ))],
                    });
                    dynamic_exprs.push(obj_expr);
                  }
                }
              }
              // Cannot occur on DOM elements
              JSXAttrValue::JSXElement(_) => todo!(),
              // Cannot occur on DOM elements
              JSXAttrValue::JSXFragment(_) => todo!(),
            }
          }
          // Case: <div {...props} />
          JSXAttrOrSpread::SpreadElement(jsx_spread_element) => {
            strings.last_mut().unwrap().push_str(" ");
            strings.push("".to_string());
            let obj_expr = Expr::Object(ObjectLit {
              span: DUMMY_SP,
              props: vec![PropOrSpread::Spread(SpreadElement {
                dot3_token: DUMMY_SP,
                expr: jsx_spread_element.expr.clone(),
              })],
            });
            dynamic_exprs.push(obj_expr);
            continue;
          }
        };
        serialized_attr.push_str("\"");
        if !is_dynamic {
          strings.last_mut().unwrap().push_str(" ");
          strings
            .last_mut()
            .unwrap()
            .push_str(serialized_attr.as_str());
        }
      }
    }

    // There are no self closing elements in HTML, only void elements.
    // Void elements are a fixed list of elements that cannot have
    // child nodes.
    // See https://developer.mozilla.org/en-US/docs/Glossary/Void_element
    // Case: <br />
    // Case: <meta />
    if is_void_element(&name) {
      strings.last_mut().unwrap().push_str(" />");
      return;
    }

    strings.last_mut().unwrap().push_str(">");

    for child in el.children.iter() {
      match child {
        // Case: <div>foo</div>
        JSXElementChild::JSXText(jsx_text) => {
          strings
            .last_mut()
            .unwrap()
            .push_str(jsx_text.value.to_string().as_str());
        }
        // Case: <div>{2 + 2}</div>
        JSXElementChild::JSXExprContainer(jsx_expr_container) => {
          match &jsx_expr_container.expr {
            // Empty JSX expressions can be ignored as they have no content
            // Case: <div>{}</div>
            // Case: <div>{/* fooo */}</div>
            JSXExpr::JSXEmptyExpr(_) => continue,
            JSXExpr::Expr(expr) => match &**expr {
              Expr::Ident(ident) => {
                strings.push("".to_string());
                dynamic_exprs.push(Expr::Ident(ident.clone()));
              }
              _ => todo!(),
            },
          }
        }
        // Case: <div><span /></div>
        JSXElementChild::JSXElement(jsx_element) => self
          .serialize_jsx_element_to_string_vec(
            *jsx_element.clone(),
            strings,
            dynamic_exprs,
          ),
        // Case: <div><></></div>
        JSXElementChild::JSXFragment(_) => todo!(),
        // Invalid, was part of an earlier JSX iteration, but no
        // transform supports it. Babel and TypeScript error when they
        // encounter this.
        JSXElementChild::JSXSpreadChild(_) => todo!(),
      }
    }

    let closing_tag = format!("</{}>", name);
    strings.last_mut().unwrap().push_str(closing_tag.as_str());
  }

  fn generate_template_join(
    &mut self,
    template_index: usize,
    el: JSXElement,
  ) -> Expr {
    let name = create_tpl_binding_name(template_index);
    let span = el.span();

    let mut static_strs: Vec<String> = vec![];
    let mut dynamic_exprs: Vec<Expr> = vec![];
    self.serialize_jsx_element_to_string_vec(
      el,
      &mut static_strs,
      &mut dynamic_exprs,
    );

    self.templates.push((template_index, static_strs));

    let mut args: Vec<ExprOrSpread> =
      Vec::with_capacity(1 + std::cmp::max(1, dynamic_exprs.len()));
    args.push(ExprOrSpread {
      spread: None,
      expr: Box::new(Expr::Ident(Ident::new(name.into(), DUMMY_SP))),
    });
    if dynamic_exprs.is_empty() {
      args.push(null_arg());
    } else {
      for dynamic_expr in dynamic_exprs.into_iter() {
        args.push(ExprOrSpread {
          spread: None,
          expr: Box::new(dynamic_expr),
        });
      }
    }

    // Case: _jsxssr($$_tpl_1, null);
    let jsx_ident = match &self.import_jsx_ssr {
      Some(ident) => ident.clone(),
      None => {
        let ident = Ident::new("_jsxssr".into(), DUMMY_SP);
        self.import_jsx_ssr = Some(ident.clone());
        ident
      }
    };

    Expr::Call(CallExpr {
      span,
      callee: Callee::Expr(Box::new(Expr::Ident(jsx_ident))),
      args,
      type_args: Default::default(),
    })
  }

  fn inject_runtime(&mut self, stmts: &mut Vec<ModuleItem>) {
    let mut imports: Vec<(Ident, Ident)> = vec![];

    if let Some(jsx_ident) = &self.import_jsx {
      let jsx_imported = if self.development { "jsxDev" } else { "jsx" };
      imports
        .push((jsx_ident.clone(), Ident::new(jsx_imported.into(), DUMMY_SP)))
    }

    if let Some(jsx_ident) = &self.import_jsx_ssr {
      imports.push((jsx_ident.clone(), Ident::new("jsxssr".into(), DUMMY_SP)))
    }

    if !imports.is_empty() {
      let jsx_runtime = if self.development {
        "jsx-dev-runtime"
      } else {
        "jsx-runtime"
      };

      let src = format!("{}/{}", self.import_source, jsx_runtime);

      let specifiers = imports
        .into_iter()
        .map(|(local, imported)| {
          ImportSpecifier::Named(ImportNamedSpecifier {
            span: DUMMY_SP,
            local,
            imported: Some(ModuleExportName::Ident(imported)),
            is_type_only: false,
          })
        })
        .collect();

      prepend_stmt(
        stmts,
        ModuleItem::ModuleDecl(ModuleDecl::Import(ImportDecl {
          span: DUMMY_SP,
          specifiers,
          src: Str {
            span: DUMMY_SP,
            raw: None,
            value: src.into(),
          }
          .into(),
          type_only: Default::default(),
          with: Default::default(),
        })),
      );
    }
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

    self.inject_runtime(&mut module.body);
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
        *expr = Expr::Lit(Lit::Null(Null { span: frag.span }));
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
  use crate::swc::visit::FoldWith;
  use crate::ModuleSpecifier;
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
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
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
const a = _jsxssr($$_tpl_1, null);
const b = _jsxssr($$_tpl_2, name);
const c = _jsxssr($$_tpl_3, {
  "onClick": onClick
}, name);"#,
    );
  }

  #[test]
  fn convert_self_closing_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div />;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div></div>"
];
const a = _jsxssr($$_tpl_1, null);"#,
    );

    // Void elements
    test_transform(
      JsxString::default(),
      r#"const a = <br></br>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<br />"
];
const a = _jsxssr($$_tpl_1, null);"#,
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
          "{}\nconst $$_tpl_1 = [\n  '<label {}=\"foo\">label</label>'\n];\nconst a = _jsxssr($$_tpl_1, null);",
          "import { jsxssr as _jsxssr } from \"react/jsx-runtime\";",
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
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<input type="checkbox" checked />'
];
const a = _jsxssr($$_tpl_1, null);"#,
    );
  }

  #[ignore]
  #[test]
  fn namespace_attr_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <a xlink:href="foo">foo</a>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<a href="foo">foo</a>',
];
const a = _jsxssr($$_tpl_1, null);"#,
    );
  }

  #[test]
  fn mixed_static_dynamic_props_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div foo="1" {...props} bar="2">foo</div>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<div foo="1" ',
  ' bar="2">foo</div>'
];
const a = _jsxssr($$_tpl_1, {
  ...props
});"#,
    );
  }

  #[test]
  fn empty_jsx_child_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <p>{}</p>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<p></p>"
];
const a = _jsxssr($$_tpl_1, null);"#,
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
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  'foo'
];
const a = _jsxssr($$_tpl_1, null);"#,
    );
  }

  #[test]
  fn nested_elements_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div>foo<p>bar</p></div>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div>foo<p>bar</p></div>"
];
const a = _jsxssr($$_tpl_1, null);"#,
    );
  }

  #[test]
  fn prop_spread_without_children_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div {...props} />;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div ",
  "></div>"
];
const a = _jsxssr($$_tpl_1, {
  ...props
});"#,
    );
  }

  #[test]
  fn prop_spread_with_children_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div {...props}>hello</div>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div ",
  ">hello</div>"
];
const a = _jsxssr($$_tpl_1, {
  ...props
});"#,
    );
  }

  #[test]
  fn component_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div><Foo /></div>;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div>",
  "</div>"
];
const a = _jsxssr($$_tpl_1, _jsx(Foo, null));"#,
    );
  }

  #[test]
  fn component_with_props_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <Foo required foo="1" bar={2} />;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  ""
];
const a = _jsxssr($$_tpl_1, _jsx(Foo, {
  "required": true,
  "foo": "1",
  "bar": 2
}));"#,
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
