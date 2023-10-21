use swc_common::DUMMY_SP;
// use swc_atoms::{js_word, Atom, JsWord};
use swc_common::Spanned;
use swc_ecma_ast::*;
use swc_ecma_utils::prepend_stmt;
use swc_ecma_utils::quote_ident;
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
  matches!(
    name,
    "area"
      | "base"
      | "br"
      | "col"
      | "embed"
      | "hr"
      | "img"
      | "input"
      | "link"
      | "meta"
      | "param"
      | "source"
      | "track"
      | "wbr"
  )
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
    JSXAttrName::Ident(ident) => normalize_dom_attr_name(ident.sym.as_ref()),
    // Case (svg only): <a xlink:href="#">...</a>
    JSXAttrName::JSXNamespacedName(_namespace_name) => {
      // TODO: Only support "xlink:href", but convert it to "href"
      todo!()
    }
  }
}

/// Convert a JSXMemberExpr to MemberExpr. We offload this to a
/// function because conversion is recursive.
fn jsx_member_expr_to_normal(jsx_member_expr: &JSXMemberExpr) -> MemberExpr {
  MemberExpr {
    span: DUMMY_SP,
    obj: match jsx_member_expr.obj.clone() {
      JSXObject::Ident(ident) => Box::new(Expr::Ident(ident.clone())),
      JSXObject::JSXMemberExpr(_) => {
        todo!()
      }
    },
    prop: MemberProp::Ident(jsx_member_expr.prop.clone()),
  }
}

fn is_serializable(opening: &JSXOpeningElement) -> bool {
  match opening.name.clone() {
    // Case: <div />
    JSXElementName::Ident(ident) => {
      let name = ident.sym.to_string();
      // Component identifiers start with an uppercase character and
      // they cannot be safely serialized. So we'll apply the default
      // JSX transform
      // Case: <Foo bar="123" />
      if name.chars().next().unwrap().is_ascii_uppercase() {
        return false;
      }

      if opening.attrs.is_empty() {
        return true;
      }

      !opening.attrs.clone().iter().any(|attr| match attr {
        JSXAttrOrSpread::SpreadElement(_) => true,
        JSXAttrOrSpread::JSXAttr(attr) => {
          let name = get_attr_name(&attr);
          match name.as_str() {
            "dangerouslySetInnerHTML" => true,
            _ => false,
          }
        }
      })
    }
    _ => false,
  }
}

fn string_lit_expr(str: String) -> Expr {
  Expr::Lit(Lit::Str(Str {
    span: DUMMY_SP,
    value: str.to_string().as_str().into(),
    raw: None,
  }))
}

impl JsxString {
  /// Mark `jsx` or `jsxDEV` as being used and return the appropriate
  /// identifier.
  fn get_jsx_identifier(&mut self) -> Ident {
    match &self.import_jsx {
      Some(ident) => ident.clone(),
      None => {
        let jsx = if self.development { "_jsxDEV" } else { "_jsx" };
        let ident = Ident::new(jsx.into(), DUMMY_SP);
        self.import_jsx = Some(ident.clone());
        ident
      }
    }
  }

  /// Mark `jsxssr` as being used and return the identifier.
  fn get_jsx_ssr_identifier(&mut self) -> Ident {
    match &self.import_jsx_ssr {
      Some(ident) => ident.clone(),
      None => {
        let ident = Ident::new("_jsxssr".into(), DUMMY_SP);
        self.import_jsx_ssr = Some(ident.clone());
        ident
      }
    }
  }

  /// Mark `jsxattr` as being used and return the identifier.
  fn get_jsx_attr_identifier(&mut self) -> Ident {
    match &self.import_jsx_attr {
      Some(ident) => ident.clone(),
      None => {
        let ident = Ident::new("_jsxattr".into(), DUMMY_SP);
        self.import_jsx_attr = Some(ident.clone());
        ident
      }
    }
  }

  /// Serializes a JSXElement to a standard jsx expression. We use
  /// this function when we cannot safely serialize a JSXElement.
  ///
  /// Case: <div {...props} />
  /// Case: <Foo bar="1" />
  fn serialize_jsx_to_call_expr(&mut self, el: &JSXElement) -> CallExpr {
    let name_expr = match el.opening.name.clone() {
      // Case: <div />
      // Case: <Foo />
      JSXElementName::Ident(ident) => {
        let name = ident.sym.to_string();
        // Component identifiers start with an uppercase character
        // Case: <Foo bar="123" />
        if name.chars().next().unwrap().is_ascii_uppercase() {
          Expr::Ident(ident)
        } else {
          Expr::Lit(Lit::Str(Str {
            span: DUMMY_SP,
            value: name.into(),
            raw: None,
          }))
        }
      }
      // Case: <ctx.Provider />
      JSXElementName::JSXMemberExpr(jsx_member_expr) => {
        Expr::Member(jsx_member_expr_to_normal(&jsx_member_expr))
      }
      JSXElementName::JSXNamespacedName(_jsx_namespaced_name) => {
        todo!()
      }
    };

    let mut args: Vec<ExprOrSpread> = vec![];
    args.push(ExprOrSpread {
      spread: None,
      expr: Box::new(name_expr),
    });

    // Serialize attributes
    // Case: <Foo />
    // Case: <Foo foo="1" bar={2} />
    // Case: <Foo baz={<div />} />
    if el.opening.attrs.is_empty() && el.children.is_empty() {
      args.push(null_arg())
    } else {
      let mut props: Vec<PropOrSpread> = vec![];
      for attr in el.opening.attrs.iter() {
        match attr {
          JSXAttrOrSpread::JSXAttr(jsx_attr) => {
            let attr_name = get_attr_name(jsx_attr);
            let prop_name = PropName::Ident(quote_ident!(attr_name));

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

            // Case: <Foo class="btn">
            // Case: <Foo class={"foo"}>
            // Case: <Foo class={2}>
            // Case: <Foo class={true}>
            // Case: <Foo class={null}>
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
              // There is no valid way to construct these
              JSXAttrValue::JSXElement(_) => {}
              JSXAttrValue::JSXFragment(_) => {}
            }
          }
          // Case: <Foo {...props} />
          JSXAttrOrSpread::SpreadElement(spread_el) => {
            props.push(PropOrSpread::Spread(spread_el.clone()));
          }
        }
      }

      // Add children as a "children" prop.
      // TODO: Not sure if we should serialize all of them as one big
      // Fragment or serialize each child individually. Serializing
      // each child individually might increase compatibility because
      // of `React.Children` API, but it's not good for performance.
      let children_name = PropName::Ident(quote_ident!("children"));
      match el.children.len() {
        0 => {}
        1 => {
          let child = el.children[0].clone();
          match child {
            JSXElementChild::JSXText(jsx_text) => {
              props.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
                KeyValueProp {
                  key: children_name.clone(),
                  value: Box::new(string_lit_expr(jsx_text.value.to_string())),
                },
              ))));
            }
            JSXElementChild::JSXExprContainer(jsx_expr_container) => {
              match &jsx_expr_container.expr {
                // Empty JSX expressions can be ignored as they have no content
                // Case: <div>{}</div>
                // Case: <div>{/* fooo */}</div>
                JSXExpr::JSXEmptyExpr(_) => {}
                JSXExpr::Expr(expr) => {
                  props.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
                    KeyValueProp {
                      key: children_name.clone(),
                      value: expr.clone(),
                    },
                  ))));
                }
              }
            }
            // Case: <div><span /></div>
            JSXElementChild::JSXElement(jsx_element) => {
              let expr = self.serialize_jsx(&*jsx_element);

              props.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
                KeyValueProp {
                  key: children_name.clone(),
                  value: Box::new(expr),
                },
              ))));
            }
            // Case: <div><></></div>
            JSXElementChild::JSXFragment(_) => todo!(),
            // Invalid, was part of an earlier JSX iteration, but no
            // transform supports it. Babel and TypeScript error when they
            // encounter this.
            JSXElementChild::JSXSpreadChild(_) => {}
          }
        }
        _ => {
          let mut elems: Vec<Option<ExprOrSpread>> = vec![];
          for child in el.children.iter() {
            match child {
              // Case: <div>foo</div>
              JSXElementChild::JSXText(jsx_text) => {
                elems.push(Some(ExprOrSpread {
                  spread: None,
                  expr: Box::new(string_lit_expr(jsx_text.value.to_string())),
                }));
              }
              // Case: <div>{2 + 2}</div>
              JSXElementChild::JSXExprContainer(jsx_expr_container) => {
                match &jsx_expr_container.expr {
                  // Empty JSX expressions can be ignored as they have no content
                  // Case: <div>{}</div>
                  // Case: <div>{/* fooo */}</div>
                  JSXExpr::JSXEmptyExpr(_) => continue,
                  JSXExpr::Expr(expr) => {
                    elems.push(Some(ExprOrSpread {
                      spread: None,
                      expr: expr.clone(),
                    }));
                  }
                }
              }
              // Case: <div><span /></div>
              JSXElementChild::JSXElement(jsx_el) => {
                let expr = self.serialize_jsx(jsx_el);
                elems.push(Some(ExprOrSpread {
                  spread: None,
                  expr: Box::new(expr.clone()),
                }));
              }
              // Case: <div><></></div>
              JSXElementChild::JSXFragment(_) => todo!(),
              // Invalid, was part of an earlier JSX iteration, but no
              // transform supports it. Babel and TypeScript error when they
              // encounter this.
              JSXElementChild::JSXSpreadChild(_) => {}
            }
          }

          props.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
            KeyValueProp {
              key: children_name.clone(),
              value: Box::new(Expr::Array(ArrayLit {
                span: DUMMY_SP,
                elems,
              })),
            },
          ))));
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

    CallExpr {
      span: DUMMY_SP,
      callee: Callee::Expr(Box::new(Expr::Ident(self.get_jsx_identifier()))),
      args: args,
      type_args: None,
    }
  }

  fn convert_to_jsx_attr_call(&mut self, name: &str, expr: &Expr) -> CallExpr {
    let mut args: Vec<ExprOrSpread> = vec![];
    args.push(ExprOrSpread {
      spread: None,
      expr: Box::new(string_lit_expr(name.to_string())),
    });

    args.push(ExprOrSpread {
      spread: None,
      expr: Box::new(expr.clone()),
    });

    CallExpr {
      span: DUMMY_SP,
      callee: Callee::Expr(Box::new(Expr::Ident(
        self.get_jsx_attr_identifier(),
      ))),
      args,
      type_args: None,
    }
  }

  fn serialize_jsx_element_to_string_vec(
    &mut self,
    el: JSXElement,
    strings: &mut Vec<String>,
    dynamic_exprs: &mut Vec<Expr>,
  ) {
    let ident = match el.opening.name.clone() {
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
      let expr = Expr::Call(self.serialize_jsx_to_call_expr(&el));
      strings.push("".to_string());
      dynamic_exprs.push(expr);
      return;
    }

    // Edge case: If the JSX opening element contains a spread attribute
    // then it's not safe to serialize it. This is because existing code
    // relies on the object spread semantics where it can overwrite
    // existing properties or values passed from spread can be
    // overwritten by setting properties after the spread. What's more
    // is that the spread object could contain `props.children` which
    // we would miss.
    //
    // Case: <div {...props} />
    // Case: <div class="foo" {...{ class: "bar"}} />
    // Case: <div {...{ class: "foo"}} class="bar"}>foo</div>
    if !is_serializable(&el.opening) {
      let expr = Expr::Call(self.serialize_jsx_to_call_expr(&el));
      strings.push("".to_string());
      dynamic_exprs.push(expr);

      return;
    } else if strings.is_empty() {
      strings.push("".to_string());
    }

    strings.last_mut().unwrap().push_str("<");
    strings.last_mut().unwrap().push_str(name.as_str());

    if !el.opening.attrs.is_empty() {
      for attr in el.opening.attrs.iter() {
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
                  // Edge Case: The "key" attribute is a special one in
                  // most frameworks. Some frameworks may want to
                  // serialize it, other's don't. To support both use
                  // cases we'll always pass it to `jsxattr()` so that
                  // they can decide for themselves what to do with it.
                  // Case: <div key="123" />
                  if attr_name == "key" {
                    strings.last_mut().unwrap().push_str(" ");
                    strings.push("".to_string());
                    let expr = self.convert_to_jsx_attr_call(
                      &attr_name,
                      &string_lit_expr(string_lit.value.to_string()),
                    );
                    dynamic_exprs.push(Expr::Call(expr));
                    continue;
                  }

                  serialized_attr.push_str("=\"");
                  serialized_attr
                    .push_str(string_lit.value.to_string().as_str());
                  serialized_attr.push_str("\"");
                  strings.last_mut().unwrap().push_str(" ");
                  strings
                    .last_mut()
                    .unwrap()
                    .push_str(serialized_attr.as_str());
                }
                // I've never seen this being possible as it would
                // always be treated as an expression.
                Lit::Bool(_) => {}
                Lit::Null(_) => {}
                Lit::Num(_) => {}
                Lit::BigInt(_) => {}
                Lit::Regex(_) => {}
                Lit::JSXText(_) => {}
              },
              JSXAttrValue::JSXExprContainer(jsx_expr_container) => {
                strings.last_mut().unwrap().push_str(" ");
                strings.push("".to_string());
                // eprintln!("jsx_expr_container {:#?}", jsx_expr_container);
                match &jsx_expr_container.expr {
                  // This is treated as a syntax error in attributes
                  JSXExpr::JSXEmptyExpr(_) => {}
                  JSXExpr::Expr(expr) => {
                    let mut args: Vec<ExprOrSpread> = vec![];
                    args.push(ExprOrSpread {
                      spread: None,
                      expr: Box::new(string_lit_expr(attr_name.to_string())),
                    });

                    args.push(ExprOrSpread {
                      spread: None,
                      expr: expr.clone(),
                    });

                    let call_expr = Expr::Call(CallExpr {
                      span: DUMMY_SP,
                      callee: Callee::Expr(Box::new(Expr::Ident(
                        self.get_jsx_attr_identifier(),
                      ))),
                      args,
                      type_args: None,
                    });
                    dynamic_exprs.push(call_expr);
                  }
                }
              }
              // These makes no sense on as attribute on HTML elements
              // so we ignore them.
              JSXAttrValue::JSXElement(_) => {}
              JSXAttrValue::JSXFragment(_) => {}
            }
          }
          // Case: <div {...props} />
          JSXAttrOrSpread::SpreadElement(_) => {
            // This case is already handled earlier
            panic!();
          }
        };
      }
    }

    strings.last_mut().unwrap().push_str(">");

    // There are no self closing elements in HTML, only void elements.
    // Void elements are a fixed list of elements that cannot have
    // child nodes.
    // See https://developer.mozilla.org/en-US/docs/Glossary/Void_element
    // Case: <br /> -> <br>
    // Case: <meta /> -> <meta>
    if is_void_element(&name) {
      // Since self closing tags don't exist in HTML we don't need to
      // add the "/" character. If the "/" character is present,
      // browsers will ignore it anyway.
      return;
    }

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
            // Case: <div>{2 + 2}</div>
            // Case: <div>{foo}</div>
            // Case: <div>{() => null}</div>
            JSXExpr::Expr(expr) => {
              strings.push("".to_string());
              dynamic_exprs.push(*expr.clone());
            }
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
        JSXElementChild::JSXSpreadChild(_) => {}
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
    let span = el.span();

    let mut static_strs: Vec<String> = vec![];
    let mut dynamic_exprs: Vec<Expr> = vec![];
    self.serialize_jsx_element_to_string_vec(
      el,
      &mut static_strs,
      &mut dynamic_exprs,
    );

    // If both vectors have the same length, then we only
    // serialized a top level component node and can skip
    // the template wrapper.
    if dynamic_exprs.len() == 1 && static_strs.len() == dynamic_exprs.len() {
      return dynamic_exprs.into_iter().nth(0).unwrap();
    }

    let name = create_tpl_binding_name(template_index);

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
    let jsx_ident = self.get_jsx_ssr_identifier();

    Expr::Call(CallExpr {
      span,
      callee: Callee::Expr(Box::new(Expr::Ident(jsx_ident))),
      args,
      type_args: Default::default(),
    })
  }

  fn serialize_jsx(&mut self, el: &JSXElement) -> Expr {
    if is_serializable(&el.opening) {
      // These are now safe to be serialized
      // Case: <div foo="1" />
      self.next_index += 1;
      self.generate_template_join(self.next_index, el.clone())
    } else {
      // Case: <div {...props} />
      Expr::Call(self.serialize_jsx_to_call_expr(&el))
    }
  }

  fn inject_runtime(&mut self, stmts: &mut Vec<ModuleItem>) {
    let mut imports: Vec<(Ident, Ident)> = vec![];

    if let Some(jsx_ident) = &self.import_jsx {
      let jsx_imported = if self.development { "jsxDev" } else { "jsx" };
      imports
        .push((jsx_ident.clone(), Ident::new(jsx_imported.into(), DUMMY_SP)))
    }

    if let Some(jsx_ssr_ident) = &self.import_jsx_ssr {
      imports
        .push((jsx_ssr_ident.clone(), Ident::new("jsxssr".into(), DUMMY_SP)))
    }

    if let Some(jsx_attr_ident) = &self.import_jsx_attr {
      imports.push((
        jsx_attr_ident.clone(),
        Ident::new("jsxattr".into(), DUMMY_SP),
      ))
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
      *expr = self.serialize_jsx(&el);
    } else if let Expr::JSXFragment(frag) = expr {
      match frag.children.len() {
        0 => {
          // Empty fragments can be replaced with null. This is a minor
          // optimization, because Fragments are a special node type that
          // would need to be rendered. Most developers think of this as
          // rendering "nothing", which is true visually, but would still
          // render a Fragment for nothing
          // Case: <></>
          *expr = Expr::Lit(Lit::Null(Null { span: frag.span }));
        }
        1 => {
          // Flatten the fragment if it only has one child
          let child = frag.children[0].clone();
          // *expr = self.serialize_jsx(&child);
          match child {
            JSXElementChild::JSXText(jsx_text) => {
              *expr = string_lit_expr(jsx_text.value.to_string())
            }
            JSXElementChild::JSXExprContainer(jsx_expr_container) => {
              match &jsx_expr_container.expr {
                // Empty JSX expressions can be ignored as they have no content
                // Case: <>{}</>
                // Case: <>{/* fooo */}</>
                JSXExpr::JSXEmptyExpr(_) => {}
                JSXExpr::Expr(jsx_expr) => *expr = *jsx_expr.clone(),
              }
            }
            // Case: <><span /></>
            JSXElementChild::JSXElement(jsx_element) => {
              *expr = self.serialize_jsx(&*jsx_element);
            }
            // Case: <><></></>
            JSXElementChild::JSXFragment(_) => todo!(),
            // Invalid, was part of an earlier JSX iteration, but no
            // transform supports it. Babel and TypeScript error when they
            // encounter this.
            JSXElementChild::JSXSpreadChild(_) => {}
          }
        }
        _ => {
          todo!();
        }
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
      r#"import { jsxssr as _jsxssr, jsxattr as _jsxattr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div>Hello!</div>"
];
const $$_tpl_2 = [
  "<div>Hello ",
  "!</div>"
];
const $$_tpl_3 = [
  '<button class="btn" ',
  ">Hello ",
  "!</button>"
];
const a = _jsxssr($$_tpl_1, null);
const b = _jsxssr($$_tpl_2, name);
const c = _jsxssr($$_tpl_3, _jsxattr("onClick", onClick), name);"#,
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
  "<br>"
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
  '<input type="checkbox" checked>'
];
const a = _jsxssr($$_tpl_1, null);"#,
    );

    test_transform(
      JsxString::default(),
      r#"const a = <input type="checkbox" checked={false} />;"#,
      r#"import { jsxssr as _jsxssr, jsxattr as _jsxattr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<input type="checkbox" ',
  ">"
];
const a = _jsxssr($$_tpl_1, _jsxattr("checked", false));"#,
    );
  }

  #[test]
  fn dynamic_attr_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div class="foo" bar={2}></div>;"#,
      r#"import { jsxssr as _jsxssr, jsxattr as _jsxattr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<div class="foo" ',
  "></div>"
];
const a = _jsxssr($$_tpl_1, _jsxattr("bar", 2));"#,
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
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx("div", {
  foo: "1",
  ...props,
  bar: "2",
  children: "foo"
});"#,
    );
  }

  #[test]
  fn dangerously_html_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div dangerouslySetInnerHTML={{__html: "foo"}}>foo</div>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx("div", {
  dangerouslySetInnerHTML: {
    __html: "foo"
  },
  children: "foo"
});"#,
    );
  }

  #[test]
  fn key_attr_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div key="foo">foo</div>;"#,
      r#"import { jsxssr as _jsxssr, jsxattr as _jsxattr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div ",
  ">foo</div>"
];
const a = _jsxssr($$_tpl_1, _jsxattr("key", "foo"));"#,
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
  fn child_expr_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <p>{2 + 2}</p>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<p>",
  "</p>"
];
const a = _jsxssr($$_tpl_1, 2 + 2);"#,
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

  #[test]
  fn fragment_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <>foo</>;"#,
      r#"const a = "foo";"#,
    );
  }

  #[ignore]
  #[test]
  fn fragment_mulitple_children_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <>foo<div /><Foo /></>;"#,
      r#"const a = "foo";"#,
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
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx("div", {
  ...props
});"#,
    );
  }

  #[test]
  fn prop_spread_with_children_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div {...props}>hello</div>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx("div", {
  ...props,
  children: "hello"
});"#,
    );
  }

  #[test]
  fn prop_spread_with_other_attrs_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <div foo="1" {...props} bar="2">hello</div>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx("div", {
  foo: "1",
  ...props,
  bar: "2",
  children: "hello"
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
  fn component_outer_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <Foo />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, null);"#,
    );
  }

  #[test]
  fn component_with_props_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <Foo required foo="1" bar={2} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  required: true,
  foo: "1",
  bar: 2
});"#,
    );
  }

  #[test]
  fn component_with_spread_props_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <Foo {...props} foo="1" />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  ...props,
  foo: "1"
});"#,
    );
  }

  #[test]
  fn component_with_children_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <Foo>bar</Foo>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  children: "bar"
});"#,
    );
  }

  #[test]
  fn component_with_children_jsx_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <Foo><span>hello</span></Foo>;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<span>hello</span>"
];
const a = _jsx(Foo, {
  children: _jsxssr($$_tpl_1, null)
});"#,
    );
  }

  #[test]
  fn component_with_multiple_children_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <Foo><span>hello</span>foo<Bar />asdf</Foo>;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<span>hello</span>"
];
const a = _jsx(Foo, {
  children: [
    _jsxssr($$_tpl_1, null),
    "foo",
    _jsx(Bar, null),
    "asdf"
  ]
});"#,
    );
  }

  #[test]
  fn component_with_multiple_children_2_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <Foo><span>hello</span>foo<Bar><p>asdf</p></Bar></Foo>;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<span>hello</span>"
];
const $$_tpl_2 = [
  "<p>asdf</p>"
];
const a = _jsx(Foo, {
  children: [
    _jsxssr($$_tpl_1, null),
    "foo",
    _jsx(Bar, {
      children: _jsxssr($$_tpl_2, null)
    })
  ]
});"#,
    );
  }

  #[test]
  fn component_child_expr_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <Foo>{2 + 2}</Foo>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  children: 2 + 2
});"#,
    );
  }

  #[test]
  fn component_with_jsx_attr() {
    test_transform(
      JsxString::default(),
      r#"const a = <Foo bar={<div>hello</div>} />;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div>hello</div>"
];
const a = _jsx(Foo, {
  bar: _jsxssr($$_tpl_1, null)
});"#,
    );

    test_transform(
      JsxString::default(),
      r#"const a = <Foo bar={<Bar>hello</Bar>} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  bar: _jsx(Bar, {
    children: "hello"
  })
});"#,
    );
  }

  #[ignore]
  #[test]
  fn component_with_jsx_frag_attr() {
    test_transform(
      JsxString::default(),
      r#"const a = <Foo bar={<>foo</>} />;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "foo"
];
const a = _jsx(Foo, {
  bar: _jsxssr($$_tpl_1, null)
}));"#,
    );
  }

  #[test]
  fn component_with_jsx_member_test() {
    test_transform(
      JsxString::default(),
      r#"const a = <ctx.Provider value={null} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(ctx.Provider, {
  value: null
});"#,
    );
  }

  // TODO: HTMLEscape attribute names + text children
  // TODO: Fresh specific: <Head> opt out? Or maybe move Fresh users to a
  //       different pattern

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
