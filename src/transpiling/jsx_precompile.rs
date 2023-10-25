// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_utils::prepend_stmt;
use swc_ecma_utils::quote_ident;
use swc_ecma_visit::noop_visit_mut_type;
use swc_ecma_visit::VisitMut;
use swc_ecma_visit::VisitMutWith;

pub struct JsxPrecompile {
  // Specify whether to use the jsx dev runtime or not
  development: bool,
  // The import path to import the jsx runtime from. Will be
  // `<import_source>/jsx-runtime`.
  import_source: String,

  // Internal state
  next_index: usize,
  templates: Vec<(usize, Vec<String>)>,
  // Track if we need to import `jsx` or `jsxDEV` and which identifier
  // to use if we do.
  import_jsx: Option<Ident>,
  // Track if we need to import `jsxssr` and which identifier
  // to use if we do.
  import_jsx_ssr: Option<Ident>,
  // Track if we need to import `jsxattr` and which identifier
  // to use if we do.
  import_jsx_attr: Option<Ident>,
}

impl Default for JsxPrecompile {
  fn default() -> Self {
    Self {
      next_index: 0,
      templates: vec![],
      development: false,
      import_source: "react".to_string(),
      import_jsx: None,
      import_jsx_ssr: None,
      import_jsx_attr: None,
    }
  }
}

impl JsxPrecompile {
  pub fn new(import_source: String, development: bool) -> Self {
    Self {
      import_source,
      development,
      ..JsxPrecompile::default()
    }
  }
}

fn create_tpl_binding_name(index: usize) -> String {
  format!("$$_tpl_{index}")
}

/// Normalize HTML attribute name casing. Depending on which part of
/// the HTML or SVG spec you look at the casings differ. Some attributes
/// are camelCased, other's kebab cased and some lowercased. When
/// developers write JSX they commonly use camelCase only, regardless
/// of the actual attribute name. The spec doesn't care about case
/// sensitivity, but we need to account for the kebab case ones and
/// developers expect attributes in an HTML document to be lowercase.
/// Custom Elements complicate this further as we cannot make any
/// assumptions if the camelCased JSX attribute should be transformed
/// to kebab-case or not. To make matters even more complex, event
/// handlers passed to JSX usually start with `on*` like `onClick`,
/// but this makes them very hard to differentiate from custom element
/// properties when they pick something like `online=""` for example.
fn normalize_dom_attr_name(name: &str) -> String {
  match name {
    // React specific
    "htmlFor" => "for".to_string(),
    "className" => "class".to_string(),
    "dangerouslySetInnerHTML" => name.to_string(),

    "panose1" => "panose-1".to_string(),
    "xlinkActuate" => "xlink:actuate".to_string(),
    "xlinkArcrole" => "xlink:arcrole".to_string(),

    // xlink:href was removed from SVG and isn't needed
    "xlinkHref" => "href".to_string(),
    "xlink:href" => "href".to_string(),

    "xlinkRole" => "xlink:role".to_string(),
    "xlinkShow" => "xlink:show".to_string(),
    "xlinkTitle" => "xlink:title".to_string(),
    "xlinkType" => "xlink:type".to_string(),
    "xmlBase" => "xml:base".to_string(),
    "xmlLang" => "xml:lang".to_string(),
    "xmlSpace" => "xml:space".to_string(),

    // Attributes that are kebab-cased
    "acceptCharset"
    | "alignmentBaseline"
    | "allowReorder"
    | "arabicForm"
    | "baselineShift"
    | "capHeight"
    | "clipPath"
    | "clipRule"
    | "colorInterpolation"
    | "colorInterpolationFilters"
    | "colorProfile"
    | "colorRendering"
    | "contentScriptType"
    | "contentStyleType"
    | "dominantBaseline"
    | "enableBackground"
    | "fillOpacity"
    | "fillRule"
    | "floodColor"
    | "floodOpacity"
    | "fontFamily"
    | "fontSize"
    | "fontSizeAdjust"
    | "fontStretch"
    | "fontStyle"
    | "fontVariant"
    | "fontWeight"
    | "glyphName"
    | "glyphOrientationHorizontal"
    | "glyphOrientationVertical"
    | "horizAdvX"
    | "horizOriginX"
    | "httpEquiv"
    | "imageRendering"
    | "letterSpacing"
    | "lightingColor"
    | "markerEnd"
    | "markerMid"
    | "markerStart"
    | "overlinePosition"
    | "overlineThickness"
    | "paintOrder"
    | "pointerEvents"
    | "renderingIntent"
    | "repeatCount"
    | "repeatDur"
    | "shapeRendering"
    | "stopColor"
    | "stopOpacity"
    | "strikethroughPosition"
    | "strikethroughThickness"
    | "strokeDasharray"
    | "strokeDashoffset"
    | "strokeLinecap"
    | "strokeLinejoin"
    | "strokeMiterlimit"
    | "strokeOpacity"
    | "strokeWidth"
    | "textAnchor"
    | "textDecoration"
    | "underlinePosition"
    | "underlineThickness"
    | "unicodeBidi"
    | "unicodeRange"
    | "unitsPerEm"
    | "vAlphabetic"
    | "vectorEffect"
    | "vertAdvY"
    | "vertOriginX"
    | "vertOriginY"
    | "vHanging"
    | "vMathematical"
    | "wordSpacing"
    | "writingMode"
    | "xHeight" => name
      .chars()
      .map(|ch| match ch {
        'A'..='Z' => format!("-{}", ch.to_lowercase()),
        _ => ch.to_string(),
      })
      .collect(),
    _ => {
      // Devs expect attributes in the HTML document to be lowercased.
      name.to_lowercase()
    }
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

fn get_attr_name(jsx_attr: &JSXAttr, is_component: bool) -> String {
  match &jsx_attr.name {
    // Case: <button class="btn">
    JSXAttrName::Ident(ident) => {
      if is_component {
        ident.sym.to_string()
      } else {
        normalize_dom_attr_name(ident.sym.as_ref())
      }
    }
    // Case: <a xlink:href="#">...</a>
    JSXAttrName::JSXNamespacedName(namespace_name) => {
      let ns = namespace_name.ns.sym.to_string();
      let name = namespace_name.name.sym.to_string();
      let combined = format!("{}:{}", ns, name);
      normalize_dom_attr_name(&combined)
    }
  }
}

fn normalize_lit_str(lit: &Lit) -> Lit {
  match lit {
    Lit::Str(lit_str) => {
      let value: &str = &lit_str.value;
      let mut replaced = "".to_string();

      for (i, line) in value.lines().enumerate() {
        if i > 0 {
          replaced.push(' ');
        }
        replaced.push_str(line.trim_start());
      }

      Lit::Str(Str {
        span: lit_str.span,
        value: replaced.into(),
        raw: None,
      })
    }
    _ => lit.clone(),
  }
}

/// Convert a JSXMemberExpr to MemberExpr. We offload this to a
/// function because conversion is recursive.
fn jsx_member_expr_to_normal(jsx_member_expr: &JSXMemberExpr) -> MemberExpr {
  MemberExpr {
    span: DUMMY_SP,
    obj: match jsx_member_expr.obj.clone() {
      JSXObject::Ident(ident) => Box::new(Expr::Ident(ident.clone())),
      JSXObject::JSXMemberExpr(member_expr) => {
        Box::new(Expr::Member(jsx_member_expr_to_normal(&member_expr)))
      }
    },
    prop: MemberProp::Ident(jsx_member_expr.prop.clone()),
  }
}

/// Edge case: If the JSX opening element contains a spread attribute
/// then it's not safe to serialize it. This is because existing code
/// relies on the object spread semantics where it can overwrite
/// existing properties or values passed from spread can be
/// overwritten by setting properties after the spread. What's more
/// is that the spread object could contain `props.children` which
/// we would miss.
/// Moreover, components cannot be safely serialized because there
/// is no specified output format.
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

      !opening.attrs.iter().any(|attr| match attr {
        JSXAttrOrSpread::SpreadElement(_) => true,
        JSXAttrOrSpread::JSXAttr(attr) => {
          let name = get_attr_name(attr, false);
          matches!(name.as_str(), "dangerouslySetInnerHTML")
        }
      })
    }
    _ => false,
  }
}

fn string_lit_expr(str: String) -> Expr {
  Expr::Lit(Lit::Str(Str {
    span: DUMMY_SP,
    value: str.into(),
    raw: None,
  }))
}

fn escape_html(str: &str) -> String {
  str
    .replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('>', "&gt;")
    .replace('\'', "&#39;")
    .replace('"', "&quot;")
}

impl JsxPrecompile {
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

  fn serialize_jsx_children_to_expr(
    &mut self,
    children: &Vec<JSXElementChild>,
  ) -> Option<Expr> {
    // Add children as a "children" prop.
    match children.len() {
      0 => None,
      1 => {
        let child = &children[0];
        match child {
          JSXElementChild::JSXText(jsx_text) => {
            Some(string_lit_expr(jsx_text.value.to_string()))
          }
          JSXElementChild::JSXExprContainer(jsx_expr_container) => {
            match &jsx_expr_container.expr {
              // Empty JSX expressions can be ignored as they have no content
              // Case: <div>{}</div>
              // Case: <div>{/* fooo */}</div>
              JSXExpr::JSXEmptyExpr(_) => None,
              JSXExpr::Expr(expr) => Some(*expr.clone()),
            }
          }
          // Case: <div><span /></div>
          JSXElementChild::JSXElement(jsx_element) => {
            Some(self.serialize_jsx(jsx_element))
          }
          // Case: <div><></></div>
          JSXElementChild::JSXFragment(jsx_frag) => {
            self.serialize_jsx_children_to_expr(&jsx_frag.children)
          }
          // Invalid, was part of an earlier JSX iteration, but no
          // transform supports it. Babel and TypeScript error when they
          // encounter this.
          JSXElementChild::JSXSpreadChild(_) => None,
        }
      }
      _ => {
        let mut elems: Vec<Option<ExprOrSpread>> = vec![];
        for child in children.iter() {
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
                // Case: <div>{/* some comment */}</div>
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
            JSXElementChild::JSXFragment(jsx_frag) => {
              if let Some(child_expr) =
                self.serialize_jsx_children_to_expr(&jsx_frag.children)
              {
                match child_expr {
                  Expr::Array(array_lit) => {
                    for item in array_lit.elems.iter() {
                      elems.push(item.clone());
                    }
                  }
                  _ => {
                    elems.push(Some(ExprOrSpread {
                      spread: None,
                      expr: Box::new(child_expr.clone()),
                    }));
                  }
                };
              }
            }
            // Invalid, was part of an earlier JSX iteration, but no
            // transform supports it. Babel and TypeScript error when they
            // encounter this.
            JSXElementChild::JSXSpreadChild(_) => {}
          }
        }

        Some(Expr::Array(ArrayLit {
          span: DUMMY_SP,
          elems,
        }))
      }
    }
  }

  /// Serializes a JSXElement to a standard jsx expression. We use
  /// this function when we cannot safely serialize a JSXElement.
  ///
  /// Case: <div {...props} />
  /// Case: <Foo bar="1" />
  fn serialize_jsx_to_call_expr(&mut self, el: &JSXElement) -> CallExpr {
    let mut is_component = false;
    let name_expr = match &el.opening.name {
      // Case: <div />
      // Case: <Foo />
      JSXElementName::Ident(ident) => {
        let name = &ident.sym;
        // Component identifiers start with an uppercase character
        // Case: <Foo bar="123" />
        if name.chars().next().unwrap().is_ascii_uppercase() {
          is_component = true;
          Expr::Ident(ident.clone())
        } else {
          string_lit_expr(name.to_string())
        }
      }
      // Case: <ctx.Provider />
      JSXElementName::JSXMemberExpr(jsx_member_expr) => {
        Expr::Member(jsx_member_expr_to_normal(jsx_member_expr))
      }
      JSXElementName::JSXNamespacedName(namespace_name) => {
        let ns = namespace_name.ns.sym.to_string();
        let name = namespace_name.name.sym.to_string();
        let combined = format!("{}:{}", ns, name);
        string_lit_expr(combined)
      }
    };

    let mut args: Vec<ExprOrSpread> = vec![];
    args.push(ExprOrSpread {
      spread: None,
      expr: Box::new(name_expr),
    });

    let mut key_value: Option<Expr> = None;

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
            let attr_name = get_attr_name(jsx_attr, is_component);
            let prop_name = PropName::Ident(quote_ident!(attr_name.clone()));

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

            if attr_name == "key" {
              key_value = match attr_value {
                JSXAttrValue::Lit(lit) => {
                  let normalized_lit = normalize_lit_str(lit);
                  Some(Expr::Lit(normalized_lit))
                }
                JSXAttrValue::JSXExprContainer(jsx_expr_container) => {
                  match &jsx_expr_container.expr {
                    // This is treated as a syntax error in attributes
                    JSXExpr::JSXEmptyExpr(_) => None,
                    JSXExpr::Expr(expr) => Some(*expr.clone()),
                  }
                }
                // There is no valid way to construct these
                _ => None,
              };

              continue;
            }

            // Case: <Foo class="btn">
            // Case: <Foo class={"foo"}>
            // Case: <Foo class={2}>
            // Case: <Foo class={true}>
            // Case: <Foo class={null}>
            match attr_value {
              JSXAttrValue::Lit(lit) => {
                let normalized_lit = normalize_lit_str(lit);

                props.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
                  KeyValueProp {
                    key: prop_name,
                    value: Box::new(Expr::Lit(normalized_lit)),
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
      let child_expr = self.serialize_jsx_children_to_expr(&el.children);

      if let Some(expr) = child_expr {
        let children_name = PropName::Ident(quote_ident!("children"));
        props.push(PropOrSpread::Prop(Box::new(Prop::KeyValue(
          KeyValueProp {
            key: children_name.clone(),
            value: Box::new(expr),
          },
        ))));
      }

      if props.is_empty() {
        args.push(null_arg())
      } else {
        let obj_expr = Box::new(Expr::Object(ObjectLit {
          span: DUMMY_SP,
          props,
        }));
        args.push(ExprOrSpread {
          spread: None,
          expr: obj_expr,
        });
      }
    }

    if let Some(key_expr) = key_value {
      args.push(ExprOrSpread {
        spread: None,
        expr: Box::new(key_expr),
      });
    }

    CallExpr {
      span: DUMMY_SP,
      callee: Callee::Expr(Box::new(Expr::Ident(self.get_jsx_identifier()))),
      args,
      type_args: None,
    }
  }

  fn convert_to_jsx_attr_call(&mut self, name: String, expr: Expr) -> CallExpr {
    let args = vec![
      ExprOrSpread {
        spread: None,
        expr: Box::new(string_lit_expr(name)),
      },
      ExprOrSpread {
        spread: None,
        expr: Box::new(expr),
      },
    ];

    CallExpr {
      span: DUMMY_SP,
      callee: Callee::Expr(Box::new(Expr::Ident(
        self.get_jsx_attr_identifier(),
      ))),
      args,
      type_args: None,
    }
  }

  fn serialize_jsx_children_to_string(
    &mut self,
    children: &[JSXElementChild],
    strings: &mut Vec<String>,
    dynamic_exprs: &mut Vec<Expr>,
  ) {
    for child in children.iter() {
      match child {
        // Case: <div>foo</div>
        JSXElementChild::JSXText(jsx_text) => {
          let escaped_text = escape_html(jsx_text.value.as_ref());
          strings.last_mut().unwrap().push_str(escaped_text.as_str());
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
            jsx_element,
            strings,
            dynamic_exprs,
          ),
        // Case: <div><></></div>
        JSXElementChild::JSXFragment(jsx_frag) => self
          .serialize_jsx_children_to_string(
            &jsx_frag.children,
            strings,
            dynamic_exprs,
          ),
        // Invalid, was part of an earlier JSX iteration, but no
        // transform supports it. Babel and TypeScript error when they
        // encounter this.
        JSXElementChild::JSXSpreadChild(_) => {}
      }
    }
  }

  fn serialize_jsx_element_to_string_vec(
    &mut self,
    el: &JSXElement,
    strings: &mut Vec<String>,
    dynamic_exprs: &mut Vec<Expr>,
  ) {
    // Case: <div {...props} />
    // Case: <div class="foo" {...{ class: "bar"}} />
    // Case: <div {...{ class: "foo"}} class="bar"}>foo</div>
    // Case: <Foo />
    if !is_serializable(&el.opening) {
      let expr = Expr::Call(self.serialize_jsx_to_call_expr(el));
      strings.push("".to_string());
      dynamic_exprs.push(expr);

      return;
    } else if strings.is_empty() {
      strings.push("".to_string());
    }

    let name: &str = match &el.opening.name {
      // Case: <div />
      JSXElementName::Ident(ident) => &ident.sym,
      _ => {
        unreachable!("serialize_jsx_element_to_string_vec(JSXNamespacedName)")
      }
    };

    strings.last_mut().unwrap().push('<');

    let escaped_name = escape_html(name);
    strings.last_mut().unwrap().push_str(escaped_name.as_str());

    for attr in &el.opening.attrs {
      // Case: <button class="btn">
      match attr {
        JSXAttrOrSpread::JSXAttr(jsx_attr) => {
          let attr_name = get_attr_name(jsx_attr, false);

          // Case: <input required />
          let Some(attr_value) = &jsx_attr.value else {
            strings.last_mut().unwrap().push(' ');
            let escaped_attr_name = escape_html(&attr_name);
            strings
              .last_mut()
              .unwrap()
              .push_str(escaped_attr_name.as_str());
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
                // Edge Case: Both "key" and "ref" attributes are
                // special attributes in most frameworks. Some
                // frameworks may want to serialize it, other's don't.
                // To support both use cases we'll always pass them to
                // `jsxattr()` so that frameowrks can decide for
                // themselves what to do with it.
                // Case: <div key="123" />
                // Case: <div ref="123" />
                if attr_name == "key" || attr_name == "ref" {
                  strings.last_mut().unwrap().push(' ');
                  strings.push("".to_string());
                  let expr = self.convert_to_jsx_attr_call(
                    attr_name,
                    string_lit_expr(string_lit.value.to_string()),
                  );
                  dynamic_exprs.push(Expr::Call(expr));
                  continue;
                }

                let serialized_attr = format!(
                  " {}=\"{}\"",
                  escape_html(&attr_name).as_str(),
                  escape_html(string_lit.value.as_ref()).as_str()
                );

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
              strings.last_mut().unwrap().push(' ');
              strings.push("".to_string());
              match &jsx_expr_container.expr {
                // This is treated as a syntax error in attributes
                JSXExpr::JSXEmptyExpr(_) => {}
                JSXExpr::Expr(expr) => {
                  let call_expr = self.convert_to_jsx_attr_call(
                    attr_name.to_string(),
                    *expr.clone(),
                  );
                  dynamic_exprs.push(Expr::Call(call_expr));
                }
              }
            }
            // These makes no sense on as attribute on HTML elements
            // so we ignore them.
            JSXAttrValue::JSXElement(_) => {}
            JSXAttrValue::JSXFragment(_) => {}
          }
        }
        // This case is already handled earlier
        // Case: <div {...props} />
        JSXAttrOrSpread::SpreadElement(_) => {}
      };
    }

    strings.last_mut().unwrap().push('>');

    // There are no self closing elements in HTML, only void elements.
    // Void elements are a fixed list of elements that cannot have
    // child nodes.
    // See https://developer.mozilla.org/en-US/docs/Glossary/Void_element
    // Case: <br /> -> <br>
    // Case: <meta /> -> <meta>
    if is_void_element(name) {
      // Since self closing tags don't exist in HTML we don't need to
      // add the "/" character. If the "/" character is present,
      // browsers will ignore it anyway.
      return;
    }

    self.serialize_jsx_children_to_string(&el.children, strings, dynamic_exprs);

    let closing_tag = format!("</{}>", name);
    strings.last_mut().unwrap().push_str(closing_tag.as_str());
  }

  fn gen_template(
    &mut self,
    template_index: usize,
    static_strs: Vec<String>,
    dynamic_exprs: Vec<Expr>,
  ) -> Expr {
    let name = create_tpl_binding_name(template_index);
    self.templates.push((template_index, static_strs));

    let mut args: Vec<ExprOrSpread> =
      Vec::with_capacity(1 + dynamic_exprs.len());
    args.push(ExprOrSpread {
      spread: None,
      expr: Box::new(Expr::Ident(Ident::new(name.into(), DUMMY_SP))),
    });
    for dynamic_expr in dynamic_exprs.into_iter() {
      args.push(ExprOrSpread {
        spread: None,
        expr: Box::new(dynamic_expr),
      });
    }

    // Case: _jsxssr($$_tpl_1);
    let jsx_ident = self.get_jsx_ssr_identifier();

    Expr::Call(CallExpr {
      span: DUMMY_SP,
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
      let index = self.next_index;
      let mut static_strs: Vec<String> = vec![];
      let mut dynamic_exprs: Vec<Expr> = vec![];
      self.serialize_jsx_element_to_string_vec(
        el,
        &mut static_strs,
        &mut dynamic_exprs,
      );

      self.gen_template(index, static_strs, dynamic_exprs)
    } else {
      // Case: <div {...props} />
      Expr::Call(self.serialize_jsx_to_call_expr(el))
    }
  }

  fn inject_runtime(&mut self, stmts: &mut Vec<ModuleItem>) {
    let mut imports: Vec<(Ident, Ident)> = vec![];

    if let Some(jsx_ident) = &self.import_jsx {
      let jsx_imported = if self.development { "jsxDEV" } else { "jsx" };
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

impl VisitMut for JsxPrecompile {
  noop_visit_mut_type!();

  fn visit_mut_module(&mut self, module: &mut Module) {
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
      *expr = self.serialize_jsx(el);
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
          let child = &frag.children[0];
          match child {
            JSXElementChild::JSXText(jsx_text) => {
              *expr = string_lit_expr(jsx_text.value.to_string())
            }
            JSXElementChild::JSXExprContainer(jsx_expr_container) => {
              match &jsx_expr_container.expr {
                // Empty JSX expressions can be ignored as they have no content
                // Case: <>{}</>
                // Case: <>{/* some comment */}</>
                JSXExpr::JSXEmptyExpr(_) => {}
                JSXExpr::Expr(jsx_expr) => *expr = *jsx_expr.clone(),
              }
            }
            // Case: <><span /></>
            JSXElementChild::JSXElement(jsx_element) => {
              *expr = self.serialize_jsx(jsx_element);
            }
            // Case: <><></></>
            JSXElementChild::JSXFragment(jsx_frag) => {
              let serialized =
                self.serialize_jsx_children_to_expr(&jsx_frag.children);
              if let Some(serialized_expr) = serialized {
                *expr = serialized_expr
              }
            }
            // Invalid, was part of an earlier JSX iteration, but no
            // transform supports it. Babel and TypeScript error when
            // they encounter this.
            JSXElementChild::JSXSpreadChild(_) => {}
          }
        }
        _ => {
          self.next_index += 1;
          let index = self.next_index;
          let mut strings: Vec<String> = vec![];
          let mut dynamic_exprs: Vec<Expr> = vec![];

          strings.push("".to_string());

          self.serialize_jsx_children_to_string(
            &frag.children,
            &mut strings,
            &mut dynamic_exprs,
          );
          *expr = self.gen_template(index, strings, dynamic_exprs)
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
  use swc_ecma_visit::as_folder;

  use super::*;

  #[test]
  fn basic_test() {
    test_transform(
      JsxPrecompile::default(),
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
const a = _jsxssr($$_tpl_1);
const b = _jsxssr($$_tpl_2, name);
const c = _jsxssr($$_tpl_3, _jsxattr("onclick", onClick), name);"#,
    );
  }

  #[test]
  fn convert_self_closing_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <div />;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div></div>"
];
const a = _jsxssr($$_tpl_1);"#,
    );

    // Void elements
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <br></br>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<br>"
];
const a = _jsxssr($$_tpl_1);"#,
    );
  }

  #[test]
  fn normalize_attr_name_test() {
    let mappings: Vec<(String, String)> = vec![
      ("htmlFor".to_string(), "for".to_string()),
      ("className".to_string(), "class".to_string()),
      ("xlinkRole".to_string(), "xlink:role".to_string()),
      ("acceptCharset".to_string(), "accept-charset".to_string()),
      ("onFoo".to_string(), "onfoo".to_string()),
    ];

    for mapping in mappings.iter() {
      test_transform(
        JsxPrecompile::default(),
        format!("const a = <label {}=\"foo\">label</label>", &mapping.0)
          .as_str(),
        format!(
          "{}\nconst $$_tpl_1 = [\n  '<label {}=\"foo\">label</label>'\n];\nconst a = _jsxssr($$_tpl_1);",
          "import { jsxssr as _jsxssr } from \"react/jsx-runtime\";",
          &mapping.1
        )
        .as_str(),
      );

      // should still be normalized if HTML element cannot
      // be serialized
      test_transform(
        JsxPrecompile::default(),
        format!("const a = <label {}=\"foo\" {{...foo}} />", &mapping.0)
          .as_str(),
        format!(
          "{}\nconst a = _jsx(\"label\", {{\n  {}: \"foo\",\n  ...foo\n}});",
          "import { jsx as _jsx } from \"react/jsx-runtime\";", &mapping.1
        )
        .as_str(),
      );
    }

    // Component props should never be normalized
    for mapping in mappings.iter() {
      test_transform(
        JsxPrecompile::default(),
        format!("const a = <Foo {}=\"foo\">foo</Foo>", &mapping.0).as_str(),
        format!(
          "{}\nconst a = _jsx(Foo, {{\n  {}: \"foo\",\n  children: \"foo\"\n}});",
          "import { jsx as _jsx } from \"react/jsx-runtime\";",
          &mapping.0
        )
        .as_str(),
      );
    }
  }

  #[test]
  fn boolean_attr_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <input type="checkbox" checked />;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<input type="checkbox" checked>'
];
const a = _jsxssr($$_tpl_1);"#,
    );

    test_transform(
      JsxPrecompile::default(),
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
      JsxPrecompile::default(),
      r#"const a = <div class="foo" bar={2}></div>;"#,
      r#"import { jsxssr as _jsxssr, jsxattr as _jsxattr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<div class="foo" ',
  "></div>"
];
const a = _jsxssr($$_tpl_1, _jsxattr("bar", 2));"#,
    );
  }

  #[test]
  fn namespace_attr_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <a xlink:href="foo">foo</a>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<a href="foo">foo</a>'
];
const a = _jsxssr($$_tpl_1);"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <a foo:bar="foo">foo</a>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<a foo:bar="foo">foo</a>'
];
const a = _jsxssr($$_tpl_1);"#,
    );
  }

  #[test]
  fn mixed_static_dynamic_props_test() {
    test_transform(
      JsxPrecompile::default(),
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
      JsxPrecompile::default(),
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
      JsxPrecompile::default(),
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
  fn key_attr_comp_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo key="foo" />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, null, "foo");"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo key={2} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, null, 2);"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo key={2}>foo</Foo>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  children: "foo"
}, 2);"#,
    );
  }

  #[test]
  fn ref_attr_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <div ref="foo">foo</div>;"#,
      r#"import { jsxssr as _jsxssr, jsxattr as _jsxattr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div ",
  ">foo</div>"
];
const a = _jsxssr($$_tpl_1, _jsxattr("ref", "foo"));"#,
    );
  }

  #[test]
  fn escape_attr_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <div class="a&<>'">foo</div>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<div class="a&amp;&lt;&gt;&#39;">foo</div>'
];
const a = _jsxssr($$_tpl_1);"#,
    );
  }

  #[test]
  fn escape_text_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <div>"a&>'</div>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div>&quot;a&amp;&gt;&#39;</div>"
];
const a = _jsxssr($$_tpl_1);"#,
    );
  }

  #[test]
  fn namespace_name_test() {
    // Note: This isn't really supported anywhere, but I guess why not
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <a:b>foo</a:b>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx("a:b", {
  children: "foo"
});"#,
    );
  }

  #[test]
  fn empty_jsx_child_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <p>{}</p>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<p></p>"
];
const a = _jsxssr($$_tpl_1);"#,
    );
  }

  #[test]
  fn child_expr_test() {
    test_transform(
      JsxPrecompile::default(),
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
      JsxPrecompile::default(),
      r#"const a = <></>;"#,
      r#"const a = null;"#,
    );
  }

  #[test]
  fn fragment_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <>foo</>;"#,
      r#"const a = "foo";"#,
    );
  }

  #[test]
  fn fragment_nested_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <><>foo</></>;"#,
      r#"const a = "foo";"#,
    );
  }

  #[test]
  fn fragment_mulitple_children_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <>foo<div /><Foo /></>;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "foo<div></div>",
  ""
];
const a = _jsxssr($$_tpl_1, _jsx(Foo, null));"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo><div /><><>foo</><span /></></Foo>;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div></div>"
];
const $$_tpl_2 = [
  "<span></span>"
];
const a = _jsx(Foo, {
  children: [
    _jsxssr($$_tpl_1),
    "foo",
    _jsxssr($$_tpl_2)
  ]
});"#,
    );
  }

  #[test]
  fn nested_elements_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <div>foo<p>bar</p></div>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div>foo<p>bar</p></div>"
];
const a = _jsxssr($$_tpl_1);"#,
    );
  }

  #[test]
  fn prop_spread_without_children_test() {
    test_transform(
      JsxPrecompile::default(),
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
      JsxPrecompile::default(),
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
      JsxPrecompile::default(),
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
      JsxPrecompile::default(),
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
      JsxPrecompile::default(),
      r#"const a = <Foo />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, null);"#,
    );
  }

  #[test]
  fn component_with_props_test() {
    test_transform(
      JsxPrecompile::default(),
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
      JsxPrecompile::default(),
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
      JsxPrecompile::default(),
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
      JsxPrecompile::default(),
      r#"const a = <Foo><span>hello</span></Foo>;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<span>hello</span>"
];
const a = _jsx(Foo, {
  children: _jsxssr($$_tpl_1)
});"#,
    );
  }

  #[test]
  fn component_with_multiple_children_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo><span>hello</span>foo<Bar />asdf</Foo>;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<span>hello</span>"
];
const a = _jsx(Foo, {
  children: [
    _jsxssr($$_tpl_1),
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
      JsxPrecompile::default(),
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
    _jsxssr($$_tpl_1),
    "foo",
    _jsx(Bar, {
      children: _jsxssr($$_tpl_2)
    })
  ]
});"#,
    );
  }

  #[test]
  fn component_child_expr_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo>{2 + 2}</Foo>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  children: 2 + 2
});"#,
    );
  }

  #[test]
  fn component_with_jsx_attr_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo bar={<div>hello</div>} />;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div>hello</div>"
];
const a = _jsx(Foo, {
  bar: _jsxssr($$_tpl_1)
});"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo bar={<Bar>hello</Bar>} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  bar: _jsx(Bar, {
    children: "hello"
  })
});"#,
    );
  }

  #[test]
  fn component_with_jsx_frag_attr_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo bar={<>foo</>} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  bar: "foo"
});"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo bar={<>foo<Foo/>bar</>} />;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "foo",
  "bar"
];
const a = _jsx(Foo, {
  bar: _jsxssr($$_tpl_1, _jsx(Foo, null))
});"#,
    );
  }

  #[test]
  fn component_with_nested_frag_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo><>foo<Bar><></></Bar></></Foo>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  children: [
    "foo",
    _jsx(Bar, null)
  ]
});"#,
    );
  }

  #[test]
  fn component_with_jsx_member_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <ctx.Provider value={null} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(ctx.Provider, {
  value: null
});"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <a.b.c.d value={null} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(a.b.c.d, {
  value: null
});"#,
    );
  }

  #[test]
  fn import_source_option_test() {
    test_transform(
      JsxPrecompile::new("foobar".to_string(), false),
      r#"const a = <div>foo</div>;"#,
      r#"import { jsxssr as _jsxssr } from "foobar/jsx-runtime";
const $$_tpl_1 = [
  "<div>foo</div>"
];
const a = _jsxssr($$_tpl_1);"#,
    );
  }

  #[test]
  fn development_option_test() {
    test_transform(
      JsxPrecompile::new("react".to_string(), true),
      r#"const a = <Foo>foo</Foo>;"#,
      r#"import { jsxDEV as _jsxDEV } from "react/jsx-dev-runtime";
const a = _jsxDEV(Foo, {
  children: "foo"
});"#,
    );
  }

  #[test]
  fn template_index_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"<div><Foo><span /></Foo></div>;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_2 = [
  "<span></span>"
];
const $$_tpl_1 = [
  "<div>",
  "</div>"
];
_jsxssr($$_tpl_1, _jsx(Foo, {
  children: _jsxssr($$_tpl_2)
}));"#,
    );
  }

  #[test]
  fn multi_jsx_string_line_to_jsx_call_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo
key="Register a module with the third party
      registry."
description="Register a module with the third party
      registry."
/>
      "#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  description: "Register a module with the third party registry."
}, "Register a module with the third party registry.");"#,
    );
  }

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
