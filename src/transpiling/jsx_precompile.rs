// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use crate::swc::atoms::Atom;
use crate::swc::parser::lexer::util::CharExt;
use swc_common::DUMMY_SP;
use swc_ecma_ast::*;
use swc_ecma_utils::prepend_stmt;
use swc_ecma_utils::quote_ident;
use swc_ecma_visit::noop_visit_mut_type;
use swc_ecma_visit::VisitMut;
use swc_ecma_visit::VisitMutWith;

pub struct JsxPrecompile {
  // The import path to import the jsx runtime from. Will be
  // `<import_source>/jsx-runtime`.
  import_source: String,

  // Internal state
  next_index: usize,
  templates: Vec<(usize, Vec<String>)>,
  // Track if we need to import `jsx` and which identifier
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
      import_source: "react".to_string(),
      import_jsx: None,
      import_jsx_ssr: None,
      import_jsx_attr: None,
    }
  }
}

impl JsxPrecompile {
  pub fn new(import_source: String) -> Self {
    Self {
      import_source,
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

// See: https://html.spec.whatwg.org/multipage/indices.html#attributes-3
fn is_boolean_attr(name: &str) -> bool {
  matches!(
    name,
    "allowfullscreen"
      | "async"
      | "autofocus"
      | "autoplay"
      | "checked"
      | "controls"
      | "default"
      | "defer"
      | "disabled"
      | "formnovalidate"
      | "inert"
      | "ismap"
      | "itemscope"
      | "loop"
      | "multiple"
      | "muted"
      | "nomodule"
      | "novalidate"
      | "open"
      | "playsinline"
      | "readonly"
      | "required"
      | "reversed"
      | "selected"
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

fn jsx_text_to_str(jsx_text: &JSXText) -> String {
  let mut text = String::new();

  let mut lines = jsx_text.value.lines().enumerate().peekable();
  while let Some((i, line)) = lines.next() {
    let line = if i != 0 {
      line.trim_start_matches(' ')
    } else if lines.peek().is_some() {
      line.trim_end_matches(' ')
    } else {
      line
    };

    if line.is_empty() {
      continue;
    }

    if i > 0 && !text.is_empty() {
      text.push(' ')
    }

    text.push_str(line);
  }

  text
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

fn is_text_valid_identifier(string_value: &str) -> bool {
  if string_value.is_empty() {
    return false;
  }
  for (i, c) in string_value.chars().enumerate() {
    if (i == 0 && !c.is_ident_start()) || !c.is_ident_part() {
      return false;
    }
  }
  true
}

fn string_lit_expr(value: Atom) -> Expr {
  Expr::Lit(Lit::Str(Str {
    span: DUMMY_SP,
    value,
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

fn serialize_attr(attr_name: &str, value: &str) -> String {
  format!(" {}=\"{}\"", escape_html(attr_name), escape_html(value))
}

fn merge_serializable_children(
  children: &[JSXElementChild],
) -> (Vec<JSXElementChild>, usize, usize) {
  // Do a first pass over children to merge sibling text nodes
  // and check if it contains any serializable nodes.
  let mut text_count = 0;
  let mut serializable_count = 0;
  let mut buf = String::new();

  let mut normalized_children: Vec<JSXElementChild> = vec![];
  for child in children.iter() {
    match child {
      JSXElementChild::JSXText(jsx_text) => {
        let text = jsx_text_to_str(jsx_text);
        if text.is_empty() {
          continue;
        }
        text_count += 1;

        buf.push_str(&text);
      }
      JSXElementChild::JSXExprContainer(jsx_expr_container) => {
        match &jsx_expr_container.expr {
          // Empty JSX expressions can be ignored as they have no content
          // Case: <div>{}</div>
          // Case: <div>{/* fooo */}</div>
          JSXExpr::JSXEmptyExpr(_) => {}
          // Case: <div>{"foo"}</div>
          // Case: <div>{2 + 2}</div>
          JSXExpr::Expr(expr) => {
            if let Expr::Lit(lit) = &**expr {
              match lit {
                // Booleans are not rendered because people usually use
                // them for conditional rendering.
                // Case <div>{foo && <span />}</div>
                Lit::Bool(_) => continue,
                // Can be flattened
                Lit::Num(num) => {
                  let text = num.to_string();
                  buf.push_str(&text);
                  continue;
                }
                // Can be flattened
                Lit::Str(str_lit) => {
                  buf.push_str(str_lit.value.as_ref());
                  continue;
                }
                _ => {}
              }
            }

            if !buf.is_empty() {
              let atom = Atom::new(buf);
              normalized_children.push(JSXElementChild::JSXText(JSXText {
                span: DUMMY_SP,
                value: atom.clone(),
                raw: atom,
              }));

              buf = String::new()
            }

            normalized_children.push(child.clone());
          }
        }
      }
      JSXElementChild::JSXElement(jsx_el) => {
        if !buf.is_empty() {
          let atom = Atom::new(buf);
          normalized_children.push(JSXElementChild::JSXText(JSXText {
            span: DUMMY_SP,
            value: atom.clone(),
            raw: atom,
          }));

          buf = String::new()
        }

        if is_serializable(&jsx_el.opening) {
          serializable_count += 1;
        }

        normalized_children.push(JSXElementChild::JSXElement(jsx_el.clone()));
      }
      // Invalid, was part of an earlier JSX iteration, but no
      // transform supports it. Babel and TypeScript error when they
      // encounter this.
      JSXElementChild::JSXSpreadChild(_) => {}
      child => {
        if !buf.is_empty() {
          let atom = Atom::new(buf);
          normalized_children.push(JSXElementChild::JSXText(JSXText {
            span: DUMMY_SP,
            value: atom.clone(),
            raw: atom,
          }));

          buf = String::new()
        }

        normalized_children.push(child.clone());
      }
    }
  }

  if !buf.is_empty() {
    let atom = Atom::new(buf);
    normalized_children.push(JSXElementChild::JSXText(JSXText {
      span: DUMMY_SP,
      value: atom.clone(),
      raw: atom,
    }));
  }

  (normalized_children, text_count, serializable_count)
}

impl JsxPrecompile {
  /// Mark `jsx` as being used and return the identifier.
  fn get_jsx_identifier(&mut self) -> Ident {
    match &self.import_jsx {
      Some(ident) => ident.clone(),
      None => {
        let ident = Ident::new("_jsx".into(), DUMMY_SP);
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
            let text = jsx_text_to_str(jsx_text);
            Some(string_lit_expr(text.into()))
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
        let (normalized_children, text_count, serializable_count) =
          merge_serializable_children(children);

        // Merge sibling children when they can be serialized into one
        // serialized child. If all children are serializable, we'll
        // merge everything into one big jsxssr() call. If everything
        // can be merged into a single string, then we'll go with that.
        // First flattend sibling children if possible
        let mut elems: Vec<Option<ExprOrSpread>> = vec![];

        // Determine whether we should wrap children with a template
        // Case: <Foo>foo<span /></Foo>
        // Case: <Foo>foo<Bar /><span /></Foo>
        // Case: <Foo><span /><Bar /><span /></Foo>
        if serializable_count > 1 || serializable_count == 1 && text_count > 0 {
          self.next_index += 1;
          let index = self.next_index;
          let mut strings: Vec<String> = vec![];
          let mut dynamic_exprs: Vec<Expr> = vec![];
          strings.push("".to_string());

          self.serialize_jsx_children_to_string(
            &normalized_children,
            &mut strings,
            &mut dynamic_exprs,
          );

          let expr = self.gen_template(index, strings, dynamic_exprs);
          elems.push(Some(ExprOrSpread {
            spread: None,
            expr: Box::new(expr.clone()),
          }));
        } else {
          // Here we parse children the normal way

          for child in normalized_children {
            match child {
              // Case: <div>foo</div>
              JSXElementChild::JSXText(jsx_text) => {
                elems.push(Some(ExprOrSpread {
                  spread: None,
                  expr: Box::new(string_lit_expr(jsx_text.value)),
                }));
              }
              // Case: <div>{2 + 2}</div>
              JSXElementChild::JSXExprContainer(jsx_expr_container) => {
                match jsx_expr_container.expr {
                  // Empty JSX expressions can be ignored as they have no content
                  // Case: <div>{}</div>
                  // Case: <div>{/* some comment */}</div>
                  JSXExpr::JSXEmptyExpr(_) => continue,
                  JSXExpr::Expr(expr) => {
                    elems.push(Some(ExprOrSpread { spread: None, expr }));
                  }
                }
              }
              // Case: <div><span /></div>
              JSXElementChild::JSXElement(jsx_el) => {
                let expr = self.serialize_jsx(&jsx_el);
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
                      elems.extend(array_lit.elems);
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
        }

        // Flatten to a single child call when children
        // array only contains one element
        if elems.len() == 1 {
          if let Some(first) = &elems[0] {
            let expr = &*first.expr;
            return Some(expr.clone());
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
          string_lit_expr(name.clone())
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
        string_lit_expr(combined.into())
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
            let prop_name = if !is_text_valid_identifier(&attr_name) {
              PropName::Str(Str {
                span: DUMMY_SP,
                raw: None,
                value: attr_name.clone().into(),
              })
            } else {
              PropName::Ident(quote_ident!(attr_name.clone()))
            };

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

  fn convert_to_jsx_attr_call(&mut self, name: Atom, expr: Expr) -> CallExpr {
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
    let (normalized_children, _text_count, _serializable_count) =
      merge_serializable_children(children);

    for child in normalized_children {
      match child {
        // Case: <div>foo</div>
        JSXElementChild::JSXText(jsx_text) => {
          let escaped_text = escape_html(jsx_text.value.as_ref());
          strings.last_mut().unwrap().push_str(escaped_text.as_str());
        }
        // Case: <div>{2 + 2}</div>
        JSXElementChild::JSXExprContainer(jsx_expr_container) => {
          match jsx_expr_container.expr {
            // Empty JSX expressions can be ignored as they have no content
            // Case: <div>{}</div>
            // Case: <div>{/* fooo */}</div>
            JSXExpr::JSXEmptyExpr(_) => continue,
            // Case: <div>{2 + 2}</div>
            // Case: <div>{foo}</div>
            // Case: <div>{() => null}</div>
            JSXExpr::Expr(expr) => {
              strings.push("".to_string());
              dynamic_exprs.push(*expr);
            }
          }
        }
        // Case: <div><span /></div>
        JSXElementChild::JSXElement(jsx_element) => self
          .serialize_jsx_element_to_string_vec(
            &jsx_element,
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
                    attr_name.into(),
                    string_lit_expr(string_lit.value.clone()),
                  );
                  dynamic_exprs.push(Expr::Call(expr));
                  continue;
                }

                let serialized_attr =
                  serialize_attr(&attr_name, &string_lit.value);

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
              match &jsx_expr_container.expr {
                // This is treated as a syntax error in attributes
                JSXExpr::JSXEmptyExpr(_) => {}
                JSXExpr::Expr(jsx_expr) => {
                  let expr = *jsx_expr.clone();

                  // Serialize numeric literal values
                  // Case: <img width={100} />
                  match &expr {
                    Expr::Lit(lit) => match lit {
                      Lit::Bool(lit_bool) => {
                        if is_boolean_attr(&attr_name) {
                          if !lit_bool.value {
                            continue;
                          }

                          strings.last_mut().unwrap().push(' ');
                          strings.last_mut().unwrap().push_str(&attr_name);
                          continue;
                        }
                      }
                      Lit::Num(num) => {
                        let serialized_attr =
                          serialize_attr(&attr_name, &num.value.to_string());

                        strings
                          .last_mut()
                          .unwrap()
                          .push_str(serialized_attr.as_str());
                        continue;
                      }
                      Lit::Str(str_lit) => {
                        let serialized_attr =
                          serialize_attr(&attr_name, &str_lit.value);

                        strings
                          .last_mut()
                          .unwrap()
                          .push_str(serialized_attr.as_str());
                        continue;
                      }
                      _ => {}
                    },
                    Expr::Unary(unary_expr) => {
                      if unary_expr.op == UnaryOp::Minus {
                        if let Expr::Lit(Lit::Num(num_lit)) = &*unary_expr.arg {
                          let value = format!("-{}", &num_lit.value);
                          let serialized_attr =
                            serialize_attr(&attr_name, &value);

                          strings
                            .last_mut()
                            .unwrap()
                            .push_str(serialized_attr.as_str());
                          continue;
                        };
                      }
                    }
                    _ => {}
                  }

                  strings.last_mut().unwrap().push(' ');
                  strings.push("".to_string());

                  if is_boolean_attr(&attr_name) {
                    let cond_expr = Expr::Cond(CondExpr {
                      span: DUMMY_SP,
                      test: Box::new(expr),
                      cons: Box::new(string_lit_expr(attr_name.into())),
                      alt: Box::new(string_lit_expr("".into())),
                    });
                    dynamic_exprs.push(cond_expr)
                  } else {
                    let call_expr =
                      self.convert_to_jsx_attr_call(attr_name.into(), expr);
                    dynamic_exprs.push(Expr::Call(call_expr));
                  }
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
      imports.push((jsx_ident.clone(), Ident::new("jsx".into(), DUMMY_SP)))
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
      let src = format!("{}/jsx-runtime", self.import_source);

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

    let non_mod_stmt_idx = module
      .body
      .iter()
      .position(|stmt| match stmt {
        ModuleItem::ModuleDecl(mod_dec) => {
          matches!(
            mod_dec,
            ModuleDecl::ExportDecl(_) | ModuleDecl::ExportDefaultDecl(_)
          )
        }
        ModuleItem::Stmt(stmt) => !matches!(stmt, Stmt::Empty(_)),
      })
      .unwrap_or(0);

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

      module.body.insert(
        non_mod_stmt_idx,
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
              *expr = string_lit_expr(jsx_text.value.clone())
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

      let quoted = if mapping.1.contains('-') || mapping.1.contains(':') {
        format!("\"{}\"", &mapping.1)
      } else {
        mapping.1.clone()
      };
      // should still be normalized if HTML element cannot
      // be serialized
      test_transform(
        JsxPrecompile::default(),
        format!("const a = <label {}=\"foo\" {{...foo}} />", &mapping.0)
          .as_str(),
        format!(
          "{}\nconst a = _jsx(\"label\", {{\n  {}: \"foo\",\n  ...foo\n}});",
          "import { jsx as _jsx } from \"react/jsx-runtime\";", quoted
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
      r#"const a = <input type="checkbox" checked={false} required={true} selected={foo} />;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<input type="checkbox" required ',
  ">"
];
const a = _jsxssr($$_tpl_1, foo ? "selected" : "");"#,
    );
  }

  #[test]
  fn dynamic_attr_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <div class="foo" bar={2 + 2}></div>;"#,
      r#"import { jsxssr as _jsxssr, jsxattr as _jsxattr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<div class="foo" ',
  "></div>"
];
const a = _jsxssr($$_tpl_1, _jsxattr("bar", 2 + 2));"#,
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
  fn non_identiifer_attr_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo aria-label="bar" {...props} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  "aria-label": "bar",
  ...props
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
  fn serialize_lit_attr_test() {
    // Numeric literals
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <img width={100} />;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<img width="100">'
];
const a = _jsxssr($$_tpl_1);"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <div tabIndex={-1} />;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<div tabindex="-1"></div>'
];
const a = _jsxssr($$_tpl_1);"#,
    );

    // String literals
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <div foo={"b&>'\"ar"} bar={'baz'} />;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  '<div foo="b&amp;&gt;&#39;&quot;ar" bar="baz"></div>'
];
const a = _jsxssr($$_tpl_1);"#,
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
  fn empty_jsx_text_children_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <p>
      foo
</p>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<p>foo</p>"
];
const a = _jsxssr($$_tpl_1);"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <p>
      foo
      bar
</p>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<p>foo bar</p>"
];
const a = _jsxssr($$_tpl_1);"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <p>
  <span />
</p>;"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<p><span></span></p>"
];
const a = _jsxssr($$_tpl_1);"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo>
  <span />
</Foo>;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<span></span>"
];
const a = _jsx(Foo, {
  children: _jsxssr($$_tpl_1)
});"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo>
  foo
</Foo>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  children: "foo"
});"#,
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
  "<span>hello</span>foo",
  "asdf"
];
const a = _jsx(Foo, {
  children: _jsxssr($$_tpl_1, _jsx(Bar, null))
});"#,
    );
  }

  #[test]
  fn component_with_multiple_children_2_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo><span>hello</span>foo<Bar><p>asdf</p></Bar></Foo>;"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_2 = [
  "<p>asdf</p>"
];
const $$_tpl_1 = [
  "<span>hello</span>foo",
  ""
];
const a = _jsx(Foo, {
  children: _jsxssr($$_tpl_1, _jsx(Bar, {
    children: _jsxssr($$_tpl_2)
  }))
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
      JsxPrecompile::new("foobar".to_string()),
      r#"const a = <div>foo</div>;"#,
      r#"import { jsxssr as _jsxssr } from "foobar/jsx-runtime";
const $$_tpl_1 = [
  "<div>foo</div>"
];
const a = _jsxssr($$_tpl_1);"#,
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

  #[test]
  fn insert_tpl_after_imports_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"import Foo from "./foo.ts";
import Bar from "./bar.ts";
const a = <div />"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
import Foo from "./foo.ts";
import Bar from "./bar.ts";
const $$_tpl_1 = [
  "<div></div>"
];
const a = _jsxssr($$_tpl_1);"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"import Foo from "./foo.ts";

export function foo() {
  return <div />
}"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
import Foo from "./foo.ts";
const $$_tpl_1 = [
  "<div></div>"
];
export function foo() {
  return _jsxssr($$_tpl_1);
}"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"import Foo from "./foo.ts";

export default function foo() {
  return <div />
}"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
import Foo from "./foo.ts";
const $$_tpl_1 = [
  "<div></div>"
];
export default function foo() {
  return _jsxssr($$_tpl_1);
}"#,
    );
  }

  #[test]
  fn merge_component_text_children_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo>foo{" "}bar{' '}</Foo>"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  children: "foo bar "
});"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo>foo{2}bar{true}{false}baz</Foo>"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
  children: "foo2barbaz"
});"#,
    );

    test_transform(
      JsxPrecompile::default(),
      r#"const a = <Foo>foo<div />bar{" "}</Foo>"#,
      r#"import { jsx as _jsx, jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "foo<div></div>bar "
];
const a = _jsx(Foo, {
  children: _jsxssr($$_tpl_1)
});"#,
    );
  }

  #[test]
  fn merge_element_text_children_test() {
    test_transform(
      JsxPrecompile::default(),
      r#"const a = <div>foo{" "}bar{' '}</div>"#,
      r#"import { jsxssr as _jsxssr } from "react/jsx-runtime";
const $$_tpl_1 = [
  "<div>foo bar </div>"
];
const a = _jsxssr($$_tpl_1);"#,
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
