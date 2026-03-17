// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use oxc::allocator::Allocator;
use oxc::allocator::CloneIn;
use oxc::allocator::StringBuilder;
use oxc::ast::AstBuilder;
use oxc::ast::ast::*;
use oxc::ast_visit::VisitMut;
use oxc::ast_visit::walk_mut;
use oxc::span::SPAN;

pub struct JsxPrecompile<'a> {
  ast: AstBuilder<'a>,
  // The import path to import the jsx runtime from. Will be
  // `<import_source>/jsx-runtime`.
  import_source: String,
  // List of HTML elements which should not be serialized
  skip_serialize: Option<Vec<String>>,
  // List of props/attributes that should not be serialized and
  // always be treated as dynamic instead.
  skip_prop_serialize: Option<Vec<String>>,

  // Internal state
  next_index: usize,
  templates: Vec<(usize, Vec<String>)>,
  // Track if we need to import `jsx` and which identifier
  // to use if we do.
  import_jsx: Option<String>,
  // Track if we need to import `jsxTemplate` and which identifier
  // to use if we do.
  import_jsx_ssr: Option<String>,
  // Track if we need to import `jsxAttr` and which identifier
  // to use if we do.
  import_jsx_attr: Option<String>,
  // Track if we need to import `jsxEscape` and which identifier
  // to use if we do.
  import_jsx_escape: Option<String>,
}

impl<'a> JsxPrecompile<'a> {
  pub fn new(
    allocator: &'a Allocator,
    import_source: Option<String>,
    skip_serialize: Option<Vec<String>>,
    skip_prop_serialize: Option<Vec<String>>,
  ) -> Self {
    Self {
      ast: AstBuilder::new(allocator),
      import_source: import_source.unwrap_or_else(|| "react".to_string()),
      skip_serialize,
      skip_prop_serialize,
      next_index: 0,
      templates: vec![],
      import_jsx: None,
      import_jsx_ssr: None,
      import_jsx_attr: None,
      import_jsx_escape: None,
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
    "accentHeight"
    | "acceptCharset"
    | "alignmentBaseline"
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
    | "horizOriginY"
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
    | "textRendering"
    | "transformOrigin"
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

    // Attributes that are camelCased and should be kept as is.
    "allowReorder"
    | "attributeName"
    | "attributeType"
    | "baseFrequency"
    | "baseProfile"
    | "calcMode"
    | "clipPathUnits"
    | "diffuseConstant"
    | "edgeMode"
    | "filterUnits"
    | "glyphRef"
    | "gradientTransform"
    | "gradientUnits"
    | "kernelMatrix"
    | "kernelUnitLength"
    | "keyPoints"
    | "keySplines"
    | "keyTimes"
    | "lengthAdjust"
    | "limitingConeAngle"
    | "markerHeight"
    | "markerUnits"
    | "markerWidth"
    | "maskContentUnits"
    | "maskUnits"
    | "numOctaves"
    | "pathLength"
    | "patternContentUnits"
    | "patternTransform"
    | "patternUnits"
    | "pointsAtX"
    | "pointsAtY"
    | "pointsAtZ"
    | "preserveAlpha"
    | "preserveAspectRatio"
    | "primitiveUnits"
    | "referrerPolicy"
    | "refX"
    | "refY"
    | "repeatCount"
    | "repeatDur"
    | "requiredExtensions"
    | "requiredFeatures"
    | "specularConstant"
    | "specularExponent"
    | "spreadMethod"
    | "startOffset"
    | "stdDeviation"
    | "stitchTiles"
    | "surfaceScale"
    | "systemLanguage"
    | "tableValues"
    | "targetX"
    | "targetY"
    | "textLength"
    | "viewBox"
    | "xChannelSelector"
    | "yChannelSelector"
    | "zoomAndPan" => name.to_string(),

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

fn get_attr_name(jsx_attr: &JSXAttribute, normalize: bool) -> String {
  match &jsx_attr.name {
    // Case: <button class="btn">
    JSXAttributeName::Identifier(ident) => {
      if !normalize {
        ident.name.to_string()
      } else {
        normalize_dom_attr_name(ident.name.as_str())
      }
    }
    // Case: <a xlink:href="#">...</a>
    JSXAttributeName::NamespacedName(namespace_name) => {
      let ns = namespace_name.namespace.name.to_string();
      let name = namespace_name.name.name.to_string();
      let combined = format!("{}:{}", ns, name);
      normalize_dom_attr_name(&combined)
    }
  }
}

fn normalize_lit_str(s: &StringLiteral) -> String {
  let value: &str = s.value.as_str();
  let mut replaced = "".to_string();

  for (i, line) in value.lines().enumerate() {
    if i > 0 {
      replaced.push(' ');
    }
    replaced.push_str(line.trim_start());
  }

  replaced
}

fn jsx_text_to_str(
  jsx_text: &JSXText,
  escape: bool,
  trim_last_child: bool,
) -> String {
  let mut text = String::new();

  let mut lines = jsx_text.value.as_str().lines().enumerate().peekable();
  while let Some((i, line)) = lines.next() {
    let mut line = if i != 0 { line.trim_start() } else { line };

    if lines.peek().is_some() || trim_last_child {
      line = line.trim_end();
    }

    if line.is_empty() {
      continue;
    }

    if i > 0 && !text.is_empty() {
      text.push(' ')
    }

    text.push_str(line);
  }

  if escape { escape_html(&text) } else { text }
}

/// Convert a JSXMemberExpression to a static member expression chain.
fn jsx_member_expr_to_normal<'a>(
  ast: AstBuilder<'a>,
  jsx_member_expr: &JSXMemberExpression<'a>,
) -> Expression<'a> {
  let obj = match &jsx_member_expr.object {
    JSXMemberExpressionObject::IdentifierReference(ident) => {
      ast.expression_identifier(SPAN, ident.name.as_str())
    }
    JSXMemberExpressionObject::MemberExpression(member_expr) => {
      jsx_member_expr_to_normal(ast, member_expr)
    }
    JSXMemberExpressionObject::ThisExpression(_) => {
      Expression::ThisExpression(ast.alloc_this_expression(SPAN))
    }
  };
  let prop = ast.identifier_name(SPAN, jsx_member_expr.property.name.as_str());
  Expression::StaticMemberExpression(
    ast.alloc_static_member_expression(SPAN, obj, prop, false),
  )
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
fn is_serializable(
  opening: &JSXOpeningElement,
  skip_serialize: &Option<Vec<String>>,
) -> bool {
  match &opening.name {
    // Case: <div />
    JSXElementName::Identifier(ident) => {
      let name = ident.name.to_string();
      // This is a plain HTML element identifier (lowercase start),
      // not a component.
      // Component identifiers start with an uppercase character and
      // they cannot be safely serialized.
      // Case: <Foo bar="123" />
      // Note: JSXElementName::Identifier is for lowercase elements.
      // Uppercase ones are JSXElementName::IdentifierReference.
      // But we still check for safety.
      if name.chars().next().unwrap().is_ascii_uppercase() {
        return false;
      }

      if let Some(skip_elements) = skip_serialize
        && skip_elements.contains(&name)
      {
        return false;
      }

      if opening.attributes.is_empty() {
        return true;
      }

      !opening.attributes.iter().any(|attr| match attr {
        JSXAttributeItem::SpreadAttribute(_) => true,
        JSXAttributeItem::Attribute(attr) => {
          let name = get_attr_name(attr, false);
          matches!(name.as_str(), "dangerouslySetInnerHTML")
        }
      })
    }
    // Component identifiers (IdentifierReference) cannot be serialized
    JSXElementName::IdentifierReference(_) => false,
    _ => false,
  }
}

fn is_text_valid_identifier(string_value: &str) -> bool {
  if string_value.is_empty() {
    return false;
  }
  for (i, c) in string_value.chars().enumerate() {
    if i == 0 {
      if !c.is_ascii_alphabetic() && c != '_' && c != '$' {
        return false;
      }
    } else if !c.is_ascii_alphanumeric() && c != '_' && c != '$' {
      return false;
    }
  }
  true
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

/// Information about merged children
struct MergedChildren<'a> {
  children: Vec<JSXChild<'a>>,
  text_count: usize,
  serializable_count: usize,
}

fn merge_serializable_children<'a>(
  ast: AstBuilder<'a>,
  children: &oxc::allocator::Vec<'a, JSXChild<'a>>,
  skip_serialize: &Option<Vec<String>>,
  escape_text_children: bool,
  is_parent_serializable: bool,
) -> MergedChildren<'a> {
  // Do a first pass over children to merge sibling text nodes
  // and check if it contains any serializable nodes.
  let mut text_count = 0;
  let mut serializable_count = 0;
  let mut buf = String::new();

  let mut normalized_children: Vec<JSXChild<'a>> = vec![];
  let mut child_iter = children.iter().peekable();
  while let Some(child) = child_iter.next() {
    match child {
      JSXChild::Text(jsx_text) => {
        let text = jsx_text_to_str(
          jsx_text,
          escape_text_children,
          is_parent_serializable && child_iter.peek().is_none(),
        );
        if text.is_empty() {
          continue;
        }
        text_count += 1;

        buf.push_str(&text);
      }
      JSXChild::ExpressionContainer(jsx_expr_container) => {
        match &jsx_expr_container.expression {
          // Empty JSX expressions can be ignored as they have no content
          // Case: <div>{}</div>
          // Case: <div>{/* fooo */}</div>
          JSXExpression::EmptyExpression(_) => {}
          // Case: <div>{"foo"}</div>
          // Case: <div>{2 + 2}</div>
          expr => {
            if let Some(expr) = expr.as_expression() {
              match expr {
                // Booleans are not rendered because people usually use
                // them for conditional rendering.
                Expression::BooleanLiteral(_) => continue,
                // Can be flattened
                Expression::NumericLiteral(num) => {
                  let text = num.value.to_string();
                  buf.push_str(&text);
                  continue;
                }
                // Can be flattened
                Expression::StringLiteral(str_lit) => {
                  buf.push_str(str_lit.value.as_str());
                  continue;
                }
                _ => {}
              }
            }

            if !buf.is_empty() {
              let alloc_str =
                StringBuilder::from_str_in(&buf, ast.allocator).into_str();
              normalized_children.push(JSXChild::Text(
                ast.alloc_jsx_text(SPAN, alloc_str, None),
              ));

              buf = String::new()
            }

            normalized_children.push(child.clone_in(ast.allocator));
          }
        }
      }
      JSXChild::Element(jsx_el) => {
        if !buf.is_empty() {
          let alloc_str =
            StringBuilder::from_str_in(&buf, ast.allocator).into_str();
          normalized_children
            .push(JSXChild::Text(ast.alloc_jsx_text(SPAN, alloc_str, None)));

          buf = String::new()
        }

        if is_serializable(&jsx_el.opening_element, skip_serialize) {
          serializable_count += 1;
        }

        normalized_children
          .push(JSXChild::Element(jsx_el.clone_in(ast.allocator)));
      }
      // Invalid, was part of an earlier JSX iteration, but no
      // transform supports it. Babel and TypeScript error when they
      // encounter this.
      JSXChild::Spread(_) => {}
      child => {
        if !buf.is_empty() {
          let alloc_str =
            StringBuilder::from_str_in(&buf, ast.allocator).into_str();
          normalized_children
            .push(JSXChild::Text(ast.alloc_jsx_text(SPAN, alloc_str, None)));

          buf = String::new()
        }

        normalized_children.push(child.clone_in(ast.allocator));
      }
    }
  }

  if !buf.is_empty() {
    let alloc_str = StringBuilder::from_str_in(&buf, ast.allocator).into_str();
    normalized_children
      .push(JSXChild::Text(ast.alloc_jsx_text(SPAN, alloc_str, None)));
  }

  MergedChildren {
    children: normalized_children,
    text_count,
    serializable_count,
  }
}

impl<'a> JsxPrecompile<'a> {
  fn alloc_str(&self, s: &str) -> &'a str {
    StringBuilder::from_str_in(s, self.ast.allocator).into_str()
  }

  fn null_arg(&self) -> Argument<'a> {
    Argument::from(self.ast.expression_null_literal(SPAN))
  }

  fn string_lit_expr(&self, value: &str) -> Expression<'a> {
    let s = self.alloc_str(value);
    self.ast.expression_string_literal(SPAN, s, None)
  }

  /// Mark `jsx` as being used and return the identifier name.
  fn get_jsx_identifier(&mut self) -> String {
    match &self.import_jsx {
      Some(name) => name.clone(),
      None => {
        let name = "_jsx".to_string();
        self.import_jsx = Some(name.clone());
        name
      }
    }
  }

  /// Mark `jsxTemplate` as being used and return the identifier name.
  fn get_jsx_ssr_identifier(&mut self) -> String {
    match &self.import_jsx_ssr {
      Some(name) => name.clone(),
      None => {
        let name = "_jsxTemplate".to_string();
        self.import_jsx_ssr = Some(name.clone());
        name
      }
    }
  }

  /// Mark `jsxAttr` as being used and return the identifier name.
  fn get_jsx_attr_identifier(&mut self) -> String {
    match &self.import_jsx_attr {
      Some(name) => name.clone(),
      None => {
        let name = "_jsxAttr".to_string();
        self.import_jsx_attr = Some(name.clone());
        name
      }
    }
  }

  /// Mark `jsxEscape` as being used and return the identifier name.
  fn get_jsx_escape_fn_identifier(&mut self) -> String {
    match &self.import_jsx_escape {
      Some(name) => name.clone(),
      None => {
        let name = "_jsxEscape".to_string();
        self.import_jsx_escape = Some(name.clone());
        name
      }
    }
  }

  fn wrap_with_jsx_escape_call(
    &mut self,
    expr: Expression<'a>,
  ) -> Expression<'a> {
    let callee_name = self.get_jsx_escape_fn_identifier();
    let callee = self
      .ast
      .expression_identifier(SPAN, self.alloc_str(&callee_name));
    let mut args = self.ast.vec_with_capacity(1);
    args.push(Argument::from(expr));

    self.ast.expression_call(
      SPAN,
      callee,
      None::<TSTypeParameterInstantiation<'a>>,
      args,
      false,
    )
  }

  /// Check if we even need to wrap it with an escape call.
  /// If we have a string literal and know that it doesn't need
  /// to be encoded we can skip the escape call.
  fn maybe_wrap_with_jsx_escape_call(
    &mut self,
    expr: Expression<'a>,
  ) -> Expression<'a> {
    match &expr {
      Expression::NumericLiteral(_) => expr,
      Expression::BooleanLiteral(_) => expr,
      Expression::NullLiteral(_) => expr,
      Expression::StringLiteral(string_lit) => {
        let escaped_value = escape_html(string_lit.value.as_str());
        if string_lit.value.as_str() != escaped_value {
          self.wrap_with_jsx_escape_call(expr)
        } else {
          expr
        }
      }
      _ => self.wrap_with_jsx_escape_call(expr),
    }
  }

  fn serialize_jsx_children_to_expr(
    &mut self,
    children: &oxc::allocator::Vec<'a, JSXChild<'a>>,
  ) -> Option<Expression<'a>> {
    // Add children as a "children" prop.
    match children.len() {
      0 => None,
      1 => {
        let child = &children[0];
        match child {
          JSXChild::Text(jsx_text) => {
            let text = jsx_text_to_str(jsx_text, false, true);
            Some(self.string_lit_expr(&text))
          }
          JSXChild::ExpressionContainer(jsx_expr_container) => {
            match &jsx_expr_container.expression {
              // Empty JSX expressions can be ignored as they have no content
              // Case: <div>{}</div>
              // Case: <div>{/* fooo */}</div>
              JSXExpression::EmptyExpression(_) => None,
              expr => {
                expr.as_expression().map(|e| e.clone_in(self.ast.allocator))
              }
            }
          }
          // Case: <div><span /></div>
          JSXChild::Element(jsx_element) => {
            Some(self.serialize_jsx(jsx_element))
          }
          // Case: <div><></></div>
          JSXChild::Fragment(jsx_frag) => {
            self.serialize_jsx_children_to_expr(&jsx_frag.children)
          }
          // Invalid, was part of an earlier JSX iteration, but no
          // transform supports it. Babel and TypeScript error when they
          // encounter this.
          JSXChild::Spread(_) => None,
        }
      }
      _ => {
        let MergedChildren {
          children: normalized_children,
          text_count,
          serializable_count,
        } = merge_serializable_children(
          self.ast,
          children,
          &self.skip_serialize,
          false,
          false,
        );

        // Merge sibling children when they can be serialized into one
        // serialized child. If all children are serializable, we'll
        // merge everything into one big jsxTemplate() call. If everything
        // can be merged into a single string, then we'll go with that.
        let mut elems: Vec<ArrayExpressionElement<'a>> = vec![];

        // Determine whether we should wrap children with a template
        if serializable_count > 1 || serializable_count == 1 && text_count > 0 {
          self.next_index += 1;
          let index = self.next_index;
          let mut strings: Vec<String> = vec![];
          let mut dynamic_exprs: Vec<Expression<'a>> = vec![];
          strings.push("".to_string());

          // Re-process children for template serialization with HTML escaping.
          // The normalized_children were merged with escape=false (component path),
          // but template strings need HTML-escaped text.
          let allocator_children = {
            let mut v = self.ast.vec_with_capacity(normalized_children.len());
            for child in &normalized_children {
              v.push(child.clone_in(self.ast.allocator));
            }
            v
          };
          self.serialize_jsx_children_to_string(
            &allocator_children,
            &mut strings,
            &mut dynamic_exprs,
            false,
          );

          let expr = self.gen_template(index, strings, dynamic_exprs);
          elems.push(ArrayExpressionElement::from(expr));
        } else {
          // Here we parse children the normal way

          for child in normalized_children {
            match child {
              // Case: <div>foo</div>
              JSXChild::Text(jsx_text) => {
                let s = self.alloc_str(jsx_text.value.as_str());
                elems.push(ArrayExpressionElement::from(
                  self.ast.expression_string_literal(SPAN, s, None),
                ));
              }
              // Case: <div>{2 + 2}</div>
              JSXChild::ExpressionContainer(jsx_expr_container) => {
                match jsx_expr_container.expression {
                  // Empty JSX expressions can be ignored as they have no content
                  JSXExpression::EmptyExpression(_) => continue,
                  ref expr => {
                    if let Some(e) = expr.as_expression() {
                      elems.push(ArrayExpressionElement::from(
                        e.clone_in(self.ast.allocator),
                      ));
                    }
                  }
                }
              }
              // Case: <div><span /></div>
              JSXChild::Element(jsx_el) => {
                let expr = self.serialize_jsx(&jsx_el);
                elems.push(ArrayExpressionElement::from(expr));
              }
              // Case: <div><></></div>
              JSXChild::Fragment(jsx_frag) => {
                if let Some(child_expr) =
                  self.serialize_jsx_children_to_expr(&jsx_frag.children)
                {
                  match child_expr {
                    Expression::ArrayExpression(array_lit) => {
                      for elem in array_lit.unbox().elements.into_iter() {
                        elems.push(elem);
                      }
                    }
                    _ => {
                      elems.push(ArrayExpressionElement::from(child_expr));
                    }
                  };
                }
              }
              // Invalid
              JSXChild::Spread(_) => {}
            }
          }
        }

        // Flatten to a single child call when children
        // array only contains one element
        if elems.len() == 1 {
          let first = elems.remove(0);
          if let ArrayExpressionElement::Elision(_) = &first {
            // skip
          } else {
            // Convert ArrayExpressionElement back to Expression
            return Some(first.into_expression());
          }
        }

        let mut arena_elems = self.ast.vec_with_capacity(elems.len());
        for elem in elems {
          arena_elems.push(elem);
        }
        Some(self.ast.expression_array(SPAN, arena_elems))
      }
    }
  }

  /// Serializes a JSXElement to a standard jsx expression. We use
  /// this function when we cannot safely serialize a JSXElement.
  ///
  /// Case: <div {...props} />
  /// Case: <Foo bar="1" />
  /// Case: <Foo.Bar bar="1" />
  fn serialize_jsx_to_call_expr(
    &mut self,
    el: &JSXElement<'a>,
  ) -> Expression<'a> {
    let mut is_component = false;
    let name_expr = match &el.opening_element.name {
      // Case: <div /> (lowercase HTML element)
      JSXElementName::Identifier(ident) => {
        let name = ident.name.as_str();
        self.string_lit_expr(name)
      }
      // Case: <Foo /> (component, starts with uppercase)
      JSXElementName::IdentifierReference(ident) => {
        is_component = true;
        self.ast.expression_identifier(SPAN, ident.name.as_str())
      }
      // Case: <ctx.Provider />
      JSXElementName::MemberExpression(jsx_member_expr) => {
        // Expressions are always treated as component since we can't
        // reliably detect if the variable holds a component or a string.
        is_component = true;
        jsx_member_expr_to_normal(self.ast, jsx_member_expr)
      }
      JSXElementName::NamespacedName(namespace_name) => {
        let ns = namespace_name.namespace.name.to_string();
        let name = namespace_name.name.name.to_string();
        let combined = format!("{}:{}", ns, name);
        self.string_lit_expr(&combined)
      }
      JSXElementName::ThisExpression(_) => {
        is_component = true;
        Expression::ThisExpression(self.ast.alloc_this_expression(SPAN))
      }
    };

    let mut args: Vec<Argument<'a>> = vec![];
    args.push(Argument::from(name_expr));

    let mut key_value: Option<Expression<'a>> = None;

    // Serialize attributes
    if el.opening_element.attributes.is_empty() && el.children.is_empty() {
      args.push(self.null_arg())
    } else {
      let mut props: Vec<ObjectPropertyKind<'a>> = vec![];
      for attr in el.opening_element.attributes.iter() {
        match attr {
          JSXAttributeItem::Attribute(jsx_attr) => {
            let attr_name = get_attr_name(jsx_attr, !is_component);
            let prop_key = if !is_text_valid_identifier(&attr_name) {
              let s = self.alloc_str(&attr_name);
              PropertyKey::StringLiteral(
                self.ast.alloc_string_literal(SPAN, s, None),
              )
            } else {
              self.ast.property_key_static_identifier(
                SPAN,
                self.alloc_str(&attr_name),
              )
            };

            // Case: <Foo required />
            let Some(attr_value) = &jsx_attr.value else {
              let prop = self.ast.object_property(
                SPAN,
                PropertyKind::Init,
                prop_key,
                self.ast.expression_boolean_literal(SPAN, true),
                false,
                false,
                false,
              );
              props
                .push(ObjectPropertyKind::ObjectProperty(self.ast.alloc(prop)));
              continue;
            };

            if attr_name == "key" {
              key_value = match attr_value {
                JSXAttributeValue::StringLiteral(s) => {
                  let normalized = normalize_lit_str(s);
                  Some(self.string_lit_expr(&normalized))
                }
                JSXAttributeValue::ExpressionContainer(jsx_expr_container) => {
                  match &jsx_expr_container.expression {
                    // This is treated as a syntax error in attributes
                    JSXExpression::EmptyExpression(_) => None,
                    expr => expr
                      .as_expression()
                      .map(|e| e.clone_in(self.ast.allocator)),
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
              JSXAttributeValue::StringLiteral(s) => {
                let normalized = normalize_lit_str(s);
                let value_expr = self.string_lit_expr(&normalized);
                let prop = self.ast.object_property(
                  SPAN,
                  PropertyKind::Init,
                  prop_key,
                  value_expr,
                  false,
                  false,
                  false,
                );
                props.push(ObjectPropertyKind::ObjectProperty(
                  self.ast.alloc(prop),
                ));
              }
              JSXAttributeValue::ExpressionContainer(jsx_expr_container) => {
                match &jsx_expr_container.expression {
                  // This is treated as a syntax error in attributes
                  JSXExpression::EmptyExpression(_) => continue,
                  expr => {
                    if let Some(e) = expr.as_expression() {
                      let prop = self.ast.object_property(
                        SPAN,
                        PropertyKind::Init,
                        prop_key,
                        e.clone_in(self.ast.allocator),
                        false,
                        false,
                        false,
                      );
                      props.push(ObjectPropertyKind::ObjectProperty(
                        self.ast.alloc(prop),
                      ));
                    }
                  }
                }
              }
              // There is no valid way to construct these
              JSXAttributeValue::Element(_) => {}
              JSXAttributeValue::Fragment(_) => {}
            }
          }
          // Case: <Foo {...props} />
          JSXAttributeItem::SpreadAttribute(spread_attr) => {
            props.push(self.ast.object_property_kind_spread_property(
              SPAN,
              spread_attr.argument.clone_in(self.ast.allocator),
            ));
          }
        }
      }

      // Add children as a "children" prop.
      let child_expr = self.serialize_jsx_children_to_expr(&el.children);

      if let Some(expr) = child_expr {
        let children_key =
          self.ast.property_key_static_identifier(SPAN, "children");
        let prop = self.ast.object_property(
          SPAN,
          PropertyKind::Init,
          children_key,
          expr,
          false,
          false,
          false,
        );
        props.push(ObjectPropertyKind::ObjectProperty(self.ast.alloc(prop)));
      }

      if props.is_empty() {
        args.push(self.null_arg())
      } else {
        let mut arena_props = self.ast.vec_with_capacity(props.len());
        for prop in props {
          arena_props.push(prop);
        }
        let obj_expr = self.ast.expression_object(SPAN, arena_props);
        args.push(Argument::from(obj_expr));
      }
    }

    if let Some(key_expr) = key_value {
      args.push(Argument::from(key_expr));
    }

    let callee_name = self.get_jsx_identifier();
    let callee = self
      .ast
      .expression_identifier(SPAN, self.alloc_str(&callee_name));
    let mut arena_args = self.ast.vec_with_capacity(args.len());
    for arg in args {
      arena_args.push(arg);
    }

    self.ast.expression_call(
      SPAN,
      callee,
      None::<TSTypeParameterInstantiation<'a>>,
      arena_args,
      false,
    )
  }

  fn convert_to_jsx_attr_call(
    &mut self,
    name: &str,
    expr: Expression<'a>,
  ) -> Expression<'a> {
    let callee_name = self.get_jsx_attr_identifier();
    let callee = self
      .ast
      .expression_identifier(SPAN, self.alloc_str(&callee_name));
    let mut args = self.ast.vec_with_capacity(2);
    args.push(Argument::from(self.string_lit_expr(name)));
    args.push(Argument::from(expr));

    self.ast.expression_call(
      SPAN,
      callee,
      None::<TSTypeParameterInstantiation<'a>>,
      args,
      false,
    )
  }

  fn serialize_jsx_children_to_string(
    &mut self,
    children: &oxc::allocator::Vec<'a, JSXChild<'a>>,
    strings: &mut Vec<String>,
    dynamic_exprs: &mut Vec<Expression<'a>>,
    is_parent_serializable: bool,
  ) {
    let MergedChildren {
      children: normalized_children,
      ..
    } = merge_serializable_children(
      self.ast,
      children,
      &self.skip_serialize,
      true,
      is_parent_serializable,
    );

    self.serialize_jsx_children_to_string_from_vec(
      &normalized_children,
      strings,
      dynamic_exprs,
      is_parent_serializable,
    );
  }

  fn serialize_jsx_children_to_string_from_vec(
    &mut self,
    normalized_children: &[JSXChild<'a>],
    strings: &mut Vec<String>,
    dynamic_exprs: &mut Vec<Expression<'a>>,
    _is_parent_serializable: bool,
  ) {
    for child in normalized_children {
      match child {
        // Case: <div>foo</div>
        JSXChild::Text(jsx_text) => {
          strings
            .last_mut()
            .unwrap()
            .push_str(jsx_text.value.as_str());
        }
        // Case: <div>{2 + 2}</div>
        JSXChild::ExpressionContainer(jsx_expr_container) => {
          match &jsx_expr_container.expression {
            // Empty JSX expressions can be ignored as they have no content
            JSXExpression::EmptyExpression(_) => continue,
            // Case: <div>{2 + 2}</div>
            // Case: <div>{foo}</div>
            // Case: <div>{() => null}</div>
            expr => {
              if let Some(e) = expr.as_expression() {
                strings.push("".to_string());
                let cloned = e.clone_in(self.ast.allocator);
                let escaped_expr = self.maybe_wrap_with_jsx_escape_call(cloned);
                dynamic_exprs.push(escaped_expr);
              }
            }
          }
        }
        // Case: <div><span /></div>
        JSXChild::Element(jsx_element) => self
          .serialize_jsx_element_to_string_vec(
            jsx_element,
            strings,
            dynamic_exprs,
          ),
        // Case: <div><></></div>
        JSXChild::Fragment(jsx_frag) => self.serialize_jsx_children_to_string(
          &jsx_frag.children,
          strings,
          dynamic_exprs,
          false,
        ),
        // Invalid
        JSXChild::Spread(_) => {}
      }
    }
  }

  fn serialize_jsx_element_to_string_vec(
    &mut self,
    el: &JSXElement<'a>,
    strings: &mut Vec<String>,
    dynamic_exprs: &mut Vec<Expression<'a>>,
  ) {
    // Case: <div {...props} />
    // Case: <Foo />
    if !is_serializable(&el.opening_element, &self.skip_serialize) {
      let expr = self.serialize_jsx_to_call_expr(el);
      strings.push("".to_string());
      dynamic_exprs.push(expr);

      return;
    } else if strings.is_empty() {
      strings.push("".to_string());
    }

    let name: &str = match &el.opening_element.name {
      // Case: <div />
      JSXElementName::Identifier(ident) => ident.name.as_str(),
      _ => {
        unreachable!("serialize_jsx_element_to_string_vec(JSXNamespacedName)")
      }
    };

    strings.last_mut().unwrap().push('<');

    let escaped_name = escape_html(name);
    strings.last_mut().unwrap().push_str(escaped_name.as_str());

    for attr in &el.opening_element.attributes {
      // Case: <button class="btn">
      match attr {
        JSXAttributeItem::Attribute(jsx_attr) => {
          // User's can force certain attributes to always be treated
          // as dynamic.
          if let Some(skip_prop_serialize) = &self.skip_prop_serialize.clone() {
            let attr_name = get_attr_name(jsx_attr, false);
            if skip_prop_serialize.contains(&attr_name) {
              strings.last_mut().unwrap().push(' ');
              strings.push("".to_string());

              let value = match &jsx_attr.value {
                Some(attr_value) => match attr_value {
                  JSXAttributeValue::StringLiteral(s) => {
                    let v = s.value.as_str();
                    self.string_lit_expr(v)
                  }
                  JSXAttributeValue::ExpressionContainer(_) => todo!(),
                  JSXAttributeValue::Element(jsx_element) => {
                    self.serialize_jsx(jsx_element)
                  }
                  JSXAttributeValue::Fragment(jsx_frag) => {
                    // Convert fragment to expression
                    self
                      .serialize_jsx_children_to_expr(&jsx_frag.children)
                      .unwrap_or_else(|| self.ast.expression_null_literal(SPAN))
                  }
                },
                None => self.ast.expression_boolean_literal(SPAN, true),
              };

              let expr = self.convert_to_jsx_attr_call(&attr_name, value);
              dynamic_exprs.push(expr);
              continue;
            }
          }

          let attr_name = get_attr_name(jsx_attr, true);

          // Case: <input required />
          let Some(attr_value) = &jsx_attr.value else {
            strings.last_mut().unwrap().push(' ');

            let escaped_attr_name = escape_html(&attr_name);
            if is_boolean_attr(&attr_name) {
              strings
                .last_mut()
                .unwrap()
                .push_str(escaped_attr_name.as_str());
            } else {
              strings.push("".to_string());
              let bool_expr = self.ast.expression_boolean_literal(SPAN, true);
              let expr = self.convert_to_jsx_attr_call(&attr_name, bool_expr);
              dynamic_exprs.push(expr);
            }
            continue;
          };

          // Case: <div class="btn">
          match attr_value {
            JSXAttributeValue::StringLiteral(string_lit) => {
              // Edge Case: Both "key" and "ref" attributes are
              // special attributes in most frameworks.
              if attr_name == "key" || attr_name == "ref" {
                strings.last_mut().unwrap().push(' ');
                strings.push("".to_string());
                let value_expr =
                  self.string_lit_expr(string_lit.value.as_str());
                let expr =
                  self.convert_to_jsx_attr_call(&attr_name, value_expr);
                dynamic_exprs.push(expr);
                continue;
              }

              let serialized_attr =
                serialize_attr(&attr_name, string_lit.value.as_str());

              strings
                .last_mut()
                .unwrap()
                .push_str(serialized_attr.as_str());
            }
            JSXAttributeValue::ExpressionContainer(jsx_expr_container) => {
              match &jsx_expr_container.expression {
                // This is treated as a syntax error in attributes
                JSXExpression::EmptyExpression(_) => {}
                expr => {
                  if let Some(jsx_expr) = expr.as_expression() {
                    let expr_clone = jsx_expr.clone_in(self.ast.allocator);

                    // Serialize numeric literal values
                    // Case: <img width={100} />
                    match &expr_clone {
                      Expression::BooleanLiteral(lit_bool) => {
                        if is_boolean_attr(&attr_name) {
                          if !lit_bool.value {
                            continue;
                          }

                          strings.last_mut().unwrap().push(' ');
                          strings.last_mut().unwrap().push_str(&attr_name);
                          continue;
                        }
                      }
                      Expression::NumericLiteral(num) => {
                        let serialized_attr =
                          serialize_attr(&attr_name, &num.value.to_string());

                        strings
                          .last_mut()
                          .unwrap()
                          .push_str(serialized_attr.as_str());
                        continue;
                      }
                      Expression::StringLiteral(str_lit) => {
                        let serialized_attr =
                          serialize_attr(&attr_name, str_lit.value.as_str());

                        strings
                          .last_mut()
                          .unwrap()
                          .push_str(serialized_attr.as_str());
                        continue;
                      }
                      Expression::UnaryExpression(unary_expr) => {
                        if unary_expr.operator == UnaryOperator::UnaryNegation
                          && let Expression::NumericLiteral(num_lit) =
                            &unary_expr.argument
                        {
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
                      _ => {}
                    }

                    strings.last_mut().unwrap().push(' ');
                    strings.push("".to_string());

                    if is_boolean_attr(&attr_name) {
                      let attr_name_str = self.alloc_str(&attr_name);
                      let cond_expr = self.ast.expression_conditional(
                        SPAN,
                        expr_clone,
                        self.ast.expression_string_literal(
                          SPAN,
                          attr_name_str,
                          None,
                        ),
                        self.ast.expression_string_literal(SPAN, "", None),
                      );
                      dynamic_exprs.push(cond_expr)
                    } else {
                      let call_expr =
                        self.convert_to_jsx_attr_call(&attr_name, expr_clone);
                      dynamic_exprs.push(call_expr);
                    }
                  }
                }
              }
            }
            // These makes no sense on as attribute on HTML elements
            // so we ignore them.
            JSXAttributeValue::Element(_) => {}
            JSXAttributeValue::Fragment(_) => {}
          }
        }
        // This case is already handled earlier
        JSXAttributeItem::SpreadAttribute(_) => {}
      };
    }

    strings.last_mut().unwrap().push('>');

    // There are no self closing elements in HTML, only void elements.
    if is_void_element(name) {
      return;
    }

    self.serialize_jsx_children_to_string(
      &el.children,
      strings,
      dynamic_exprs,
      true,
    );

    let closing_tag = format!("</{}>", name);
    strings.last_mut().unwrap().push_str(closing_tag.as_str());
  }

  fn gen_template(
    &mut self,
    template_index: usize,
    static_strs: Vec<String>,
    dynamic_exprs: Vec<Expression<'a>>,
  ) -> Expression<'a> {
    let name = create_tpl_binding_name(template_index);
    self.templates.push((template_index, static_strs));

    let mut args = self.ast.vec_with_capacity(1 + dynamic_exprs.len());
    args.push(Argument::from(
      self.ast.expression_identifier(SPAN, self.alloc_str(&name)),
    ));
    for dynamic_expr in dynamic_exprs.into_iter() {
      args.push(Argument::from(dynamic_expr));
    }

    // Case: _jsxTemplate($$_tpl_1);
    let jsx_ident_name = self.get_jsx_ssr_identifier();
    let callee = self
      .ast
      .expression_identifier(SPAN, self.alloc_str(&jsx_ident_name));

    self.ast.expression_call(
      SPAN,
      callee,
      None::<TSTypeParameterInstantiation<'a>>,
      args,
      false,
    )
  }

  fn serialize_jsx(&mut self, el: &JSXElement<'a>) -> Expression<'a> {
    if is_serializable(&el.opening_element, &self.skip_serialize) {
      // These are now safe to be serialized
      self.next_index += 1;
      let index = self.next_index;
      let mut static_strs: Vec<String> = vec![];
      let mut dynamic_exprs: Vec<Expression<'a>> = vec![];
      self.serialize_jsx_element_to_string_vec(
        el,
        &mut static_strs,
        &mut dynamic_exprs,
      );

      self.gen_template(index, static_strs, dynamic_exprs)
    } else {
      // Case: <div {...props} />
      self.serialize_jsx_to_call_expr(el)
    }
  }

  fn inject_runtime(
    &mut self,
    stmts: &mut oxc::allocator::Vec<'a, Statement<'a>>,
  ) {
    let mut imports: Vec<(String, String)> = vec![];

    if let Some(jsx_ident) = &self.import_jsx {
      imports.push((jsx_ident.clone(), "jsx".to_string()))
    }

    if let Some(jsx_ssr_ident) = &self.import_jsx_ssr {
      imports.push((jsx_ssr_ident.clone(), "jsxTemplate".to_string()))
    }

    if let Some(jsx_attr_ident) = &self.import_jsx_attr {
      imports.push((jsx_attr_ident.clone(), "jsxAttr".to_string()))
    }

    if let Some(escape_ident) = self.import_jsx_escape.take() {
      imports.push((escape_ident, "jsxEscape".to_string()))
    }

    if !imports.is_empty() {
      let src = format!("{}/jsx-runtime", self.import_source);

      let mut specifiers = self.ast.vec_with_capacity(imports.len());
      for (local_name, imported_name) in imports.into_iter() {
        let imported = self.ast.module_export_name_identifier_name(
          SPAN,
          self.alloc_str(&imported_name),
        );
        let local = self
          .ast
          .binding_identifier(SPAN, self.alloc_str(&local_name));
        specifiers.push(ImportDeclarationSpecifier::ImportSpecifier(
          self.ast.alloc_import_specifier(
            SPAN,
            imported,
            local,
            ImportOrExportKind::Value,
          ),
        ));
      }

      let src_str = self.ast.string_literal(SPAN, self.alloc_str(&src), None);
      let import_decl = self.ast.import_declaration(
        SPAN,
        Some(specifiers),
        src_str,
        None,
        None::<WithClause<'a>>,
        ImportOrExportKind::Value,
      );

      let import_stmt =
        Statement::ImportDeclaration(self.ast.alloc(import_decl));

      // Prepend the import statement
      stmts.insert(0, import_stmt);
    }
  }
}

impl<'a> VisitMut<'a> for JsxPrecompile<'a> {
  fn visit_program(&mut self, program: &mut Program<'a>) {
    // Visit children first (transforms expressions)
    walk_mut::walk_program(self, program);

    // Find the first non-import statement index so templates are inserted
    // after all import declarations.
    let non_mod_stmt_idx = program
      .body
      .iter()
      .position(|stmt| {
        !matches!(
          stmt,
          Statement::ImportDeclaration(_) | Statement::EmptyStatement(_)
        )
      })
      .unwrap_or(0);

    for (idx, strings) in self.templates.iter().rev() {
      let mut elems = self.ast.vec_with_capacity(strings.len());
      for el in strings.iter() {
        let s = self.alloc_str(el);
        elems.push(ArrayExpressionElement::from(
          self.ast.expression_string_literal(SPAN, s, None),
        ));
      }

      let arr_expr = self.ast.expression_array(SPAN, elems);
      let binding_name = create_tpl_binding_name(*idx);
      let pattern = self.ast.binding_pattern_binding_identifier(
        SPAN,
        self.alloc_str(&binding_name),
      );
      let mut declarators = self.ast.vec_with_capacity(1);
      declarators.push(self.ast.variable_declarator(
        SPAN,
        VariableDeclarationKind::Const,
        pattern,
        None::<TSTypeAnnotation<'a>>,
        Some(arr_expr),
        false,
      ));

      let var_decl = Statement::from(self.ast.declaration_variable(
        SPAN,
        VariableDeclarationKind::Const,
        declarators,
        false,
      ));

      program.body.insert(non_mod_stmt_idx, var_decl);
    }

    self.inject_runtime(&mut program.body);
  }

  fn visit_expression(&mut self, expr: &mut Expression<'a>) {
    if let Expression::JSXElement(el) = expr {
      let el_ref: &JSXElement<'a> = el;
      *expr = self.serialize_jsx(el_ref);
    } else if let Expression::JSXFragment(frag) = expr {
      match frag.children.len() {
        0 => {
          // Empty fragments can be replaced with null.
          *expr = self.ast.expression_null_literal(SPAN);
        }
        1 => {
          // Flatten the fragment if it only has one child
          let child = &frag.children[0];
          match child {
            JSXChild::Text(_) | JSXChild::ExpressionContainer(_) => {
              // We always need to materialize a Fragment with a single
              // text child to avoid double escaping.
              self.next_index += 1;
              let index = self.next_index;
              let mut strings: Vec<String> = vec![];
              let mut dynamic_exprs: Vec<Expression<'a>> = vec![];

              strings.push("".to_string());

              self.serialize_jsx_children_to_string(
                &frag.children,
                &mut strings,
                &mut dynamic_exprs,
                false,
              );
              *expr = self.gen_template(index, strings, dynamic_exprs)
            }
            // Case: <><span /></>
            JSXChild::Element(jsx_element) => {
              *expr = self.serialize_jsx(jsx_element);
            }
            // Case: <><></></>
            JSXChild::Fragment(jsx_frag) => {
              let serialized =
                self.serialize_jsx_children_to_expr(&jsx_frag.children);
              if let Some(serialized_expr) = serialized {
                *expr = serialized_expr
              }
            }
            // Invalid
            JSXChild::Spread(_) => {}
          }
        }
        _ => {
          self.next_index += 1;
          let index = self.next_index;
          let mut strings: Vec<String> = vec![];
          let mut dynamic_exprs: Vec<Expression<'a>> = vec![];

          strings.push("".to_string());

          self.serialize_jsx_children_to_string(
            &frag.children,
            &mut strings,
            &mut dynamic_exprs,
            false,
          );
          *expr = self.gen_template(index, strings, dynamic_exprs)
        }
      }
    }

    walk_mut::walk_expression(self, expr);
  }
}

#[cfg(test)]
mod tests {
  use std::collections::HashMap;

  use oxc::allocator::Allocator;
  use oxc::ast_visit::VisitMut;
  use oxc::codegen::Codegen;
  use oxc::parser::Parser;
  use oxc::span::SourceType;
  use pretty_assertions::assert_eq;

  use super::*;

  /// Normalize output for comparison: collapse all whitespace runs
  /// (including newlines) to a single space so that OXC vs SWC
  /// formatting differences don't cause spurious failures.
  ///
  /// Also normalizes string quoting: converts all string literals to
  /// use double quotes with escaped inner double quotes, so that
  /// `'foo"bar'` and `"foo\"bar"` compare equal.
  fn normalize(s: &str) -> String {
    let s = s.trim();
    let mut result = String::new();
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    let mut prev_ws = false;

    while i < chars.len() {
      let ch = chars[i];

      // Handle string literals
      if ch == '"' || ch == '\'' {
        let quote = ch;
        result.push('"'); // always use double quotes in normalized form
        i += 1;
        let mut string_content = String::new();
        while i < chars.len() {
          if chars[i] == '\\' && i + 1 < chars.len() {
            let escaped = chars[i + 1];
            if escaped == quote {
              // \' in single-quoted or \" in double-quoted → literal quote char
              string_content.push(escaped);
            } else {
              string_content.push('\\');
              string_content.push(escaped);
            }
            i += 2;
          } else if chars[i] == quote {
            i += 1;
            break;
          } else {
            string_content.push(chars[i]);
            i += 1;
          }
        }
        // Re-escape double quotes in the content for normalized form
        result.push_str(&string_content.replace('"', "\\\""));
        result.push('"');
        prev_ws = false;
      } else if ch.is_whitespace() {
        if !prev_ws {
          result.push(' ');
        }
        prev_ws = true;
        i += 1;
      } else {
        prev_ws = false;
        result.push(ch);
        i += 1;
      }
    }
    // Normalize bracket spacing
    result
      .replace("[ ", "[")
      .replace(" ]", "]")
      .replace("( ", "(")
      .replace(" )", ")")
      .replace("{ ", "{")
      .replace(" }", "}")
  }

  #[track_caller]
  fn test_transform<'a>(
    allocator: &'a Allocator,
    mut transform: JsxPrecompile<'a>,
    src: &str,
    expected_output: &str,
  ) {
    let source_type = SourceType::tsx();
    let source_in_arena = allocator.alloc_str(src);
    let mut ret = Parser::new(allocator, source_in_arena, source_type).parse();
    assert!(!ret.panicked, "Parse failed for: {src}");
    transform.visit_program(&mut ret.program);
    let output = Codegen::new().build(&ret.program).code;
    assert_eq!(normalize(&output), normalize(expected_output));
  }

  #[test]
  fn basic_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div>Hello!</div>;
const b = <div>Hello {name}!</div>;
const c = <button class="btn" onClick={onClick}>Hello {name}!</button>;
"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxAttr as _jsxAttr, jsxEscape as _jsxEscape } from "react/jsx-runtime";
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
const a = _jsxTemplate($$_tpl_1);
const b = _jsxTemplate($$_tpl_2, _jsxEscape(name));
const c = _jsxTemplate($$_tpl_3, _jsxAttr("onclick", onClick), _jsxEscape(name));"#,
    );
  }

  #[test]
  fn convert_self_closing_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div />;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div></div>"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    // Void elements
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <br></br>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<br>"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );
  }

  #[test]
  fn boolean_attr_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <input type="checkbox" checked />;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	'<input type="checkbox" checked>'
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <input type="checkbox" checked={false} required={true} selected={foo} />;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	'<input type="checkbox" required ',
	">"
];
const a = _jsxTemplate($$_tpl_1, foo ? "selected" : "");"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div f-client-nav />;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxAttr as _jsxAttr } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div ",
	"></div>"
];
const a = _jsxTemplate($$_tpl_1, _jsxAttr("f-client-nav", true));"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div f-client-nav={false} />;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxAttr as _jsxAttr } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div ",
	"></div>"
];
const a = _jsxTemplate($$_tpl_1, _jsxAttr("f-client-nav", false));"#,
    );
  }

  #[test]
  fn dynamic_attr_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div class="foo" bar={2 + 2}></div>;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxAttr as _jsxAttr } from "react/jsx-runtime";
const $$_tpl_1 = [
	'<div class="foo" ',
	"></div>"
];
const a = _jsxTemplate($$_tpl_1, _jsxAttr("bar", 2 + 2));"#,
    );
  }

  #[test]
  fn namespace_attr_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <a xlink:href="foo">foo</a>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	'<a href="foo">foo</a>'
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <a foo:bar="foo">foo</a>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	'<a foo:bar="foo">foo</a>'
];
const a = _jsxTemplate($$_tpl_1);"#,
    );
  }

  #[test]
  fn mixed_static_dynamic_props_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
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
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
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
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
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
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div key="foo">foo</div>;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxAttr as _jsxAttr } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div ",
	">foo</div>"
];
const a = _jsxTemplate($$_tpl_1, _jsxAttr("key", "foo"));"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div key={foo}>foo</div>;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxAttr as _jsxAttr } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div ",
	">foo</div>"
];
const a = _jsxTemplate($$_tpl_1, _jsxAttr("key", foo));"#,
    );
  }

  #[test]
  fn key_attr_comp_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo key="foo" />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, null, "foo");"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo key={2} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, null, 2);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo key={2}>foo</Foo>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
	children: "foo"
}, 2);"#,
    );
  }

  #[test]
  fn ref_attr_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div ref="foo">foo</div>;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxAttr as _jsxAttr } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div ",
	">foo</div>"
];
const a = _jsxTemplate($$_tpl_1, _jsxAttr("ref", "foo"));"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div ref={bar}>foo</div>;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxAttr as _jsxAttr } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div ",
	">foo</div>"
];
const a = _jsxTemplate($$_tpl_1, _jsxAttr("ref", bar));"#,
    );
  }

  #[test]
  fn serialize_lit_attr_test() {
    // Numeric literals
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <img width={100} />;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	'<img width="100">'
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div tabIndex={-1} />;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	'<div tabindex="-1"></div>'
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    // String literals
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div foo={"b&>'\"ar"} bar={'baz'} />;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	'<div foo="b&amp;&gt;&#39;&quot;ar" bar="baz"></div>'
];
const a = _jsxTemplate($$_tpl_1);"#,
    );
  }

  #[test]
  fn escape_attr_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div class="a&<>'">foo</div>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	'<div class="a&amp;&lt;&gt;&#39;">foo</div>'
];
const a = _jsxTemplate($$_tpl_1);"#,
    );
  }

  #[test]
  fn escape_children_test() {
    // Note: OXC's parser doesn't accept unescaped single quotes in JSX text
    // (`<div>"a&>'</div>`) unlike SWC. Test with chars that need escaping.
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div>"a&</div>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div>&quot;a&amp;</div>"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const child = [`"a&>'`].join("");
const a = <div>{child}</div>;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxEscape as _jsxEscape } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div>",
	"</div>"
];
const child = [`"a&>'`].join("");
const a = _jsxTemplate($$_tpl_1, _jsxEscape(child));"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div>{foo}{bar}</div>;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxEscape as _jsxEscape } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div>",
	"",
	"</div>"
];
const a = _jsxTemplate($$_tpl_1, _jsxEscape(foo), _jsxEscape(bar));"#,
    );
  }

  #[test]
  fn namespace_name_test() {
    let allocator = Allocator::default();
    // Note: This isn't really supported anywhere, but I guess why not
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <a:b>foo</a:b>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx("a:b", {
	children: "foo"
});"#,
    );
  }

  #[test]
  fn empty_jsx_child_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <p>{}</p>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<p></p>"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );
  }

  #[test]
  fn empty_jsx_text_children_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <p>
      foo
</p>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<p>foo</p>"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <p>
      foo
      bar
</p>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<p>foo bar</p>"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <p>
  <span />
</p>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<p><span></span></p>"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo>
  <span />
</Foo>;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<span></span>"
];
const a = _jsx(Foo, {
	children: _jsxTemplate($$_tpl_1)
});"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
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
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <p>{2 + 2}</p>;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxEscape as _jsxEscape } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<p>",
	"</p>"
];
const a = _jsxTemplate($$_tpl_1, _jsxEscape(2 + 2));"#,
    );
  }

  #[test]
  fn empty_fragment_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <></>;"#,
      r#"const a = null;"#,
    );
  }

  #[test]
  fn fragment_expr_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <>{"foo"}</>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"foo"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <>&'"</>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"&amp;&#39;&quot;"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );
  }

  #[test]
  fn fragment_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <>foo</>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"foo"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <>&'"</>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"&amp;&#39;&quot;"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );
  }

  #[test]
  fn fragment_nested_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <><>foo</></>;"#,
      r#"const a = "foo";"#,
    );
  }

  #[test]
  fn text_indent_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      // force tab indentation
      "const result = <div>\n\t\t\tfoo\t\t\n\t\t\tbar\t\t\n</div>;",
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div>foo bar</div>"
];
const result = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      // force space indentation
      "const result = <div>\n  foo    \n  bar    \n</div>;",
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div>foo bar</div>"
];
const result = _jsxTemplate($$_tpl_1);"#,
    );
  }

  #[test]
  fn fragment_mulitple_children_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <>foo<div /><Foo /></>;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"foo<div></div>",
	""
];
const a = _jsxTemplate($$_tpl_1, _jsx(Foo, null));"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <>{foo}<Foo /></>;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate, jsxEscape as _jsxEscape } from "react/jsx-runtime";
const $$_tpl_1 = [
	"",
	"",
	""
];
const a = _jsxTemplate($$_tpl_1, _jsxEscape(foo), _jsx(Foo, null));"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <>{foo}</>;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxEscape as _jsxEscape } from "react/jsx-runtime";
const $$_tpl_1 = [
	"",
	""
];
const a = _jsxTemplate($$_tpl_1, _jsxEscape(foo));"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <>{foo}{bar}</>;"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxEscape as _jsxEscape } from "react/jsx-runtime";
const $$_tpl_1 = [
	"",
	"",
	""
];
const a = _jsxTemplate($$_tpl_1, _jsxEscape(foo), _jsxEscape(bar));"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo><div /><><>foo</><span /></></Foo>;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div></div>"
];
const $$_tpl_2 = [
	"<span></span>"
];
const a = _jsx(Foo, {
	children: [
		_jsxTemplate($$_tpl_1),
		"foo",
		_jsxTemplate($$_tpl_2)
	]
});"#,
    );
  }

  #[test]
  fn fragment_escape_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const Component = (props: any) => <>{props.children}</>;
const jsx1 = (
  <Component>
    "test"
    <span>test</span>
  </Component>
);
const jsx2 = (
  <Component>
    "test"
  </Component>
);"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate, jsxEscape as _jsxEscape } from "react/jsx-runtime";
const $$_tpl_1 = [
	"",
	""
];
const $$_tpl_2 = [
	"&quot;test&quot;<span>test</span>"
];
const Component = (props: any) => _jsxTemplate($$_tpl_1, _jsxEscape(props.children));
const jsx1 = _jsx(Component, {
	children: _jsxTemplate($$_tpl_2)
});
const jsx2 = _jsx(Component, {
	children: '"test"'
});"#,
    )
  }

  #[test]
  fn nested_elements_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div>foo<p>bar</p></div>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div>foo<p>bar</p></div>"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );
  }

  #[test]
  fn prop_spread_without_children_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div {...props} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx("div", {
	...props
});"#,
    );
  }

  #[test]
  fn prop_spread_with_children_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
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
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
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
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div><Foo /></div>;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div>",
	"</div>"
];
const a = _jsxTemplate($$_tpl_1, _jsx(Foo, null));"#,
    );
  }

  #[test]
  fn component_outer_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, null);"#,
    );
  }

  #[test]
  fn component_with_props_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
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
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
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
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo>bar</Foo>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
	children: "bar"
});"#,
    );
  }

  #[test]
  fn component_with_children_jsx_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo><span>hello</span></Foo>;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<span>hello</span>"
];
const a = _jsx(Foo, {
	children: _jsxTemplate($$_tpl_1)
});"#,
    );
  }

  #[test]
  fn component_with_multiple_children_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo><span>hello</span>foo<Bar />asdf</Foo>;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<span>hello</span>foo",
	"asdf"
];
const a = _jsx(Foo, {
	children: _jsxTemplate($$_tpl_1, _jsx(Bar, null))
});"#,
    );
  }

  #[test]
  fn component_with_multiple_children_2_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo><span>hello</span>foo<Bar><p>asdf</p></Bar></Foo>;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_2 = [
	"<p>asdf</p>"
];
const $$_tpl_1 = [
	"<span>hello</span>foo",
	""
];
const a = _jsx(Foo, {
	children: _jsxTemplate($$_tpl_1, _jsx(Bar, {
		children: _jsxTemplate($$_tpl_2)
	}))
});"#,
    );
  }

  #[test]
  fn component_child_expr_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo>{2 + 2}</Foo>;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
	children: 2 + 2
});"#,
    );
  }

  #[test]
  fn component_with_jsx_attr_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo bar={<div>hello</div>} />;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div>hello</div>"
];
const a = _jsx(Foo, {
	bar: _jsxTemplate($$_tpl_1)
});"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
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
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo bar={<>foo</>} />;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"foo"
];
const a = _jsx(Foo, {
	bar: _jsxTemplate($$_tpl_1)
});"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo bar={<>foo<Foo/>bar</>} />;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"foo",
	"bar"
];
const a = _jsx(Foo, {
	bar: _jsxTemplate($$_tpl_1, _jsx(Foo, null))
});"#,
    );
  }

  #[test]
  fn component_with_nested_frag_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
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
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <ctx.Provider value={null} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(ctx.Provider, {
	value: null
});"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <a.b.c.d value={null} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(a.b.c.d, {
	value: null
});"#,
    );
  }

  #[test]
  fn component_prop_casing_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo someCasing={2} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
	someCasing: 2
});"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <MyIsland.Foo someCasing={2} />;"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(MyIsland.Foo, {
	someCasing: 2
});"#,
    );
  }

  #[test]
  fn import_source_option_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, Some("foobar".to_string()), None, None),
      r#"const a = <div>foo</div>;"#,
      r#"import { jsxTemplate as _jsxTemplate } from "foobar/jsx-runtime";
const $$_tpl_1 = [
	"<div>foo</div>"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );
  }

  #[test]
  fn template_index_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"<div><Foo><span /></Foo></div>;"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_2 = [
	"<span></span>"
];
const $$_tpl_1 = [
	"<div>",
	"</div>"
];
_jsxTemplate($$_tpl_1, _jsx(Foo, {
	children: _jsxTemplate($$_tpl_2)
}));"#,
    );
  }

  #[test]
  fn multi_jsx_string_line_to_jsx_call_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
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
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"import Foo from "./foo.ts";
import Bar from "./bar.ts";
const a = <div />"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
import Foo from "./foo.ts";
import Bar from "./bar.ts";
const $$_tpl_1 = [
	"<div></div>"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"import Foo from "./foo.ts";

export function foo() {
  return <div />
}"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
import Foo from "./foo.ts";
const $$_tpl_1 = [
	"<div></div>"
];
export function foo() {
	return _jsxTemplate($$_tpl_1);
}"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"import Foo from "./foo.ts";

export default function foo() {
  return <div />
}"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
import Foo from "./foo.ts";
const $$_tpl_1 = [
	"<div></div>"
];
export default function foo() {
	return _jsxTemplate($$_tpl_1);
}"#,
    );
  }

  #[test]
  fn merge_component_text_children_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo>foo{" "}bar{' '}</Foo>"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
	children: "foo bar "
});"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo>foo{2}bar{true}{false}baz</Foo>"#,
      r#"import { jsx as _jsx } from "react/jsx-runtime";
const a = _jsx(Foo, {
	children: "foo2barbaz"
});"#,
    );

    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <Foo>foo<div />bar{" "}</Foo>"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"foo<div></div>bar "
];
const a = _jsx(Foo, {
	children: _jsxTemplate($$_tpl_1)
});"#,
    );
  }

  #[test]
  fn merge_element_text_children_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(&allocator, None, None, None),
      r#"const a = <div>foo{" "}bar{' '}</div>"#,
      r#"import { jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div>foo bar </div>"
];
const a = _jsxTemplate($$_tpl_1);"#,
    );
  }

  #[test]
  fn skip_serialization_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(
        &allocator,
        Some("react".to_string()),
        Some(vec!["a".to_string(), "img".to_string()]),
        None,
      ),
      r#"const a = <div><img src="foo.jpg"/><a href="\#">foo</a></div>"#,
      r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div>",
	"",
	"</div>"
];
const a = _jsxTemplate($$_tpl_1, _jsx("img", {
	src: "foo.jpg"
}), _jsx("a", {
	href: "\\#",
	children: "foo"
}));"#,
    );
  }

  #[test]
  fn skip_prop_serialization_test() {
    let allocator = Allocator::default();
    test_transform(
      &allocator,
      JsxPrecompile::new(
        &allocator,
        Some("react".to_string()),
        None,
        Some(vec!["class".to_string(), "className".to_string()]),
      ),
      r#"const a = <div class="foo"><img id="foo" className="foo" /></div>"#,
      r#"import { jsxTemplate as _jsxTemplate, jsxAttr as _jsxAttr } from "react/jsx-runtime";
const $$_tpl_1 = [
	"<div ",
	'><img id="foo" ',
	"></div>"
];
const a = _jsxTemplate($$_tpl_1, _jsxAttr("class", "foo"), _jsxAttr("className", "foo"));"#,
    );
  }

  #[test]
  fn attr_casing_test() {
    let values = HashMap::from([
      ("accentHeight", "accent-height"),
      ("acceptCharset", "accept-charset"),
      ("alignmentBaseline", "alignment-baseline"),
      ("allowReorder", "allowReorder"),
      ("arabicForm", "arabic-form"),
      ("attributeName", "attributeName"),
      ("attributeType", "attributeType"),
      ("baseFrequency", "baseFrequency"),
      ("baselineShift", "baseline-shift"),
      ("baseProfile", "baseProfile"),
      ("calcMode", "calcMode"),
      ("capHeight", "cap-height"),
      ("className", "class"),
      ("clipPath", "clip-path"),
      ("clipPathUnits", "clipPathUnits"),
      ("clipRule", "clip-rule"),
      ("colorInterpolation", "color-interpolation"),
      ("colorInterpolationFilters", "color-interpolation-filters"),
      ("colorProfile", "color-profile"),
      ("colorRendering", "color-rendering"),
      ("contentScriptType", "content-script-type"),
      ("contentStyleType", "content-style-type"),
      ("diffuseConstant", "diffuseConstant"),
      ("dominantBaseline", "dominant-baseline"),
      ("edgeMode", "edgeMode"),
      ("enableBackground", "enable-background"),
      ("fillOpacity", "fill-opacity"),
      ("fillRule", "fill-rule"),
      ("filterUnits", "filterUnits"),
      ("floodColor", "flood-color"),
      ("floodOpacity", "flood-opacity"),
      ("fontFamily", "font-family"),
      ("fontSize", "font-size"),
      ("fontSizeAdjust", "font-size-adjust"),
      ("fontStretch", "font-stretch"),
      ("fontStyle", "font-style"),
      ("fontVariant", "font-variant"),
      ("fontWeight", "font-weight"),
      ("glyphName", "glyph-name"),
      ("glyphOrientationHorizontal", "glyph-orientation-horizontal"),
      ("glyphOrientationVertical", "glyph-orientation-vertical"),
      ("glyphRef", "glyphRef"),
      ("gradientTransform", "gradientTransform"),
      ("gradientUnits", "gradientUnits"),
      ("horizAdvX", "horiz-adv-x"),
      ("horizOriginX", "horiz-origin-x"),
      ("horizOriginY", "horiz-origin-y"),
      ("htmlFor", "for"),
      ("httpEquiv", "http-equiv"),
      ("imageRendering", "image-rendering"),
      ("kernelMatrix", "kernelMatrix"),
      ("kernelUnitLength", "kernelUnitLength"),
      ("keyPoints", "keyPoints"),
      ("keySplines", "keySplines"),
      ("keyTimes", "keyTimes"),
      ("lengthAdjust", "lengthAdjust"),
      ("letterSpacing", "letter-spacing"),
      ("lightingColor", "lighting-color"),
      ("limitingConeAngle", "limitingConeAngle"),
      ("markerEnd", "marker-end"),
      ("markerHeight", "markerHeight"),
      ("markerMid", "marker-mid"),
      ("markerStart", "marker-start"),
      ("markerUnits", "markerUnits"),
      ("markerWidth", "markerWidth"),
      ("maskContentUnits", "maskContentUnits"),
      ("maskUnits", "maskUnits"),
      ("numOctaves", "numOctaves"),
      ("overlinePosition", "overline-position"),
      ("overlineThickness", "overline-thickness"),
      ("paintOrder", "paint-order"),
      ("panose1", "panose-1"),
      ("pathLength", "pathLength"),
      ("patternContentUnits", "patternContentUnits"),
      ("patternTransform", "patternTransform"),
      ("patternUnits", "patternUnits"),
      ("pointsAtX", "pointsAtX"),
      ("pointsAtY", "pointsAtY"),
      ("pointsAtZ", "pointsAtZ"),
      ("pointerEvents", "pointer-events"),
      ("preserveAlpha", "preserveAlpha"),
      ("preserveAspectRatio", "preserveAspectRatio"),
      ("primitiveUnits", "primitiveUnits"),
      ("referrerPolicy", "referrerPolicy"),
      ("refX", "refX"),
      ("refY", "refY"),
      ("renderingIntent", "rendering-intent"),
      ("repeatCount", "repeatCount"),
      ("repeatDur", "repeatDur"),
      ("requiredExtensions", "requiredExtensions"),
      ("requiredFeatures", "requiredFeatures"),
      ("shapeRendering", "shape-rendering"),
      ("specularConstant", "specularConstant"),
      ("specularExponent", "specularExponent"),
      ("spreadMethod", "spreadMethod"),
      ("startOffset", "startOffset"),
      ("stdDeviation", "stdDeviation"),
      ("stitchTiles", "stitchTiles"),
      ("stopColor", "stop-color"),
      ("stopOpacity", "stop-opacity"),
      ("strikethroughPosition", "strikethrough-position"),
      ("strikethroughThickness", "strikethrough-thickness"),
      ("strokeDasharray", "stroke-dasharray"),
      ("strokeDashoffset", "stroke-dashoffset"),
      ("strokeLinecap", "stroke-linecap"),
      ("strokeLinejoin", "stroke-linejoin"),
      ("strokeMiterlimit", "stroke-miterlimit"),
      ("strokeOpacity", "stroke-opacity"),
      ("strokeWidth", "stroke-width"),
      ("surfaceScale", "surfaceScale"),
      ("systemLanguage", "systemLanguage"),
      ("tableValues", "tableValues"),
      ("targetX", "targetX"),
      ("targetY", "targetY"),
      ("textAnchor", "text-anchor"),
      ("textDecoration", "text-decoration"),
      ("textLength", "textLength"),
      ("textRendering", "text-rendering"),
      ("transformOrigin", "transform-origin"),
      ("underlinePosition", "underline-position"),
      ("underlineThickness", "underline-thickness"),
      ("unicodeBidi", "unicode-bidi"),
      ("unicodeRange", "unicode-range"),
      ("unitsPerEm", "units-per-em"),
      ("vAlphabetic", "v-alphabetic"),
      ("viewBox", "viewBox"),
      ("vectorEffect", "vector-effect"),
      ("vertAdvY", "vert-adv-y"),
      ("vertOriginX", "vert-origin-x"),
      ("vertOriginY", "vert-origin-y"),
      ("vHanging", "v-hanging"),
      ("vMathematical", "v-mathematical"),
      ("wordSpacing", "word-spacing"),
      ("writingMode", "writing-mode"),
      ("xChannelSelector", "xChannelSelector"),
      ("xHeight", "x-height"),
      ("xlinkActuate", "xlink:actuate"),
      ("xlinkArcrole", "xlink:arcrole"),
      ("xlinkHref", "href"),
      ("xlink:href", "href"),
      ("xlinkRole", "xlink:role"),
      ("xlinkShow", "xlink:show"),
      ("xlinkTitle", "xlink:title"),
      ("xlinkType", "xlink:type"),
      ("xmlBase", "xml:base"),
      ("xmlLang", "xml:lang"),
      ("xmlSpace", "xml:space"),
      ("yChannelSelector", "yChannelSelector"),
      ("zoomAndPan", "zoomAndPan"),
    ]);

    for (key, value) in values.into_iter() {
      let allocator = Allocator::default();
      let input = format!("const a = <div {}=\"foo\" />", key);
      let expected = [
        "import { jsxTemplate as _jsxTemplate } from \"react/jsx-runtime\";",
        "const $$_tpl_1 = [",
        &format!("\t'<div {}=\"foo\"></div>'", value),
        "];",
        "const a = _jsxTemplate($$_tpl_1);",
      ]
      .join("\n");
      test_transform(
        &allocator,
        JsxPrecompile::new(&allocator, Some("react".to_string()), None, None),
        &input,
        &expected,
      );
    }
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
      let allocator = Allocator::default();
      test_transform(
        &allocator,
        JsxPrecompile::new(&allocator, None, None, None),
        format!("const a = <label {}=\"foo\">label</label>", &mapping.0)
          .as_str(),
        format!(
          "{}\nconst $$_tpl_1 = [\n\t'<label {}=\"foo\">label</label>'\n];\nconst a = _jsxTemplate($$_tpl_1);",
          "import { jsxTemplate as _jsxTemplate } from \"react/jsx-runtime\";",
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
      let allocator = Allocator::default();
      test_transform(
        &allocator,
        JsxPrecompile::new(&allocator, None, None, None),
        format!("const a = <label {}=\"foo\" {{...foo}} />", &mapping.0)
          .as_str(),
        format!(
          "{}\nconst a = _jsx(\"label\", {{\n\t{}: \"foo\",\n\t...foo\n}});",
          "import { jsx as _jsx } from \"react/jsx-runtime\";", quoted
        )
        .as_str(),
      );
    }

    // Component props should never be normalized
    for mapping in mappings.iter() {
      let allocator = Allocator::default();
      test_transform(
        &allocator,
        JsxPrecompile::new(&allocator, None, None, None),
        format!("const a = <Foo {}=\"foo\">foo</Foo>", &mapping.0).as_str(),
        format!(
          "{}\nconst a = _jsx(Foo, {{\n\t{}: \"foo\",\n\tchildren: \"foo\"\n}});",
          "import { jsx as _jsx } from \"react/jsx-runtime\";",
          &mapping.0
        )
        .as_str(),
      );
    }
  }
}
