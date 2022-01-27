// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::collections::HashSet;

use crate::swc::ast::*;
use crate::swc::visit::Visit;
use crate::ParsedSource;
use anyhow::bail;
use anyhow::Result;
use swc_atoms::JsWord;
use swc_ecmascript::visit::VisitWith;

#[derive(Debug, Clone)]
pub struct CjsAnalysis {
  pub exports: Vec<String>,
  pub reexports: Vec<String>,
}

pub fn analyze_cjs(script: &ParsedSource) -> Result<CjsAnalysis> {
  if !matches!(script.program_ref(), Program::Script(_)) {
    bail!("Cannot cjs analyze non-script: {}", script.specifier())
  }
  let mut visitor = CjsVisitor::default();
  visitor.visit_script(&script.script());

  Ok(visitor.take_result())
}

#[derive(Default)]
struct CjsVisitor {
  exports: HashSet<String>,
  reexports: HashSet<String>,
  unsafe_getters: HashSet<String>,
  var_assignments: HashMap<String, String>,
}

impl CjsVisitor {
  fn take_result(self) -> CjsAnalysis {
    let unsafe_getters = self.unsafe_getters;
    let mut exports = self
      .exports
      .into_iter()
      .filter(|n| !unsafe_getters.contains(n))
      .collect::<Vec<_>>();
    exports.sort();
    let mut reexports = self
      .reexports
      .into_iter()
      .filter(|n| !unsafe_getters.contains(n))
      .collect::<Vec<_>>();
    reexports.sort();
    CjsAnalysis { exports, reexports }
  }

  fn add_export(&mut self, export: &str) {
    self.exports.insert(export.to_string());
  }

  fn add_reexport(&mut self, reexport: &str) {
    self.reexports.insert(reexport.to_string());
  }

  fn set_reexport_assignment_value(&mut self, value: &str) {
    self.reexports.clear();
    self.add_reexport(value);
  }

  fn add_unsafe_getter(&mut self, name: &str) {
    self.unsafe_getters.insert(name.to_string());
  }

  /// This will take `_external003[key]` and the symbol for `key` and return "external003"
  /// ```js
  /// var _external003 = require("external003");
  /// Object.keys(_external003).forEach(function (key) {
  ///   exports[key] = _external003[key];
  /// });
  /// ```
  fn get_member_require_value(
    &self,
    member: &MemberExpr,
    key: &JsWord,
  ) -> Option<String> {
    let obj_ident = member.obj.as_ident()?;
    let require_value = self.var_assignments.get(&*obj_ident.sym).cloned()?;
    let prop_ident = member.prop.as_computed()?.expr.as_ident()?;
    if &prop_ident.sym == key {
      Some(require_value)
    } else {
      None
    }
  }
}

impl Visit for CjsVisitor {
  fn visit_var_decl(&mut self, stmt: &VarDecl) {
    for decl in &stmt.decls {
      if let Some(id) = decl.name.as_ident() {
        if let Some(init) = &decl.init {
          if let Some(require_value) = get_expr_require_value(init) {
            self
              .var_assignments
              .insert(id.id.sym.to_string(), require_value.to_string());
          }
        }
      }
    }
  }

  fn visit_call_expr(&mut self, call_expr: &CallExpr) {
    // Object.defineProperty(exports, ..., { ... });
    if is_object_define_callee(&call_expr.callee)
      && call_expr.args.len() >= 3
      && is_module_exports_or_exports(&call_expr.args[0].expr)
      && call_expr.args[0].spread.is_none()
      && call_expr.args[1].spread.is_none()
      && call_expr.args[2].spread.is_none()
    {
      match &*call_expr.args[1].expr {
        // Object.defineProperty(exports, "someExport", { ... });
        Expr::Lit(Lit::Str(str)) => {
          let lit_value = &*str.value;
          if is_valid_object_define_obj_lit(&call_expr.args[2].expr) {
            self.add_export(lit_value);
          } else {
            self.add_unsafe_getter(lit_value);
          }
        }
        // Object.defineProperty(exports, key, { ... });
        Expr::Ident(object_define_prop_ident) => {
          if let Some(obj_lit) = call_expr.args[2].expr.as_object() {
            if let Some(get_prop) = get_object_define_get_prop(obj_lit) {
              // get: function () {
              //   return _external002[key];
              // }
              if let Some(Expr::Member(member)) =
                get_prop_return_expr(&get_prop)
              {
                if let Some(require_value) = self.get_member_require_value(
                  member,
                  &object_define_prop_ident.sym,
                ) {
                  self.add_reexport(&require_value);
                }
              }
            }
          }
        }
        _ => {}
      }
    }

    // __export, __exportStar, tslib.__export, etc...
    if call_expr.args.len() == 1
      && call_expr.args[0].spread.is_none()
      && is_export_callee(&call_expr.callee)
    {
      if let Some(name) = get_expr_require_value(&call_expr.args[0].expr) {
        self.add_reexport(name);
      }
    }

    call_expr.visit_children_with(self);

    fn is_object_define_callee(callee: &Callee) -> bool {
      let member = match get_callee_member_expr(callee) {
        Some(member) => member,
        None => return false,
      };
      get_expr_ident_text(&member.obj) == Some("Object")
        && get_member_prop_ident_text(&member.prop) == Some("defineProperty")
    }

    fn is_export_callee(callee: &Callee) -> bool {
      if let Some(member) = get_callee_member_expr(callee) {
        // ex. tslib.__exportStar
        if matches!(&*member.obj, Expr::Ident(_)) {
          if let Some(right_side) = get_member_prop_ident_text(&member.prop) {
            return matches!(right_side, "__exportStar" | "__export");
          }
        }
      } else if let Some(ident) = get_callee_ident(callee) {
        return matches!(&*ident.sym, "__exportStar" | "__export");
      }
      false
    }
  }

  fn visit_assign_expr(&mut self, assign_expr: &AssignExpr) {
    if assign_expr.op != AssignOp::Assign {
      return;
    }

    let left_expr =
      match assign_expr.left.as_pat().and_then(|pat| pat.as_expr()) {
        Some(expr) => expr,
        _ => return,
      };

    // check if left hand side is "module.exports = " or "exports ="
    if is_module_exports_or_exports(left_expr) {
      match &*assign_expr.right {
        Expr::Object(object_lit) => {
          for prop in &object_lit.props {
            match prop {
              PropOrSpread::Prop(prop) => {
                if let Some(prop_name) = get_prop_name(prop) {
                  if let Some(require_value) = get_prop_require_value(prop) {
                    self.add_reexport(require_value);
                  } else {
                    if is_supported_object_prop(prop) {
                      self.add_export(prop_name);
                    } else {
                      self.add_unsafe_getter(prop_name);
                    }
                  }
                }
              }
              PropOrSpread::Spread(spread) => {
                if let Some(require_value) =
                  get_expr_require_value(&spread.expr)
                {
                  self.add_reexport(require_value);
                }
              }
            }
          }
        }
        Expr::Call(call_expr) => {
          // module.exports = require(...);
          if let Some(require_value) = get_call_expr_require_value(call_expr) {
            self.set_reexport_assignment_value(require_value);
          }
        }
        _ => {}
      };
    } else if let Some(left_member) = left_expr.as_member() {
      if is_module_exports_or_exports(&*left_member.obj) {
        // check for:
        // * `exports["something"] = other`
        // * `exports.something = other`
        if let Some(prop_name) = get_member_prop_text(&left_member.prop) {
          if let Some(require_value) =
            get_expr_require_value(&*assign_expr.right)
          {
            self.add_reexport(require_value);
          } else {
            self.add_export(prop_name);
          }
        } else if let Some(right_member) = assign_expr.right.as_member() {
          // check for:
          // * `exports[key] = _something[key];
          if let MemberProp::Computed(computed) = &left_member.prop {
            if let Some(computed_ident) = computed.expr.as_ident() {
              if let Some(require_value) =
                self.get_member_require_value(right_member, &computed_ident.sym)
              {
                self.add_reexport(&require_value);
              }
            }
          }
        }
      }
    }
  }
}

fn is_module_exports_or_exports(expr: &Expr) -> bool {
  is_module_exports_expr(expr) || is_exports_expr(expr)
}

fn is_module_exports_expr(expr: &Expr) -> bool {
  if let Some(member_expr) = expr.as_member() {
    if let Some(obj_ident) = member_expr.obj.as_ident() {
      if obj_ident.sym == *"module" {
        return get_member_prop_text(&member_expr.prop) == Some("exports");
      }
    }
  }
  false
}

fn is_exports_expr(expr: &Expr) -> bool {
  expr
    .as_ident()
    .map(|i| &i.sym == "exports")
    .unwrap_or(false)
}

fn get_prop_require_value(prop: &Prop) -> Option<&str> {
  match prop {
    Prop::KeyValue(prop) => get_expr_require_value(&*prop.value),
    _ => None,
  }
}

fn get_expr_require_value(expr: &Expr) -> Option<&str> {
  match expr {
    Expr::Call(call_expr) => get_call_expr_require_value(call_expr),
    _ => None,
  }
}

fn get_call_expr_require_value(call_expr: &CallExpr) -> Option<&str> {
  let callee_expr = call_expr.callee.as_expr()?;
  let ident = callee_expr.as_ident()?;
  match &*ident.sym {
    "require" => {
      let arg = call_expr.args.get(0)?;
      let lit = arg.expr.as_lit()?;
      if let Lit::Str(str) = lit {
        return Some(&*str.value);
      }
    }
    // _interopRequireWildcard(require(...))
    "_interopRequireWildcard" => {
      let arg = call_expr.args.get(0)?;
      let call_expr = arg.expr.as_call()?;
      return get_call_expr_require_value(call_expr);
    }
    _ => {}
  }
  None
}

fn get_callee_member_expr(callee: &Callee) -> Option<&MemberExpr> {
  if let Callee::Expr(expr) = callee {
    if let Expr::Member(member) = &**expr {
      return Some(member);
    }
  }
  None
}

fn get_callee_ident(callee: &Callee) -> Option<&Ident> {
  if let Callee::Expr(expr) = callee {
    if let Expr::Ident(ident) = &**expr {
      return Some(ident);
    }
  }
  None
}

fn is_valid_object_define_obj_lit(expr: &Expr) -> bool {
  let obj = match expr.as_object() {
    Some(obj) => obj,
    None => return false,
  };

  has_supported_get_prop(obj)
}

// todo: extract out and unit test
fn has_supported_get_prop(obj: &ObjectLit) -> bool {
  let get_prop = match get_object_define_get_prop(obj) {
    Some(prop) => prop,
    None => return true,
  };
  is_supported_get_prop(get_prop)
}

fn get_object_define_get_prop(obj: &ObjectLit) -> Option<&Prop> {
  obj
    .props
    .iter()
    .filter_map(|p| {
      if let PropOrSpread::Prop(prop) = p {
        if get_prop_name(&prop) == Some("get") {
          return Some(prop);
        }
      }
      None
    })
    .next()
    .map(|p| &**p)
}

fn is_supported_object_prop(prop: &Prop) -> bool {
  match prop {
    Prop::Method(_) | Prop::KeyValue(_) | Prop::Shorthand(_) => true,
    Prop::Getter(getter) => match &getter.body {
      Some(stmt) => is_non_side_effect_block_stmt(stmt),
      _ => false,
    },
    Prop::Assign(_) | Prop::Setter(_) => false,
  }
}

fn is_supported_get_prop(prop: &Prop) -> bool {
  match prop {
    Prop::Method(method) => is_non_side_effect_function(&method.function),
    Prop::KeyValue(key_value) => match &*key_value.value {
      Expr::Fn(expr) => is_non_side_effect_function(&expr.function),
      Expr::Arrow(expr) => is_non_side_effect_block_stmt_or_expr(&expr.body),
      _ => false,
    },
    _ => false,
  }
}

fn get_prop_return_expr(prop: &Prop) -> Option<&Expr> {
  match prop {
    Prop::Method(method) => get_function_return_expr(&method.function),
    Prop::KeyValue(key_value) => match &*key_value.value {
      Expr::Fn(expr) => get_function_return_expr(&expr.function),
      Expr::Arrow(expr) => match &expr.body {
        BlockStmtOrExpr::BlockStmt(stmt) => get_block_stmt_return_expr(stmt),
        BlockStmtOrExpr::Expr(expr) => Some(expr),
      },
      _ => None,
    },
    _ => None,
  }
}

fn is_non_side_effect_function(function: &Function) -> bool {
  function
    .body
    .as_ref()
    .map(|b| is_non_side_effect_block_stmt(b))
    .unwrap_or(false)
}

fn is_non_side_effect_block_stmt_or_expr(node: &BlockStmtOrExpr) -> bool {
  match node {
    BlockStmtOrExpr::BlockStmt(stmt) => is_non_side_effect_block_stmt(stmt),
    BlockStmtOrExpr::Expr(expr) => is_non_side_effect_expr(expr),
  }
}

fn is_non_side_effect_block_stmt(stmt: &BlockStmt) -> bool {
  match get_block_stmt_return_expr(stmt) {
    Some(expr) => is_non_side_effect_expr(expr),
    None => false,
  }
}

fn get_function_return_expr(function: &Function) -> Option<&Expr> {
  function
    .body
    .as_ref()
    .map(|b| get_block_stmt_return_expr(b))
    .flatten()
}

fn get_block_stmt_return_expr(stmt: &BlockStmt) -> Option<&Expr> {
  if stmt.stmts.len() > 1 || stmt.stmts.is_empty() {
    return None;
  }
  match stmt.stmts.get(0)? {
    Stmt::Return(stmt) => match &stmt.arg {
      Some(expr) => Some(&expr),
      _ => None,
    },
    _ => None,
  }
}

fn is_non_side_effect_expr(expr: &Expr) -> bool {
  match expr {
    Expr::Member(member) => {
      is_non_side_effect_expr(&member.obj)
        && is_non_side_effect_member_prop(&member.prop)
    }
    Expr::Ident(_) | Expr::Lit(_) | Expr::Object(_) => true,
    _ => false,
  }
}

fn is_non_side_effect_member_prop(member_prop: &MemberProp) -> bool {
  match member_prop {
    MemberProp::PrivateName(_) | MemberProp::Ident(_) => true,
    MemberProp::Computed(computed) => is_non_side_effect_expr(&computed.expr),
  }
}

fn get_expr_ident_text<'a>(expr: &'a Expr) -> Option<&str> {
  match expr {
    Expr::Ident(ident) => Some(&ident.sym),
    _ => None,
  }
}

fn get_member_prop_ident_text(member_prop: &MemberProp) -> Option<&str> {
  match member_prop {
    MemberProp::Ident(ident) => Some(&ident.sym),
    _ => None,
  }
}

fn get_member_prop_text(member_prop: &MemberProp) -> Option<&str> {
  match member_prop {
    MemberProp::Ident(ident) => Some(&ident.sym),
    MemberProp::Computed(computed) => match &*computed.expr {
      Expr::Lit(Lit::Str(str)) => Some(&str.value),
      _ => None,
    },
    _ => None,
  }
}

fn get_prop_name(prop: &Prop) -> Option<&str> {
  match prop {
    Prop::KeyValue(prop) => prop_name_from_key(&prop.key),
    Prop::Shorthand(ident) => Some(&*ident.sym),
    Prop::Getter(prop) => prop_name_from_key(&prop.key),
    Prop::Setter(prop) => prop_name_from_key(&prop.key),
    Prop::Method(prop) => prop_name_from_key(&prop.key),
    // invalid for object literal, so ignore
    Prop::Assign(_) => None,
  }
}

fn prop_name_from_key(prop: &PropName) -> Option<&str> {
  match prop {
    PropName::Ident(ident) => Some(&*ident.sym),
    PropName::Str(str) => Some(&*str.value),
    PropName::BigInt(_) | PropName::Computed(_) | PropName::Num(_) => None,
  }
}

#[cfg(test)]
mod test {
  use pretty_assertions::assert_eq;
  use std::cell::RefCell;

  use crate::parse_script;
  use crate::MediaType;
  use crate::ParseParams;
  use crate::SourceTextInfo;

  use super::*;

  struct CjsAnalysisTester {
    analysis: RefCell<CjsAnalysis>,
  }

  impl CjsAnalysisTester {
    pub fn assert_exports(&self, mut values: Vec<&str>) {
      values.sort();
      let mut analysis = self.analysis.borrow_mut();
      assert_eq!(analysis.exports, values);
      analysis.exports.clear();
    }

    pub fn assert_reexports(&self, mut values: Vec<&str>) {
      values.sort();
      let mut analysis = self.analysis.borrow_mut();
      assert_eq!(analysis.reexports, values);
      analysis.reexports.clear();
    }

    pub fn assert_empty(&self) {
      let analysis = self.analysis.borrow();
      if !analysis.exports.is_empty() {
        panic!("Had exports: {}", analysis.exports.join(", "))
      }
      if !analysis.reexports.is_empty() {
        panic!("Had reexports: {}", analysis.reexports.join(", "))
      }
    }
  }

  impl Drop for CjsAnalysisTester {
    fn drop(&mut self) {
      if !std::thread::panicking() {
        self.assert_empty();
      }
    }
  }

  fn parse_cjs(source: &str) -> CjsAnalysisTester {
    let parsed_source = parse_script(ParseParams {
      specifier: "".to_string(),
      source: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Cjs,
      capture_tokens: true,
      scope_analysis: false,
      maybe_syntax: None,
    })
    .unwrap();
    let analysis = analyze_cjs(&parsed_source).unwrap();
    CjsAnalysisTester {
      analysis: RefCell::new(analysis),
    }
  }

  // Tests ported from https://github.com/nodejs/cjs-module-lexer/blob/main/test/_unit.js

  #[test]
  fn esbuild_hint_style() {
    let tester = parse_cjs(
      "0 && (module.exports = {a, b, c}) && __exportStar(require('fs'));",
    );

    tester.assert_exports(vec!["a", "b", "c"]);
    tester.assert_reexports(vec!["fs"]);
  }

  #[test]
  fn getter_opt_outs() {
    let tester = parse_cjs(
      r#"
    Object.defineProperty(exports, 'a', {
        enumerable: true,
        get: function () {
          return q.p;
        }
      });
      if (false) {
        Object.defineProperty(exports, 'a', {
          enumerable: false,
          get: function () {
            return dynamic();
          }
        });
      }"#,
    );

    tester.assert_empty();
  }

  #[test]
  fn typescript_reexports() {
    let tester = parse_cjs(
      r#"
    "use strict";
      function __export(m) {
          for (var p in m) if (!exports.hasOwnProperty(p)) exports[p] = m[p];
      }
      Object.defineProperty(exports, "__esModule", { value: true });
      __export(require("external1"));
      tslib.__export(require("external2"));
      __exportStar(require("external3"));
      tslib1.__exportStar(require("external4"));
      "use strict";
      Object.defineProperty(exports, "__esModule", { value: true });
      var color_factory_1 = require("./color-factory");
      Object.defineProperty(exports, "colorFactory", { enumerable: true, get: function () { return color_factory_1.colorFactory; }, });
    "#,
    );

    tester.assert_exports(vec!["__esModule", "colorFactory"]);
    tester.assert_reexports(vec![
      "external1",
      "external2",
      "external3",
      "external4",
    ]);
  }

  #[test]
  fn rollup_babel_reexport_getter() {
    let tester = parse_cjs(
      r#"
      Object.defineProperty(exports, 'a', {
        enumerable: true,
        get: function () {
          return q.p;
        }
      });
      Object.defineProperty(exports, 'b', {
        enumerable: false,
        get: function () {
          return q.p;
        }
      });
      Object.defineProperty(exports, "c", {
        get: function get () {
          return q['p' ];
        }
      });
      Object.defineProperty(exports, 'd', {
        get: function () {
          return __ns.val;
        }
      });
      Object.defineProperty(exports, 'e', {
        get () {
          return external;
        }
      });
      Object.defineProperty(exports, "f", {
        get: function get () {
          return q['p' ];
        }
      });
    "#,
    );

    tester.assert_exports(vec![
      "a", "c", "d", "e",
      "f", // changed code from cjs-module-lexer tests so it parses
      // added support -- why does cjs-module-lexer match c and d and not b?
      "b",
    ]);
  }

  #[test]
  fn rollup_babel_reexports() {
    let tester = parse_cjs(
      r#"
    "use strict";
      exports.__esModule = true;
      not.detect = require("ignored");
      var _external = require("external");
      // Babel <7.12.0, loose mode
      Object.keys(_external).forEach(function (key) {
        if (key === "default" || key === "__esModule") return;
        exports[key] = _external[key];
      });
      var _external2 = require("external2");
      // Babel <7.12.0
      Object.keys(_external2).forEach(function (key) {
        if (key === "default" || /*comment!*/ key === "__esModule") return;
        Object.defineProperty(exports, key, {
          enumerable: true,
          get: function () {
            return _external2[key];
          }
        });
      });
      var _external001 = require("external001");
      // Babel >=7.12.0, loose mode
      Object.keys(_external001).forEach(function (key) {
        if (key === "default" || key === "__esModule") return;
        if (key in exports && exports[key] === _external001[key]) return;
        exports[key] = _external001[key];
      });
      var _external003 = require("external003");
      // Babel >=7.12.0, loose mode, reexports conflicts filter
      Object.keys(_external003).forEach(function (key) {
        if (key === "default" || key === "__esModule") return;
        if (Object.prototype.hasOwnProperty.call(_exportNames, key)) return;
        if (key in exports && exports[key] === _external003[key]) return;
        exports[key] = _external003[key];
      });
      var _external002 = require("external002");
      // Babel >=7.12.0
      Object.keys(_external002).forEach(function (key) {
        if (key === "default" || key === "__esModule") return;
        if (key in exports && exports[key] === _external002[key]) return;
        Object.defineProperty(exports, key, {
          enumerable: true,
          get: function () {
            return _external002[key];
          }
        });
      });
      var _external004 = require("external004");
      // Babel >=7.12.0, reexports conflict filter
      Object.keys(_external004).forEach(function (key) {
        if (key === "default" || key === "__esModule") return;
        if (Object.prototype.hasOwnProperty.call(_exportNames, key)) return;
        if (key in exports && exports[key] === _external004[key]) return;
        Object.defineProperty(exports, key, {
          enumerable: true,
          get: function () {
            return _external004[key];
          }
        });
      });
      let external3 = require('external3');
      const external4 = require('external4');
      Object.keys(external3).forEach(function (k) {
        if (k !== 'default') Object.defineProperty(exports, k, {
          enumerable: true,
          get: function () {
            return external3[k];
          }
        });
      });
      Object.keys(external4).forEach(function (k) {
        if (k !== 'default') exports[k] = external4[k];
      });
      const externalǽ = require('external😃');
      Object.keys(externalǽ).forEach(function (k) {
        if (k !== 'default') exports[k] = externalǽ[k];
      });
      let external5 = require('e5');
      let external6 = require('e6');
      Object.keys(external5).forEach(function (k) {
        if (k !== 'default' && !Object.hasOwnProperty.call(exports, k)) exports[k] = external5[k];
      });
      const not = require('not');
      Object.keys(not).forEach(function (k) {
        if (k !== 'default' && !a().hasOwnProperty(k)) exports[k] = not[k];
      });
      Object.keys(external6).forEach(function (k) {
        if (k !== 'default' && !exports.hasOwnProperty(k)) exports[k] = external6[k];
      });
      const external𤭢 = require('external𤭢');
      Object.keys(external𤭢).forEach(function (k) {
        if (k !== 'default') exports[k] = external𤭢[k];
      });
      const notexternal1 = require('notexternal1');
      Object.keys(notexternal1);
      const notexternal2 = require('notexternal2');
      Object.keys(notexternal2).each(function(){
      });
      const notexternal3 = require('notexternal3');
      Object.keys(notexternal2).forEach(function () {
      });
      const notexternal4 = require('notexternal4');
      Object.keys(notexternal2).forEach(function (x) {
      });
      const notexternal5 = require('notexternal5');
      Object.keys(notexternal5).forEach(function (x) {
        if (true);
      });
      const notexternal6 = require('notexternal6');
      Object.keys(notexternal6).forEach(function (x) {
        if (x);
      });
      const notexternal7 = require('notexternal7');
      Object.keys(notexternal7).forEach(function(x){
        if (x ==='default');
      });
      const notexternal8 = require('notexternal8');
      Object.keys(notexternal8).forEach(function(x){
        if (x ==='default'||y);
      });
      const notexternal9 = require('notexternal9');
      Object.keys(notexternal9).forEach(function(x){
        if (x ==='default'||x==='__esM');
      });
      const notexternal10 = require('notexternal10');
      Object.keys(notexternal10).forEach(function(x){
        if (x !=='default') return
      });
      const notexternal11 = require('notexternal11');
      Object.keys(notexternal11).forEach(function(x){
        if (x ==='default'||x==='__esModule') return
      });
      const notexternal12 = require('notexternal12');
      Object.keys(notexternal12).forEach(function(x){
        if (x ==='default'||x==='__esModule') return
        exports[y] = notexternal12[y];
      });
      const notexternal13 = require('notexternal13');
      Object.keys(notexternal13).forEach(function(x){
        if (x ==='default'||x==='__esModule') return
        exports[y] = notexternal13[y];
      });
      const notexternal14 = require('notexternal14');
      Object.keys(notexternal14).forEach(function(x){
        if (x ==='default'||x==='__esModule') return
        Object.defineProperty(exports, k, {
          enumerable: false,
          get: function () {
            return external14[k];
          }
        });
      });
      const notexternal15 = require('notexternal15');
      Object.keys(notexternal15).forEach(function(x){
        if (x ==='default'||x==='__esModule') return
        Object.defineProperty(exports, k, {
          enumerable: false,
          get: function () {
            return externalnone[k];
          }
        });
      });
      const notexternal16 = require('notexternal16');
      Object.keys(notexternal16).forEach(function(x){
        if (x ==='default'||x==='__esModule') return
        exports[x] = notexternal16[x];
        extra;
      });
      {
        const notexternal17 = require('notexternal17');
        Object.keys(notexternal17).forEach(function(x){
          if (x ==='default'||x==='__esModule') return
          exports[x] = notexternal17[x];
        });
      }
      var _styles = require("./styles");
      Object.keys(_styles).forEach(function (key) {
        if (key === "default" || key === "__esModule") return;
        if (Object.prototype.hasOwnProperty.call(_exportNames, key)) return;
        Object.defineProperty(exports, key, {
          enumerable: true,
          get: function get() {
            return _styles[key];
          }
        });
      });
      var _styles2 = require("./styles2");
      Object.keys(_styles2).forEach(function (key) {
        if (key === "default" || key === "__esModule") return;
        if (Object.prototype.hasOwnProperty.call(_exportNames, key)) return;
        Object.defineProperty(exports, key, {
          enumerable: true,
          get () {
            return _styles2[key];
          }
        });
      });
      var _Accordion = _interopRequireWildcard(require("./Accordion"));
      Object.keys(_Accordion).forEach(function (key) {
        if (key === "default" || key === "__esModule") return;
        if (Object.prototype.hasOwnProperty.call(_exportNames, key)) return;
        Object.defineProperty(exports, key, {
          enumerable: true,
          get: function () {
            return _Accordion[key];
          }
        });
      });
    "#,
    );

    tester.assert_exports(vec!["__esModule"]);
    tester.assert_reexports(vec![
      "external",
      "external2",
      "external001",
      "external003",
      "external002",
      "external004",
      "external3",
      "external4",
      "external😃",
      "e5",
      "e6",
      "external𤭢",
      "./styles",
      "./styles2",
      "./Accordion",
      // extra values not matched in cjs-module-lexer
      "not",           // why not?
      "notexternal12", // seems like an unrealistic test?
      "notexternal13", // seems like an unrealistic test?
      "notexternal16", // might as well match
      "notexternal17", // might as well match
    ]);
  }

  #[test]
  fn module_exports_reexport_spread() {
    let tester = parse_cjs(
      r#"
    module.exports = {
        ...a,
        ...b,
        ...require('dep1'),
        c: d,
        ...require('dep2'),
        name
      };
    "#,
    );

    tester.assert_exports(vec!["c", "name"]);
    tester.assert_reexports(vec!["dep1", "dep2"]);
  }

  #[test]
  fn shebang() {
    parse_cjs(r#"#!"#);

    let tester = parse_cjs(
      r#"#! (  {
        exports.asdf = 'asdf';
      "#,
    );

    tester.assert_exports(vec!["asdf"]);
  }

  #[test]
  fn non_identifiers() {
    let tester = parse_cjs(
      r#"
      module.exports = { 'ab cd': foo };
      exports['not identifier'] = 'asdf';
      exports['\''] = 1;
      exports['@notidentifier'] = 'asdf';
      Object.defineProperty(exports, "%notidentifier", { value: x });
      Object.defineProperty(exports, 'hm🤔', { value: x });
      exports['⨉'] = 45;
      exports['α'] = 54;
      exports.package = 'STRICT RESERVED!';
      exports.var = 'RESERVED';
    "#,
    );

    tester.assert_exports(vec![
      "%notidentifier",
      "ab cd",
      "not identifier",
      "\'",
      "@notidentifier",
      "hm🤔",
      "⨉",
      "α",
      "package",
      "var",
    ]);
  }

  #[test]
  fn literal_exports() {
    let tester = parse_cjs(r#"module.exports = { a, b: c, d, 'e': f };"#);

    tester.assert_exports(vec!["a", "b", "d", "e"]);
  }

  #[test]
  fn literal_exports_example() {
    let tester = parse_cjs(
      r#"
      module.exports = {
        a: a,
        b: b,
        // cjs-module-lexer has "e" as an export and then bails
        // on the object literal because it encounters require
        e: require('d'),
        f: 'f'
      }
    "#,
    );

    tester.assert_exports(vec!["a", "b", "f"]);
    tester.assert_reexports(vec!["d"]);
  }

  #[test]
  fn literal_exports_complex() {
    let tester = parse_cjs(
      r#"
      function defineProp(name, value) {
        delete module.exports[name];
        module.exports[name] = value;
        return value;
      }

      module.exports = {
        Parser: Parser,
        Tokenizer: require("./Tokenizer.js"),
        ElementType: require("domelementtype"),
        DomHandler: DomHandler,
        get FeedHandler() {
            return defineProp("FeedHandler", require("./FeedHandler.js"));
        },
        get Stream() {
            return defineProp("Stream", require("./Stream.js"));
        },
        get WritableStream() {
            return defineProp("WritableStream", require("./WritableStream.js"));
        },
        get ProxyHandler() {
            return defineProp("ProxyHandler", require("./ProxyHandler.js"));
        },
        get DomUtils() {
            return defineProp("DomUtils", require("domutils"));
        },
        get CollectingHandler() {
            return defineProp(
                "CollectingHandler",
                require("./CollectingHandler.js")
            );
        },
        // For legacy support
        DefaultHandler: DomHandler,
        get RssHandler() {
            return defineProp("RssHandler", this.FeedHandler);
        },
        //helper methods
        parseDOM: function(data, options) {
            var handler = new DomHandler(options);
            new Parser(handler, options).end(data);
            return handler.dom;
        },
        parseFeed: function(feed, options) {
            var handler = new module.exports.FeedHandler(options);
            new Parser(handler, options).end(feed);
            return handler.dom;
        },
        createDomStream: function(cb, options, elementCb) {
            var handler = new DomHandler(cb, options, elementCb);
            return new Parser(handler, options);
        },
        // List of all events that the parser emits
        EVENTS: {
            /* Format: eventname: number of arguments */
            attribute: 2,
            cdatastart: 0,
            cdataend: 0,
            text: 1,
            processinginstruction: 2,
            comment: 1,
            commentend: 0,
            closetag: 1,
            opentag: 2,
            opentagname: 1,
            error: 1,
            end: 0
        }
      };
    "#,
    );

    #[rustfmt::skip]
    tester.assert_exports(
      vec![
        "Parser",
        // added support for below - cjs-module-lexer bails early for object literals, but we can understand this
        "DefaultHandler",
        "DomHandler",
        "EVENTS",
        "createDomStream",
        "parseDOM",
        "parseFeed",
        // now classified as reexport, but exports in cjs-module-lexer
        // "Tokenizer",
      ]
    );
    tester.assert_reexports(vec![
      // added support for below
      "./Tokenizer.js",
      "domelementtype",
    ]);
  }

  #[test]
  fn define_property_value() {
    let tester = parse_cjs(
      r#"
      Object.defineProperty(exports, 'namedExport', { enumerable: false, value: true });
      Object.defineProperty(exports, 'namedExport', { configurable: false, value: true });
      Object.defineProperty(exports, 'a', {
        enumerable: false,
        get () {
          return p;
        }
      });
      Object.defineProperty(exports, 'b', {
        configurable: true,
        get () {
          return p;
        }
      });
      Object.defineProperty(exports, 'c', {
        get: () => p
      });
      Object.defineProperty(exports, 'd', {
        enumerable: true,
        get: function () {
          return dynamic();
        }
      });
      Object.defineProperty(exports, 'e', {
        enumerable: true,
        get () {
          return 'str';
        }
      });
      Object.defineProperty(module.exports, 'thing', { value: true });
      Object.defineProperty(exports, "other", { enumerable: true, value: true });
      Object.defineProperty(exports, "__esModule", { value: true });
    "#,
    );

    tester.assert_exports(vec![
      "__esModule",
      "other",
      "thing",
      // added support for below
      "a",
      "b",
      "c",
      "e",
      "namedExport",
    ]);
  }

  #[test]
  fn module_assign() {
    let tester = parse_cjs(
      r#"
      module.exports.asdf = 'asdf';
      exports = 'asdf';
      module.exports = require('./asdf');
      if (maybe)
        module.exports = require("./another");
    "#,
    );

    tester.assert_exports(vec!["asdf"]);
    tester.assert_reexports(vec!["./another"]);
  }

  #[test]
  fn template_string_expression_ambiguity() {
    let tester = parse_cjs(
      r#"
      `$`
      import('a');
      ``
      exports.a = 'a';
      `a$b`
      exports['b'] = 'b';
      `{$}`
      exports['b'].b;
    "#,
    );

    tester.assert_exports(vec!["a", "b"]);
  }
}
