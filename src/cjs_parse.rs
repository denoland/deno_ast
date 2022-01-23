// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::parsing::parse_script;
use crate::Diagnostic;
use crate::MediaType;
use crate::ParseParams;
use crate::SourceTextInfo;
use swc_ecmascript::ast;
use swc_ecmascript::utils::ExprExt;
use swc_ecmascript::visit::{noop_visit_type, Visit, VisitWith};

#[derive(Debug, Default)]
struct CjsAnalysis {
  exports: Vec<String>,
  reexports: Vec<String>,
}

fn parse_cjs(source: &str) -> Result<CjsAnalysis, Diagnostic> {
  let parsed_source = parse_script(ParseParams {
    specifier: "".to_string(),
    source: SourceTextInfo::from_string(source.to_string()),
    media_type: MediaType::Cjs,
    capture_tokens: true,
    scope_analysis: false,
    maybe_syntax: None,
  })?;

  let mut visitor = CjsVisitor {
    analysis: CjsAnalysis::default(),
  };
  visitor.visit_program(&parsed_source.program());

  // parsed_source.with_view(|program| {
  // ..
  // });

  Ok(visitor.analysis)
}

struct CjsVisitor {
  analysis: CjsAnalysis,
}

impl CjsVisitor {
  fn add_export(&mut self, export: &str) {
    // TODO(bartlomieju): should be a HashSet?
    self.analysis.exports.push(export.to_string());
  }

  fn add_reexport(&mut self, reexport: &str) {
    // TODO(bartlomieju): should be a HashSet?
    self.analysis.reexports.push(reexport.to_string());
  }

  fn is_module_exports_or_exports(&self, expr: &ast::Expr) -> bool {
    if let Some(member_expr) = expr.as_member() {
      if let Some(ident) = member_expr.obj.as_ident() {
        if ident.sym == *"module" {
          if let Some(prop_ident) = member_expr.prop.as_ident() {
            if prop_ident.sym == *"exports" {
              return true;
            }
          }
        }
      }
    } else if let Some(ident) = expr.as_ident() {
      if ident.sym == *"exports" {
        return true;
      }
    }

    false
  }
}

impl Visit for CjsVisitor {
  fn visit_assign_expr(&mut self, assign_expr: &ast::AssignExpr) {
    // TODO(bartlomieju): use `if_chain` crate
    // check if left hand side is "module.exports = " or "exports ="
    eprintln!("assign_expr {:#?}", assign_expr);
    if assign_expr.op == ast::AssignOp::Assign {
      if let Some(pat) = assign_expr.left.as_pat() {
        if let Some(inner_expr) = pat.as_expr() {
          if self.is_module_exports_or_exports(inner_expr) {
            eprintln!("is_module_export_or_exports true");
            match &*assign_expr.right {
              ast::Expr::Object(object_lit) => {
                for prop in &object_lit.props {
                  if let Some(prop) = prop.as_prop() {
                    if let Some(assign_prop) = prop.as_assign() {
                      self.add_export(&assign_prop.key.sym);
                    }
                  }
                }
              }
              ast::Expr::Call(call_expr) => {
                if let Some(callee_expr) = call_expr.callee.as_expr() {
                  if let Some(ident) = callee_expr.as_ident() {
                    if ident.sym == *"require" {
                      if let Some(arg) = call_expr.args.get(0) {
                        if let Some(lit) = arg.expr.as_lit() {
                          if let ast::Lit::Str(str_) = lit {
                            self.add_reexport(&str_.value);
                          }
                        }
                      }
                    }
                  }
                }
              }
              _ => todo!(),
            };
          } else if let Some(ident) = assign_expr.left.as_ident() {
            if ident.sym == *"exports" {
              todo!()
            }
          }
        }
      }
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  // Tests ported from https://github.com/nodejs/cjs-module-lexer/blob/main/test/_unit.js

  #[test]
  fn esbuild_hint_style() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
      "0 && (module.exports = {a, b, c}) && __exportStar(require('fs'));",
    )
    .unwrap();

    assert_eq!(exports.len(), 3);
    assert_eq!(exports[0], "a");
    assert_eq!(exports[1], "b");
    assert_eq!(exports[2], "c");
    assert_eq!(reexports.len(), 1);
    assert_eq!(reexports[0], "fs");
  }

  #[test]
  fn getter_opt_outs() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
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
    )
    .unwrap();

    assert_eq!(exports.len(), 0);
    assert_eq!(reexports.len(), 0);
  }

  #[test]
  fn typescript_reexports() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
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
    ).unwrap();

    assert_eq!(exports.len(), 2);
    assert_eq!(exports[0], "__esModule");
    assert_eq!(exports[1], "colorFactory");
    assert_eq!(reexports.len(), 4);
    assert_eq!(reexports[0], "external1");
    assert_eq!(reexports[1], "external2");
    assert_eq!(reexports[2], "external3");
    assert_eq!(reexports[3], "external4");
  }

  #[test]
  fn rollup_babel_reexport_getter() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
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
        get: functionget () {
          return q['p' ];
        }
      });
    "#,
    )
    .unwrap();

    assert_eq!(exports.len(), 4);
    assert_eq!(exports[0], "a");
    assert_eq!(exports[1], "c");
    assert_eq!(exports[2], "d");
    assert_eq!(exports[3], "e");
    assert_eq!(reexports.len(), 0);
  }

  #[test]
  fn rollup_babel_reexports() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
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
        export[y] = notexternal12[y];
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
    ).unwrap();

    assert_eq!(exports.len(), 1);
    assert_eq!(exports[0], "__esModule");
    assert_eq!(reexports.len(), 15);
    assert_eq!(reexports[0], "external");
    assert_eq!(reexports[1], "external2");
    assert_eq!(reexports[2], "external001");
    assert_eq!(reexports[3], "external003");
    assert_eq!(reexports[4], "external002");
    assert_eq!(reexports[5], "external004");
    assert_eq!(reexports[6], "external3");
    assert_eq!(reexports[7], "external4");
    assert_eq!(reexports[8], "external😃");
    assert_eq!(reexports[9], "e5");
    assert_eq!(reexports[10], "e6");
    assert_eq!(reexports[11], "external𤭢");
    assert_eq!(reexports[12], "./styles");
    assert_eq!(reexports[13], "./styles2");
    assert_eq!(reexports[14], "./Accordion");
  }

  #[test]
  fn module_exports_reexport_spread() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
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
    )
    .unwrap();

    assert_eq!(exports.len(), 2);
    assert_eq!(exports[0], "c");
    assert_eq!(exports[1], "name");
    assert_eq!(reexports.len(), 2);
    assert_eq!(reexports[0], "dep1");
    assert_eq!(reexports[1], "dep2");
  }

  #[test]
  fn regexp_case() {
    parse_cjs(
      r#"
        class Number {
        }
        
        /("|')(?<value>(\\\\(\\1)|[^\\1])*)?(\\1)/.exec(\`'\\\\"\\\\'aa'\`);
        
        const x = \`"\${label.replace(/"/g, "\\\\\\"")}"\`
    "#,
    )
    .unwrap();
  }

  #[test]
  fn regexp_division() {
    parse_cjs(r#"\nconst x = num / /'/.exec(l)[0].slice(1, -1)//'""#).unwrap();
  }

  #[test]
  fn multiline_string_escapes() {
    parse_cjs(
      r#"const str = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAB4AAAAeCAYAAAA7MK6iAAAABmJLR0QA/wAAAAAzJ3zzAAAGTElEQV\\\r\n\t\tRIx+VXe1BU1xn/zjn7ugvL4sIuQnll5U0ELAQxig7WiQYz6NRHa6O206qdSXXSxs60dTK200zNY9q0dcRpMs1jkrRNWmaijCVoaU';\r\n"#,
    ).unwrap();
  }

  #[test]
  fn dotted_number() {
    parse_cjs(r#"const x = 5. / 10"#).unwrap();
  }

  #[test]
  fn division_operator_case() {
    parse_cjs(
      r#"
    function log(r){
        if(g>=0){u[g++]=m;g>=n.logSz&&(g=0)}else{u.push(m);u.length>=n.logSz&&(g=0)}/^(DBG|TICK): /.test(r)||t.Ticker.tick(454,o.slice(0,200));
      }
      
      (function(n){
      })();
    "#,
    ).unwrap();
  }

  #[test]
  fn single_parse_cases() {
    parse_cjs(r#"'asdf'"#).unwrap();
    parse_cjs(r#"/asdf/"#).unwrap();
    parse_cjs(r#"\`asdf\`"#).unwrap();
    parse_cjs(r#"/**/"#).unwrap();
    parse_cjs(r#"//"#).unwrap();
  }

  #[test]
  fn shebang() {
    let CjsAnalysis { exports, reexports } = parse_cjs(r#"#!"#).unwrap();

    assert_eq!(exports.len(), 0);
    assert_eq!(reexports.len(), 0);

    let CjsAnalysis { exports, reexports } = parse_cjs(
      r#"#! (  {
        exports.asdf = 'asdf';
      "#,
    )
    .unwrap();

    assert_eq!(exports.len(), 1);
    assert_eq!(exports[0], "asdf");
    assert_eq!(reexports.len(), 0);
  }

  #[test]
  fn non_identifiers() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
      r#"
      module.exports = { 'ab cd': foo };
      exports['not identifier'] = 'asdf';
      exports['\\u{D83C}\\u{DF10}'] = 1;
      exports['\\u{D83C}'] = 1;
      exports['\\''] = 1;
      exports['@notidentifier'] = 'asdf';
      Object.defineProperty(exports, "%notidentifier", { value: x });
      Object.defineProperty(exports, 'hm🤔', { value: x });
      exports['⨉'] = 45;
      exports['α'] = 54;
      exports.package = 'STRICT RESERVED!';
      exports.var = 'RESERVED';
    "#,
    )
    .unwrap();

    assert_eq!(exports.len(), 11);
    assert_eq!(exports[0], "ab cd");
    assert_eq!(exports[1], "not identifier");
    // assert_eq!(exports[2], "\u{D83C}\u{DF10}");
    assert_eq!(exports[3], "\'");
    assert_eq!(exports[4], "@notidentifier");
    assert_eq!(exports[5], "%notidentifier");
    assert_eq!(exports[6], "hm🤔");
    assert_eq!(exports[7], "⨉");
    assert_eq!(exports[8], "α");
    assert_eq!(exports[9], "package");
    assert_eq!(exports[10], "var");
    assert_eq!(reexports.len(), 0);
  }

  #[test]
  fn literal_exports() {
    let CjsAnalysis { exports, reexports } =
      parse_cjs(r#"module.exports = { a, b: c, d, 'e': f };"#).unwrap();

    assert_eq!(exports.len(), 4);
    assert_eq!(exports[0], "a");
    assert_eq!(exports[1], "b");
    assert_eq!(exports[2], "d");
    assert_eq!(exports[3], "e");
    assert_eq!(reexports.len(), 0);
  }

  #[test]
  fn literal_exports_unsupported() {
    let CjsAnalysis { exports, reexports } =
      parse_cjs(r#"module.exports = { a = 5, b };"#).unwrap();

    assert_eq!(exports.len(), 1);
    assert_eq!(exports[0], "a");
    assert_eq!(reexports.len(), 0);
  }

  #[test]
  fn literal_exports_example() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
      r#"
      module.exports = {
        // These WILL be detected as exports
        a: a,
        b: b,
        
        // This WILL be detected as an export
        e: require('d'),
      
        // These WONT be detected as exports
        // because the object parser stops on the non-identifier
        // expression "require('d')"
        f: 'f'
      }
    "#,
    )
    .unwrap();

    assert_eq!(exports.len(), 3);
    assert_eq!(exports[2], "e");
    assert_eq!(reexports.len(), 0);
  }

  #[test]
  fn literal_exports_complex() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
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
    )
    .unwrap();

    assert_eq!(exports.len(), 2);
    assert_eq!(exports[0], "Parser");
    assert_eq!(exports[1], "Tokenizer");
    assert_eq!(reexports.len(), 0);
  }

  #[test]
  fn define_property_value() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
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
    ).unwrap();

    assert_eq!(exports.len(), 3);
    assert_eq!(exports[0], "thing");
    assert_eq!(exports[1], "other");
    assert_eq!(exports[2], "__esModule");
    assert_eq!(reexports.len(), 0);
  }

  #[test]
  fn module_assign() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
      r#"
      module.exports.asdf = 'asdf';
      exports = 'asdf';
      module.exports = require('./asdf');
      if (maybe)
        module.exports = require("./another");
    "#,
    )
    .unwrap();

    assert_eq!(exports.len(), 1);
    assert_eq!(exports[0], "asdf");
    assert_eq!(reexports.len(), 1);
    assert_eq!(reexports[0], "./another");
  }

  #[test]
  fn simple_export_with_unicode_conversions() {
    parse_cjs(
      r#"
        export var p𓀀s,q
    "#,
    )
    .unwrap_err();
  }

  #[test]
  fn simple_import() {
    parse_cjs(
      r#"
      import test from "test";
      console.log(test);
    "#,
    )
    .unwrap_err();
  }

  #[test]
  fn exported_function() {
    parse_cjs(
      r#"
    export function a𓀀 () {
    }
    export class Q{
    }
    "#,
    )
    .unwrap_err();
  }

  #[test]
  fn export_destructuring() {
    parse_cjs(
      r#"
    export const { a, b } = foo;
    export { ok };
    "#,
    )
    .unwrap_err();
  }

  #[test]
  fn minified_import_syntax() {
    parse_cjs(r#"
    import{TemplateResult as t}from"lit-html";import{a as e}from"./chunk-4be41b30.js";export{j as SVGTemplateResult,i as TemplateResult,g as html,h as svg}from"./chunk-4be41b30.js";window.JSCompiler_renameProperty='asdf';
    "#).unwrap_err();
  }

  #[test]
  fn plus_plus_division() {
    parse_cjs(r#"tick++/fetti;f=(1)+")";"#).unwrap();
  }

  #[test]
  fn return_bracket_division() {
    parse_cjs(r#"function variance(){return s/(a-1)}"#).unwrap();
  }

  #[test]
  fn import_dot_meta() {
    parse_cjs(
      r#"
      export var hello = 'world';
      console.log(import.meta.url);
    "#,
    )
    .unwrap_err();
  }

  #[test]
  fn import_meta_edge_cases() {
    parse_cjs(
      r#"
    // Import meta
    import.
     meta
    // Not import meta
    a.
    import.
      meta
    "#,
    )
    .unwrap_err();
  }

  #[test]
  fn dynamic_import_method() {
    parse_cjs(
      r#"
      class A {
        import() {
        }
      }
    "#,
    )
    .unwrap();
  }

  #[test]
  fn comments() {
    parse_cjs(
      r#"
    /*
    VERSION
  */import util from 'util';
//
function x() {
}
      /**/
      // '
      /* / */
      /*
         * export { b }
      \\*/export { a }
      function () {
        /***/
      }
    
    "#,
    )
    .unwrap_err();
  }

  #[test]
  fn bracket_matching() {
    parse_cjs(
      r#"
      instance.extend('parseExprAtom', function (nextMethod) {
        return function () {
          function parseExprAtom(refDestructuringErrors) {
            if (this.type === tt._import) {
              return parseDynamicImport.call(this);
            }
            return c(refDestructuringErrors);
          }
        }();
      });
    "#,
    )
    .unwrap();
  }

  #[test]
  fn division_regex_ambiguity() {
    parse_cjs(
      r#"
    /as)df/; x();
      a / 2; '  /  '
      while (true)
        /test'/
      x-/a'/g
      try {}
      finally{}/a'/g
      (x);{f()}/d'export { b }/g
      ;{}/e'/g;
      {}/f'/g
      a / 'b' / c;
      /a'/ - /b'/;
      +{} /g -'/g'
      ('a')/h -'/g'
      if //x
      ('a')/i'/g;
      /asdf/ / /as'df/; // '
      p = \`\${/test/ + 5}\`;
      /regex/ / x;
      function m() {
        return /*asdf8*// 5/;
      }
    "#,
    )
    .unwrap();
  }

  #[test]
  fn template_string_expression_ambiguity() {
    let CjsAnalysis { exports, reexports } = parse_cjs(
      r#"
      \`$\`
      import('a');
      \`\`
      exports.a = 'a';
      \`a$b\`
      exports['b'] = 'b';
      \`{$}\`
      exports['b'].b;
    "#,
    )
    .unwrap();

    assert_eq!(exports.len(), 2);
    assert_eq!(exports[0], "a");
    assert_eq!(exports[1], "b");
    assert_eq!(reexports.len(), 0);
  }
}
