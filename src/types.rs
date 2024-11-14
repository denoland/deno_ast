// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::fmt;
use deno_error::JsError;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticLevel;
use crate::diagnostics::DiagnosticLocation;
use crate::diagnostics::DiagnosticSnippet;
use crate::diagnostics::DiagnosticSnippetHighlight;
use crate::diagnostics::DiagnosticSnippetHighlightStyle;
use crate::diagnostics::DiagnosticSourcePos;
use crate::diagnostics::DiagnosticSourceRange;
use crate::swc::parser::error::SyntaxError;
use crate::LineAndColumnDisplay;
use crate::ModuleSpecifier;
use crate::SourceRange;
use crate::SourceRangedForSpanned;
use crate::SourceTextInfo;

/// Parsing diagnostic.
#[derive(Debug, Clone, JsError)]
#[class(SYNTAX)]
pub struct ParseDiagnostic {
  /// Specifier of the source the diagnostic occurred in.
  pub specifier: ModuleSpecifier,
  /// Range of the diagnostic.
  pub range: SourceRange,
  /// Swc syntax error
  pub kind: SyntaxError,
  pub(crate) source: SourceTextInfo,
}

impl Eq for ParseDiagnostic {}

impl PartialEq for ParseDiagnostic {
  fn eq(&self, other: &Self) -> bool {
    // excludes the source
    self.specifier == other.specifier
      && self.range == other.range
      && self.kind == other.kind
  }
}

impl ParseDiagnostic {
  /// 1-indexed display position the diagnostic occurred at.
  pub fn display_position(&self) -> LineAndColumnDisplay {
    self.source.line_and_column_display(self.range.start)
  }
}

impl Diagnostic for ParseDiagnostic {
  fn level(&self) -> DiagnosticLevel {
    DiagnosticLevel::Error
  }

  fn code(&self) -> Cow<'_, str> {
    Cow::Borrowed(match &self.kind {
      SyntaxError::Eof => "eof",
      SyntaxError::DeclNotAllowed => "decl-not-allowed",
      SyntaxError::UsingDeclNotAllowed => "using-decl-not-allowed",
      SyntaxError::UsingDeclNotAllowedForForInLoop => {
        "using-decl-not-allowed-for-for-in-loop"
      }
      SyntaxError::UsingDeclNotEnabled => "using-decl-not-enabled",
      SyntaxError::InvalidNameInUsingDecl => "invalid-name-in-using-decl",
      SyntaxError::InitRequiredForUsingDecl => "init-required-for-using-decl",
      SyntaxError::PrivateNameInInterface => "private-name-in-interface",
      SyntaxError::InvalidSuperCall => "invalid-super-call",
      SyntaxError::InvalidSuper => "invalid-super",
      SyntaxError::InvalidSuperPrivateName => "invalid-super-private-name",
      SyntaxError::InvalidNewTarget => "invalid-new-target",
      SyntaxError::InvalidImport => "invalid-import",
      SyntaxError::ArrowNotAllowed => "arrow-not-allowed",
      SyntaxError::ExportNotAllowed => "export-not-allowed",
      SyntaxError::GetterSetterCannotBeReadonly => {
        "getter-setter-cannot-be-readonly"
      }
      SyntaxError::GetterParam => "getter-param",
      SyntaxError::SetterParam => "setter-param",
      SyntaxError::TopLevelAwaitInScript => "top-level-await-in-script",
      SyntaxError::LegacyDecimal => "legacy-decimal",
      SyntaxError::LegacyOctal => "legacy-octal",
      SyntaxError::InvalidIdentChar => "invalid-ident-char",
      SyntaxError::ExpectedDigit { .. } => "expected-digit",
      SyntaxError::SetterParamRequired => "setter-param-required",
      SyntaxError::RestPatInSetter => "rest-pat-in-setter",
      SyntaxError::UnterminatedBlockComment => "unterminated-block-comment",
      SyntaxError::UnterminatedStrLit => "unterminated-str-lit",
      SyntaxError::ExpectedUnicodeEscape => "expected-unicode-escape",
      SyntaxError::EscapeInReservedWord { .. } => "escape-in-reserved-word",
      SyntaxError::UnterminatedRegExp => "unterminated-reg-exp",
      SyntaxError::UnterminatedTpl => "unterminated-tpl",
      SyntaxError::IdentAfterNum => "ident-after-num",
      SyntaxError::UnexpectedChar { .. } => "unexpected-char",
      SyntaxError::InvalidStrEscape => "invalid-str-escape",
      SyntaxError::InvalidUnicodeEscape => "invalid-unicode-escape",
      SyntaxError::BadCharacterEscapeSequence { .. } => {
        "bad-character-escape-sequence"
      }
      SyntaxError::NumLitTerminatedWithExp => "num-lit-terminated-with-exp",
      SyntaxError::LegacyCommentInModule => "legacy-comment-in-module",
      SyntaxError::InvalidIdentInStrict(_) => "invalid-ident-in-strict",
      SyntaxError::InvalidIdentInAsync => "invalid-ident-in-async",
      SyntaxError::EvalAndArgumentsInStrict => "eval-and-arguments-in-strict",
      SyntaxError::ArgumentsInClassField => "arguments-in-class-field",
      SyntaxError::IllegalLanguageModeDirective => {
        "illegal-language-mode-directive"
      }
      SyntaxError::UnaryInExp { .. } => "unary-in-exp",
      SyntaxError::Hash => "hash",
      SyntaxError::LineBreakInThrow => "line-break-in-throw",
      SyntaxError::LineBreakBeforeArrow => "line-break-before-arrow",
      SyntaxError::Unexpected { .. } => "unexpected",
      SyntaxError::UnexpectedTokenWithSuggestions { .. } => {
        "unexpected-token-with-suggestions"
      }
      SyntaxError::ReservedWordInImport => "reserved-word-in-import",
      SyntaxError::AssignProperty => "assign-property",
      SyntaxError::Expected(_, _) => "expected",
      SyntaxError::ExpectedSemiForExprStmt { .. } => {
        "expected-semi-for-expr-stmt"
      }
      SyntaxError::AwaitStar => "await-star",
      SyntaxError::ReservedWordInObjShorthandOrPat => {
        "reserved-word-in-obj-shorthand-or-pat"
      }
      SyntaxError::NullishCoalescingWithLogicalOp => {
        "nullish-coalescing-with-logical-op"
      }
      SyntaxError::MultipleDefault { .. } => "multiple-default",
      SyntaxError::CommaAfterRestElement => "comma-after-rest-element",
      SyntaxError::NonLastRestParam => "non-last-rest-param",
      SyntaxError::SpreadInParenExpr => "spread-in-paren-expr",
      SyntaxError::EmptyParenExpr => "empty-paren-expr",
      SyntaxError::InvalidPat => "invalid-pat",
      SyntaxError::InvalidExpr => "invalid-expr",
      SyntaxError::NotSimpleAssign => "not-simple-assign",
      SyntaxError::ExpectedIdent => "expected-ident",
      SyntaxError::ExpectedSemi => "expected-semi",
      SyntaxError::DuplicateLabel(_) => "duplicate-label",
      SyntaxError::AsyncGenerator => "async-generator",
      SyntaxError::NonTopLevelImportExport => "non-top-level-import-export",
      SyntaxError::ImportExportInScript => "import-export-in-script",
      SyntaxError::ImportMetaInScript => "import-meta-in-script",
      SyntaxError::PatVarWithoutInit => "pat-var-without-init",
      SyntaxError::WithInStrict => "with-in-strict",
      SyntaxError::ReturnNotAllowed => "return-not-allowed",
      SyntaxError::TooManyVarInForInHead => "too-many-var-in-for-in-head",
      SyntaxError::VarInitializerInForInHead => {
        "var-initializer-in-for-in-head"
      }
      SyntaxError::LabelledGeneratorOrAsync => "labelled-generator-or-async",
      SyntaxError::LabelledFunctionInStrict => "labelled-function-in-strict",
      SyntaxError::YieldParamInGen => "yield-param-in-gen",
      SyntaxError::AwaitParamInAsync => "await-param-in-async",
      SyntaxError::AwaitForStmt => "await-for-stmt",
      SyntaxError::AwaitInFunction => "await-in-function",
      SyntaxError::UnterminatedJSXContents => "unterminated-jsx-contents",
      SyntaxError::EmptyJSXAttr => "empty-jsx-attr",
      SyntaxError::InvalidJSXValue => "invalid-jsx-value",
      SyntaxError::JSXExpectedClosingTagForLtGt => {
        "jsx-expected-closing-tag-for-lt-gt"
      }
      SyntaxError::JSXExpectedClosingTag { .. } => "jsx-expected-closing-tag",
      SyntaxError::InvalidLeadingDecorator => "invalid-leading-decorator",
      SyntaxError::DecoratorOnExport => "decorator-on-export",
      SyntaxError::TsRequiredAfterOptional => "ts-required-after-optional",
      SyntaxError::TsInvalidParamPropPat => "ts-invalid-param-prop-pat",
      SyntaxError::SpaceBetweenHashAndIdent => "space-between-hash-and-ident",
      SyntaxError::AsyncConstructor => "async-constructor",
      SyntaxError::PropertyNamedConstructor => "property-named-constructor",
      SyntaxError::PrivateConstructor => "private-constructor",
      SyntaxError::PrivateNameModifier(_) => "private-name-modifier",
      SyntaxError::ConstructorAccessor => "constructor-accessor",
      SyntaxError::ReadOnlyMethod => "read-only-method",
      SyntaxError::GeneratorConstructor => "generator-constructor",
      SyntaxError::DuplicateConstructor => "duplicate-constructor",
      SyntaxError::TsBindingPatCannotBeOptional => {
        "ts-binding-pat-cannot-be-optional"
      }
      SyntaxError::SuperCallOptional => "super-call-optional",
      SyntaxError::OptChainCannotFollowConstructorCall => {
        "opt-chain-cannot-follow-constructor-call"
      }
      SyntaxError::TaggedTplInOptChain => "tagged-tpl-in-opt-chain",
      SyntaxError::TrailingCommaInsideImport => "trailing-comma-inside-import",
      SyntaxError::ExportDefaultWithOutFrom => "export-default-without-from",
      SyntaxError::ExportExpectFrom(_) => "export-expect-from",
      SyntaxError::DotsWithoutIdentifier => "dots-without-identifier",
      SyntaxError::NumericSeparatorIsAllowedOnlyBetweenTwoDigits => {
        "numeric-separator-allowed-only-between-two-digits"
      }
      SyntaxError::ImportBindingIsString(_) => "import-binding-is-string",
      SyntaxError::ExportBindingIsString => "export-binding-is-string",
      SyntaxError::ConstDeclarationsRequireInitialization => {
        "const-declarations-require-initialization"
      }
      SyntaxError::DuplicatedRegExpFlags(_) => "duplicated-reg-exp-flags",
      SyntaxError::UnknownRegExpFlags => "unknown-reg-exp-flags",
      SyntaxError::TS1003 => "TS1003",
      SyntaxError::TS1005 => "TS1005",
      SyntaxError::TS1009 => "TS1009",
      SyntaxError::TS1014 => "TS1014",
      SyntaxError::TS1015 => "TS1015",
      SyntaxError::TS1029(_, _) => "TS1029",
      SyntaxError::TS1030(_) => "TS1030",
      SyntaxError::TS1031 => "TS1031",
      SyntaxError::TS1038 => "TS1038",
      SyntaxError::TS1042 => "TS1042",
      SyntaxError::TS1047 => "TS1047",
      SyntaxError::TS1048 => "TS1048",
      SyntaxError::TS1056 => "TS1056",
      SyntaxError::TS1085 => "TS1085",
      SyntaxError::TS1089(_) => "TS1089",
      SyntaxError::TS1092 => "TS1092",
      SyntaxError::TS1096 => "TS1096",
      SyntaxError::TS1098 => "TS1098",
      SyntaxError::TS1100 => "TS1100",
      SyntaxError::TS1102 => "TS1102",
      SyntaxError::TS1105 => "TS1105",
      SyntaxError::TS1106 => "TS1106",
      SyntaxError::TS1107 => "TS1107",
      SyntaxError::TS1109 => "TS1109",
      SyntaxError::TS1110 => "TS1110",
      SyntaxError::TS1114 => "TS1114",
      SyntaxError::TS1115 => "TS1115",
      SyntaxError::TS1116 => "TS1116",
      SyntaxError::TS1123 => "TS1123",
      SyntaxError::TS1141 => "TS1141",
      SyntaxError::TS1162 => "TS1162",
      SyntaxError::TS1164 => "TS1164",
      SyntaxError::TS1171 => "TS1171",
      SyntaxError::TS1172 => "TS1172",
      SyntaxError::TS1173 => "TS1173",
      SyntaxError::TS1174 => "TS1174",
      SyntaxError::TS1175 => "TS1175",
      SyntaxError::TS1183 => "TS1183",
      SyntaxError::TS1184 => "TS1184",
      SyntaxError::TS1185 => "TS1185",
      SyntaxError::TS1093 => "TS1093",
      SyntaxError::TS1196 => "TS1196",
      SyntaxError::TS1242 => "TS1242",
      SyntaxError::TS1243(_, _) => "TS1243",
      SyntaxError::TS1244 => "TS1244",
      SyntaxError::TS1245 => "TS1245",
      SyntaxError::TS1267 => "TS1267",
      SyntaxError::TS1273(_) => "TS1273",
      SyntaxError::TS1274(_) => "TS1274",
      SyntaxError::TS1277(_) => "TS1277",
      SyntaxError::TS2206 => "TS2206",
      SyntaxError::TS2207 => "TS2207",
      SyntaxError::TS2369 => "TS2369",
      SyntaxError::TS2371 => "TS2371",
      SyntaxError::TS2406 => "TS2406",
      SyntaxError::TS2410 => "TS2410",
      SyntaxError::TS2414 => "TS2414",
      SyntaxError::TS2427 => "TS2427",
      SyntaxError::TS2452 => "TS2452",
      SyntaxError::TS2483 => "TS2483",
      SyntaxError::TS2491 => "TS2491",
      SyntaxError::TS2499 => "TS2499",
      SyntaxError::TS2703 => "TS2703",
      SyntaxError::TS4112 => "TS4112",
      SyntaxError::TS8038 => "TS8038",
      SyntaxError::TSTypeAnnotationAfterAssign => {
        "ts-type-annotation-after-assign"
      }
      SyntaxError::TsNonNullAssertionNotAllowed(_) => {
        "ts-non-null-assertion-not-allowed"
      }
      SyntaxError::WithLabel { .. } => "with-label",
      SyntaxError::ReservedTypeAssertion => "reserved-type-assertion",
      SyntaxError::ReservedArrowTypeParam => "reserved-arrow-type-param",
      _ => "unknown",
    })
  }

  fn message(&self) -> Cow<'_, str> {
    self.kind.msg()
  }

  fn location(&self) -> DiagnosticLocation {
    DiagnosticLocation::ModulePosition {
      specifier: Cow::Borrowed(&self.specifier),
      source_pos: DiagnosticSourcePos::SourcePos(self.range.start),
      text_info: Cow::Borrowed(&self.source),
    }
  }

  fn snippet(&self) -> Option<DiagnosticSnippet<'_>> {
    Some(DiagnosticSnippet {
      source: Cow::Borrowed(&self.source),
      highlights: vec![DiagnosticSnippetHighlight {
        style: DiagnosticSnippetHighlightStyle::Error,
        range: DiagnosticSourceRange {
          start: DiagnosticSourcePos::SourcePos(self.range.start),
          end: DiagnosticSourcePos::SourcePos(self.range.end),
        },
        description: None,
      }],
    })
  }

  fn hint(&self) -> Option<Cow<'_, str>> {
    None
  }

  fn snippet_fixed(&self) -> Option<crate::diagnostics::DiagnosticSnippet<'_>> {
    None
  }

  fn info(&self) -> Cow<'_, [Cow<'_, str>]> {
    Cow::Borrowed(&[])
  }

  fn docs_url(&self) -> Option<Cow<'_, str>> {
    None
  }
}

impl ParseDiagnostic {
  pub fn from_swc_error(
    err: crate::swc::parser::error::Error,
    specifier: &ModuleSpecifier,
    source: SourceTextInfo,
  ) -> ParseDiagnostic {
    ParseDiagnostic {
      range: err.range(),
      specifier: specifier.clone(),
      kind: err.into_kind(),
      source,
    }
  }
}

impl std::error::Error for ParseDiagnostic {}

impl fmt::Display for ParseDiagnostic {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let display_position = self.display_position();
    write!(
      f,
      "{} at {}:{}:{}\n\n{}",
      self.message(),
      self.specifier,
      display_position.line_number,
      display_position.column_number,
      // todo(dsherret): remove this catch unwind once we've
      // tested this out a lot
      std::panic::catch_unwind(|| {
        get_range_text_highlight(&self.source, self.range)
          .lines()
          // indent two spaces
          .map(|l| {
            if l.trim().is_empty() {
              String::new()
            } else {
              format!("  {}", l)
            }
          })
          .collect::<Vec<_>>()
          .join("\n")
      })
      .unwrap_or_else(|err| {
        format!("Bug in Deno. Please report this issue: {:?}", err)
      }),
    )
  }
}

#[derive(Debug, JsError)]
#[class(SYNTAX)]
pub struct ParseDiagnosticsError(pub Vec<ParseDiagnostic>);

impl std::error::Error for ParseDiagnosticsError {}

impl fmt::Display for ParseDiagnosticsError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (i, diagnostic) in self.0.iter().enumerate() {
      if i > 0 {
        write!(f, "\n\n")?;
      }

      write!(f, "{}", diagnostic)?
    }

    Ok(())
  }
}

/// Code in this function was adapted from:
/// https://github.com/dprint/dprint/blob/a026a1350d27a61ea18207cb31897b18eaab51a1/crates/core/src/formatting/utils/string_utils.rs#L62
fn get_range_text_highlight(
  source: &SourceTextInfo,
  byte_range: SourceRange,
) -> String {
  fn get_column_index_of_pos(text: &str, pos: usize) -> usize {
    let line_start_byte_pos = get_line_start_byte_pos(text, pos);
    text[line_start_byte_pos..pos].chars().count()
  }

  fn get_line_start_byte_pos(text: &str, pos: usize) -> usize {
    let text_bytes = text.as_bytes();
    for i in (0..pos).rev() {
      if text_bytes.get(i) == Some(&(b'\n')) {
        return i + 1;
      }
    }

    0
  }

  fn get_text_and_error_range(
    source: &SourceTextInfo,
    byte_range: SourceRange,
  ) -> (&str, (usize, usize)) {
    let mut first_line_index = source.line_index(byte_range.start);
    let mut first_line_start = source.line_start(first_line_index);
    let last_line_end = source.line_end(source.line_index(byte_range.end));
    let mut sub_text =
      source.range_text(&SourceRange::new(first_line_start, last_line_end));

    // while the text is empty, show the previous line
    while sub_text.trim().is_empty() && first_line_index > 0 {
      first_line_index -= 1;
      first_line_start = source.line_start(first_line_index);
      sub_text =
        source.range_text(&SourceRange::new(first_line_start, last_line_end));
    }

    let error_start = byte_range.start - first_line_start;
    let error_end = error_start + (byte_range.end - byte_range.start);
    (sub_text, (error_start, error_end))
  }

  let (sub_text, (error_start, error_end)) =
    get_text_and_error_range(source, byte_range);

  let mut result = String::new();
  // don't use .lines() here because it will trim any empty
  // lines, which might for some reason be part of the range
  let lines = sub_text.split('\n').collect::<Vec<_>>();
  let line_count = lines.len();
  for (i, mut line) in lines.into_iter().enumerate() {
    if line.ends_with('\r') {
      line = &line[..line.len() - 1]; // trim the \r
    }
    let is_last_line = i == line_count - 1;
    // don't show all the lines if there are more than 3 lines
    if i > 2 && !is_last_line {
      continue;
    }
    if i > 0 {
      result.push('\n');
    }
    if i == 2 && !is_last_line {
      result.push_str("...");
      continue;
    }

    let mut error_start_char_index = if i == 0 {
      get_column_index_of_pos(sub_text, error_start)
    } else {
      0
    };
    let mut error_end_char_index = if is_last_line {
      get_column_index_of_pos(sub_text, error_end)
    } else {
      line.chars().count()
    };
    let line_char_count = line.chars().count();
    if line_char_count > 90 {
      let start_char_index = if error_start_char_index > 60 {
        std::cmp::min(error_start_char_index - 20, line_char_count - 80)
      } else {
        0
      };
      error_start_char_index -= start_char_index;
      error_end_char_index -= start_char_index;
      let code_text = line
        .chars()
        .skip(start_char_index)
        .take(80)
        .collect::<String>();
      let mut line_text = String::new();
      if start_char_index > 0 {
        line_text.push_str("...");
        error_start_char_index += 3;
        error_end_char_index += 3;
      }
      line_text.push_str(&code_text);
      if line_char_count > start_char_index + code_text.chars().count() {
        error_end_char_index =
          std::cmp::min(error_end_char_index, line_text.chars().count());
        line_text.push_str("...");
      }
      result.push_str(&line_text);
    } else {
      result.push_str(line);
    }
    result.push('\n');

    result.push_str(&" ".repeat(error_start_char_index));
    result.push_str(&"~".repeat(std::cmp::max(
      1, // this means it's the end of the line, so display a single ~
      error_end_char_index - error_start_char_index,
    )));
  }
  result
}

#[cfg(test)]
mod test {
  use dprint_swc_ext::common::SourceRange;
  use dprint_swc_ext::common::SourceTextInfo;
  use pretty_assertions::assert_eq;

  use super::get_range_text_highlight;

  #[test]
  fn range_highlight_all_text() {
    let text = SourceTextInfo::from_string(
      concat!(
        "Line 0 - Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6\n",
        "Line 1\n",
        "Line 2\n",
        "Line 3\n",
        "Line 4"
      ).to_string(),
    );
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_start(0), text.line_end(4))
      ),
      concat!(
        "Line 0 - Testing this out with a long line testing0 testing1 testing2 testing3 t...\n",
        "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n",
        "Line 1\n",
        "~~~~~~\n",
        "...\n",
        "Line 4\n",
        "~~~~~~",
      ),
    );
  }

  #[test]
  fn range_highlight_all_text_last_line_long() {
    let text = SourceTextInfo::from_string(
      concat!(
        "Line 0\n",
        "Line 1\n",
        "Line 2\n",
        "Line 3\n",
        "Line 4 - Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6\n",
      ).to_string(),
    );
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_start(0), text.line_end(4))
      ),
      concat!(
        "Line 0\n",
        "~~~~~~\n",
        "Line 1\n",
        "~~~~~~\n",
        "...\n",
        "Line 4 - Testing this out with a long line testing0 testing1 testing2 testing3 t...\n",
        "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
      ),
    );
  }

  #[test]
  fn range_highlight_range_start_long_line() {
    let text = SourceTextInfo::from_string(
      "Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6 testing7".to_string(),
    );
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_start(0), text.line_start(0) + 1)
      ),
      concat!(
        "Testing this out with a long line testing0 testing1 testing2 testing3 testing4 t...\n",
        "~",
      ),
    );
  }

  #[test]
  fn range_highlight_range_end_long_line() {
    let text = SourceTextInfo::from_string(
      "Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6 testing7".to_string(),
    );
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_end(0) - 1, text.line_end(0))
      ),
      concat!(
        "...ong line testing0 testing1 testing2 testing3 testing4 testing5 testing6 testing7\n",
        "                                                                                  ~",
      ),
    );
  }

  #[test]
  fn range_highlight_whitespace_start_line() {
    let text = SourceTextInfo::from_string("  testing\r\ntest".to_string());
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_end(0) - 1, text.line_end(1))
      ),
      concat!("  testing\n", "        ~\n", "test\n", "~~~~",),
    );
  }

  #[test]
  fn range_end_of_line() {
    let text =
      SourceTextInfo::from_string("  testingtestingtestingtesting".to_string());
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_end(0), text.line_end(0))
      ),
      concat!(
        "  testingtestingtestingtesting\n",
        "                              ~",
      ),
    );
  }
}
