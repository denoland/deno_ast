// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use oxc::allocator::Allocator;
use oxc::ast::ast::CommentKind;
use oxc::parser::{ParseOptions, Parser};

use crate::MediaType;
use crate::parsing::get_source_type;

#[derive(Debug, Clone)]
pub enum TokenOrComment {
  Token(oxc::parser::Token),
  Comment { kind: CommentKind, text: String },
}

#[derive(Debug, Clone)]
pub struct LexedItem {
  /// Range of the token or comment.
  pub range: std::ops::Range<usize>,
  /// Token or comment.
  pub inner: TokenOrComment,
}

/// Given the source text and media type, tokenizes the provided
/// text to a collection of tokens and comments.
pub fn lex(source: &str, media_type: MediaType) -> Vec<LexedItem> {
  let allocator = Allocator::default();
  let source_type = get_source_type(media_type);

  let ret = Parser::new(&allocator, source, source_type)
    .with_options(ParseOptions {
      parse_regular_expression: false,
      ..Default::default()
    })
    .parse();

  let mut items: Vec<LexedItem> = Vec::new();

  // Extract comments from the parsed program
  for comment in &ret.program.comments {
    let start = comment.span.start as usize;
    let end = comment.span.end as usize;
    items.push(LexedItem {
      range: start..end,
      inner: TokenOrComment::Comment {
        kind: comment.kind,
        text: source[start..end].to_string(),
      },
    });
  }

  // OXC doesn't expose a separate token stream in the same way SWC does.
  // Tokens are available via ParserReturn.tokens when enabled.
  for &token in &ret.tokens {
    let start = token.start() as usize;
    let end = token.end() as usize;
    items.push(LexedItem {
      range: start..end,
      inner: TokenOrComment::Token(token),
    });
  }

  items.sort_by_key(|item| item.range.start);
  items
}
