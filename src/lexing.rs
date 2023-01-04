// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::rc::Rc;

use crate::get_syntax;
use crate::swc::atoms::Atom;
use crate::swc::common::comments::Comment;
use crate::swc::common::comments::CommentKind;
use crate::swc::common::comments::SingleThreadedComments;
use crate::swc::common::input::StringInput;
use crate::swc::parser::lexer::Lexer;
use crate::swc::parser::token::Token;
use crate::MediaType;
use crate::SourceRangedForSpanned;
use crate::StartSourcePos;
use crate::ES_VERSION;

#[derive(Debug, Clone)]
pub enum TokenOrComment {
  Token(Token),
  Comment { kind: CommentKind, text: Atom },
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
  let comments = SingleThreadedComments::default();
  let start_pos = StartSourcePos::START_SOURCE_POS;
  let lexer = Lexer::new(
    get_syntax(media_type),
    ES_VERSION,
    StringInput::new(
      source,
      start_pos.as_byte_pos(),
      (start_pos + source.len()).as_byte_pos(),
    ),
    Some(&comments),
  );

  let mut tokens: Vec<LexedItem> = lexer
    .map(|token| LexedItem {
      range: token.range().as_byte_range(start_pos),
      inner: TokenOrComment::Token(token.token),
    })
    .collect();

  tokens.extend(flatten_comments(comments).map(|comment| LexedItem {
    range: comment.range().as_byte_range(start_pos),
    inner: TokenOrComment::Comment {
      kind: comment.kind,
      text: comment.text,
    },
  }));

  tokens.sort_by_key(|item| item.range.start);

  tokens
}

fn flatten_comments(
  comments: SingleThreadedComments,
) -> impl Iterator<Item = Comment> {
  let (leading, trailing) = comments.take_all();
  let leading = Rc::try_unwrap(leading).unwrap().into_inner();
  let trailing = Rc::try_unwrap(trailing).unwrap().into_inner();
  let mut comments = leading;
  comments.extend(trailing);
  comments.into_iter().flat_map(|el| el.1)
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::MediaType;

  #[test]
  fn tokenize_with_comments() {
    let items = lex(
      "const /* 1 */ t: number /* 2 */ = 5; // 3",
      MediaType::TypeScript,
    );
    assert_eq!(items.len(), 10);

    // only bother testing a few
    assert!(matches!(items[1].inner, TokenOrComment::Comment { .. }));
    assert!(matches!(
      items[3].inner,
      TokenOrComment::Token(Token::Colon)
    ));
    assert!(matches!(items[9].inner, TokenOrComment::Comment { .. }));
  }

  #[test]
  fn handle_bom() {
    const BOM_CHAR: char = '\u{FEFF}';
    let items = lex(&format!("{}1", BOM_CHAR), MediaType::JavaScript);
    assert_eq!(items.len(), 1);
    assert_eq!(items[0].range.start, BOM_CHAR.len_utf8());
  }
}
