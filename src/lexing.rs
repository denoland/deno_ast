// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use std::rc::Rc;

use crate::get_syntax;
use crate::swc::common::comments::Comment;
use crate::swc::common::comments::CommentKind;
use crate::swc::common::comments::SingleThreadedComments;
use crate::swc::common::input::StringInput;
use crate::swc::common::BytePos;
use crate::swc::common::Span;
use crate::swc::parser::lexer::Lexer;
use crate::swc::parser::token::Token;
use crate::MediaType;
use crate::TARGET;

pub enum TokenOrComment {
  Token(Token),
  Comment { kind: CommentKind, text: String },
}

pub struct LexedItem {
  /// Range of the token or comment.
  pub span: Span,
  /// Token or comment.
  pub inner: TokenOrComment,
}

/// Given the source text and media type, tokenizes the provided
/// text to a collecion of tokens and comments.
pub fn lex(source: &str, media_type: MediaType) -> Vec<LexedItem> {
  let comments = SingleThreadedComments::default();
  let lexer = Lexer::new(
    get_syntax(media_type),
    TARGET,
    StringInput::new(source, BytePos(0), BytePos(source.len() as u32)),
    Some(&comments),
  );

  let mut tokens: Vec<LexedItem> = lexer
    .map(|token| LexedItem {
      span: token.span,
      inner: TokenOrComment::Token(token.token),
    })
    .collect();

  tokens.extend(flatten_comments(comments).map(|comment| LexedItem {
    span: comment.span,
    inner: TokenOrComment::Comment {
      kind: comment.kind,
      text: comment.text,
    },
  }));

  tokens.sort_by_key(|item| item.span.lo.0);

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
