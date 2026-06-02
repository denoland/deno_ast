// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

// In OXC, comments are stored directly in Program.comments as Vec<'a, Comment>.
// There is no separate comments data structure needed like SWC's
// SingleThreadedComments/MultiThreadedComments.
//
// OXC's Comment type is:
// pub struct Comment {
//     pub span: Span,
//     pub kind: CommentKind, // Line or Block
// }
//
// The comment text can be extracted from the source text using the span.
//
// This module is kept for any helper functions we may need.

pub use oxc::ast::ast::Comment;
pub use oxc::ast::ast::CommentKind;
