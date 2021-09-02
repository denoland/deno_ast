// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use crate::swc::common::comments::Comment;
use crate::swc::common::comments::Comments;
use crate::swc::common::comments::SingleThreadedComments;
use crate::swc::common::comments::SingleThreadedCommentsMapInner;
use crate::swc::common::BytePos;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug)]
struct MultiThreadedCommentsInner {
  leading: SingleThreadedCommentsMapInner,
  trailing: SingleThreadedCommentsMapInner,
}

/// An implementation of swc's `Comments` that implements `Sync`
/// to support being used in multi-threaded code. This implementation
/// is immutable and should you need mutability you may create a copy
/// by converting it to an swc `SingleThreadedComments`.
#[derive(Clone, Debug)]
pub struct MultiThreadedComments {
  inner: Arc<MultiThreadedCommentsInner>,
}

impl MultiThreadedComments {
  pub fn from_single_threaded(comments: SingleThreadedComments) -> Self {
    let (leading, trailing) = comments.take_all();
    let leading = Rc::try_unwrap(leading).unwrap().into_inner();
    let trailing = Rc::try_unwrap(trailing).unwrap().into_inner();
    MultiThreadedComments {
      inner: Arc::new(MultiThreadedCommentsInner { leading, trailing }),
    }
  }

  pub fn as_single_threaded(&self) -> SingleThreadedComments {
    let inner = &self.inner;
    let leading = Rc::new(RefCell::new(inner.leading.to_owned()));
    let trailing = Rc::new(RefCell::new(inner.trailing.to_owned()));
    SingleThreadedComments::from_leading_and_trailing(leading, trailing)
  }

  pub fn leading_map(&self) -> &SingleThreadedCommentsMapInner {
    &self.inner.leading
  }

  pub fn trailing_map(&self) -> &SingleThreadedCommentsMapInner {
    &self.inner.trailing
  }

  /// Gets a vector of all the comments sorted by position.
  pub fn get_vec(&self) -> Vec<Comment> {
    let mut comments = self
      .inner
      .leading
      .values()
      .chain(self.inner.trailing.values())
      .flatten()
      .cloned()
      .collect::<Vec<_>>();
    comments.sort_by_key(|comment| comment.span.lo);
    comments
  }
}

impl Comments for MultiThreadedComments {
  fn has_leading(&self, pos: BytePos) -> bool {
    self.inner.leading.contains_key(&pos)
  }

  fn get_leading(&self, pos: BytePos) -> Option<Vec<Comment>> {
    self.inner.leading.get(&pos).cloned()
  }

  fn has_trailing(&self, pos: BytePos) -> bool {
    self.inner.trailing.contains_key(&pos)
  }

  fn get_trailing(&self, pos: BytePos) -> Option<Vec<Comment>> {
    self.inner.trailing.get(&pos).cloned()
  }

  fn add_leading(&self, _pos: BytePos, _cmt: Comment) {
    panic_readonly();
  }

  fn add_leading_comments(&self, _pos: BytePos, _comments: Vec<Comment>) {
    panic_readonly();
  }

  fn move_leading(&self, _from: BytePos, _to: BytePos) {
    panic_readonly();
  }

  fn take_leading(&self, _pos: BytePos) -> Option<Vec<Comment>> {
    panic_readonly();
  }

  fn add_trailing(&self, _pos: BytePos, _cmt: Comment) {
    panic_readonly();
  }

  fn add_trailing_comments(&self, _pos: BytePos, _comments: Vec<Comment>) {
    panic_readonly();
  }

  fn move_trailing(&self, _from: BytePos, _to: BytePos) {
    panic_readonly();
  }

  fn take_trailing(&self, _pos: BytePos) -> Option<Vec<Comment>> {
    panic_readonly();
  }

  fn add_pure_comment(&self, _pos: BytePos) {
    panic_readonly();
  }
}

fn panic_readonly() -> ! {
  panic!("MultiThreadedComments do not support write operations")
}
