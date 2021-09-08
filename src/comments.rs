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
///
/// When using this, you will want to use the
/// `deno_ast::swc::common::comments::Comments` trait.
#[derive(Clone, Debug)]
pub struct MultiThreadedComments {
  inner: Arc<MultiThreadedCommentsInner>,
}

impl MultiThreadedComments {
  /// Creates a new `MultiThreadedComments` from an swc `SingleThreadedComments`.
  pub fn from_single_threaded(comments: SingleThreadedComments) -> Self {
    let (leading, trailing) = comments.take_all();
    let leading = Rc::try_unwrap(leading).unwrap().into_inner();
    let trailing = Rc::try_unwrap(trailing).unwrap().into_inner();
    MultiThreadedComments {
      inner: Arc::new(MultiThreadedCommentsInner { leading, trailing }),
    }
  }

  /// Gets a clone of the underlying data as `SingleThreadedComments`.
  ///
  /// This may be useful for getting a mutable data structure for use
  /// when transpiling.
  pub fn as_single_threaded(&self) -> SingleThreadedComments {
    let inner = &self.inner;
    let leading = Rc::new(RefCell::new(inner.leading.to_owned()));
    let trailing = Rc::new(RefCell::new(inner.trailing.to_owned()));
    SingleThreadedComments::from_leading_and_trailing(leading, trailing)
  }

  /// Gets a reference to the leading comment map.
  pub fn leading_map(&self) -> &SingleThreadedCommentsMapInner {
    &self.inner.leading
  }

  /// Gets a reference to the trailing comment map.
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

#[cfg(test)]
mod test {
  use swc_common::comments::Comments;

  use crate::parse_module;
  use crate::swc::common::comments::SingleThreadedComments;
  use crate::swc::common::BytePos;
  use crate::MediaType;
  use crate::MultiThreadedComments;
  use crate::ParseParams;
  use crate::SourceTextInfo;

  #[test]
  fn general_use() {
    let comments = get_single_threaded_comments("// 1\nt;/* 2 */");
    let comments = MultiThreadedComments::from_single_threaded(comments);

    // maps
    assert_eq!(comments.leading_map().len(), 1);
    assert_eq!(
      comments.leading_map().get(&BytePos(5)).unwrap()[0].text,
      " 1"
    );
    assert_eq!(comments.trailing_map().len(), 1);
    assert_eq!(
      comments.trailing_map().get(&BytePos(7)).unwrap()[0].text,
      " 2 "
    );

    // comment vector
    let comments_vec = comments.get_vec();
    assert_eq!(comments_vec.len(), 2);
    assert_eq!(comments_vec[0].text, " 1");
    assert_eq!(comments_vec[1].text, " 2 ");

    // comments trait
    assert!(comments.has_leading(BytePos(5)));
    assert!(!comments.has_leading(BytePos(7)));

    assert_eq!(comments.get_leading(BytePos(5)).unwrap()[0].text, " 1");
    assert!(comments.get_leading(BytePos(7)).is_none());

    assert!(!comments.has_trailing(BytePos(5)));
    assert!(comments.has_trailing(BytePos(7)));

    assert!(comments.get_trailing(BytePos(5)).is_none());
    assert_eq!(comments.get_trailing(BytePos(7)).unwrap()[0].text, " 2 ");
  }

  fn get_single_threaded_comments(text: &str) -> SingleThreadedComments {
    let module = parse_module(ParseParams {
      specifier: "file.ts".to_string(),
      source: SourceTextInfo::from_string(text.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
    })
    .expect("expects a module");
    module.comments().as_single_threaded()
  }
}
