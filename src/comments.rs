// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

// Need to enable this for this file in order to
// implement swc's `Comments` trait
#![allow(clippy::disallowed_types)]

use crate::swc::common::comments::Comment;
use crate::swc::common::comments::Comments as SwcComments;
use crate::swc::common::comments::SingleThreadedComments;
use crate::swc::common::comments::SingleThreadedCommentsMapInner;
use crate::swc::common::BytePos as SwcBytePos;
use crate::SourcePos;

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
  /// Creates a new `MultiThreadedComments` from an swc `SingleThreadedComments`.
  pub fn from_single_threaded(comments: SingleThreadedComments) -> Self {
    let (leading, trailing) = comments.take_all();
    let leading = Rc::try_unwrap(leading).unwrap().into_inner();
    let trailing = Rc::try_unwrap(trailing).unwrap().into_inner();
    Self::from_leading_and_trailing(leading, trailing)
  }

  pub fn from_leading_and_trailing(
    leading: SingleThreadedCommentsMapInner,
    trailing: SingleThreadedCommentsMapInner,
  ) -> Self {
    Self {
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

  pub fn into_single_threaded(self) -> SingleThreadedComments {
    let inner = match Arc::try_unwrap(self.inner) {
      Ok(inner) => inner,
      Err(inner) => MultiThreadedCommentsInner {
        leading: inner.leading.clone(),
        trailing: inner.trailing.clone(),
      },
    };
    let leading = Rc::new(RefCell::new(inner.leading));
    let trailing = Rc::new(RefCell::new(inner.trailing));
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
    let mut comments = self.iter_unstable().cloned().collect::<Vec<_>>();
    comments.sort_by_key(|comment| comment.span.lo);
    comments
  }

  /// Iterates through all the comments in an unstable order.
  pub fn iter_unstable(&self) -> impl Iterator<Item = &Comment> {
    self
      .inner
      .leading
      .values()
      .chain(self.inner.trailing.values())
      .flatten()
  }

  pub fn has_leading(&self, pos: SourcePos) -> bool {
    self.inner.leading.contains_key(&pos.as_byte_pos())
  }

  pub fn get_leading(&self, pos: SourcePos) -> Option<&Vec<Comment>> {
    self.inner.leading.get(&pos.as_byte_pos())
  }

  pub fn has_trailing(&self, pos: SourcePos) -> bool {
    self.inner.trailing.contains_key(&pos.as_byte_pos())
  }

  pub fn get_trailing(&self, pos: SourcePos) -> Option<&Vec<Comment>> {
    self.inner.trailing.get(&pos.as_byte_pos())
  }

  /// Gets the comments as an `SwcComments` trait.
  ///
  /// Calling this is fast because it uses a shared reference.
  pub fn as_swc_comments(&self) -> Box<dyn SwcComments> {
    Box::new(SwcMultiThreadedComments(self.clone()))
  }
}

// Don't want to expose this API easily, so someone should
// use the `.as_swc_comments()` above to access it.
struct SwcMultiThreadedComments(MultiThreadedComments);

impl SwcComments for SwcMultiThreadedComments {
  fn has_leading(&self, pos: SwcBytePos) -> bool {
    // It's ok to convert these byte positions to source
    // positions because we received them from swc and
    // didn't create them on their own.
    self.0.has_leading(SourcePos::unsafely_from_byte_pos(pos))
  }

  fn get_leading(&self, pos: SwcBytePos) -> Option<Vec<Comment>> {
    self
      .0
      .get_leading(SourcePos::unsafely_from_byte_pos(pos))
      .cloned()
  }

  fn has_trailing(&self, pos: SwcBytePos) -> bool {
    self.0.has_trailing(SourcePos::unsafely_from_byte_pos(pos))
  }

  fn get_trailing(&self, pos: SwcBytePos) -> Option<Vec<Comment>> {
    self
      .0
      .get_trailing(SourcePos::unsafely_from_byte_pos(pos))
      .cloned()
  }

  fn add_leading(&self, _pos: SwcBytePos, _cmt: Comment) {
    panic_readonly();
  }

  fn add_leading_comments(&self, _pos: SwcBytePos, _comments: Vec<Comment>) {
    panic_readonly();
  }

  fn move_leading(&self, _from: SwcBytePos, _to: SwcBytePos) {
    panic_readonly();
  }

  fn take_leading(&self, _pos: SwcBytePos) -> Option<Vec<Comment>> {
    panic_readonly();
  }

  fn add_trailing(&self, _pos: SwcBytePos, _cmt: Comment) {
    panic_readonly();
  }

  fn add_trailing_comments(&self, _pos: SwcBytePos, _comments: Vec<Comment>) {
    panic_readonly();
  }

  fn move_trailing(&self, _from: SwcBytePos, _to: SwcBytePos) {
    panic_readonly();
  }

  fn take_trailing(&self, _pos: SwcBytePos) -> Option<Vec<Comment>> {
    panic_readonly();
  }

  fn add_pure_comment(&self, _pos: SwcBytePos) {
    panic_readonly();
  }
}

fn panic_readonly() -> ! {
  panic!("MultiThreadedComments do not support write operations")
}

#[cfg(test)]
mod test {
  use crate::parse_module;
  use crate::swc::common::comments::SingleThreadedComments;
  use crate::MediaType;
  use crate::MultiThreadedComments;
  use crate::ParseParams;
  use crate::SourceTextInfo;
  use crate::StartSourcePos;

  #[test]
  fn general_use() {
    let (comments, start_pos) = get_single_threaded_comments("// 1\nt;/* 2 */");
    let comments = MultiThreadedComments::from_single_threaded(comments);

    // maps
    assert_eq!(comments.leading_map().len(), 1);
    assert_eq!(
      comments
        .leading_map()
        .get(&(start_pos + 5).as_byte_pos())
        .unwrap()[0]
        .text,
      " 1"
    );
    assert_eq!(comments.trailing_map().len(), 1);
    assert_eq!(
      comments
        .trailing_map()
        .get(&(start_pos + 7).as_byte_pos())
        .unwrap()[0]
        .text,
      " 2 "
    );

    // comment vector
    let comments_vec = comments.get_vec();
    assert_eq!(comments_vec.len(), 2);
    assert_eq!(comments_vec[0].text, " 1");
    assert_eq!(comments_vec[1].text, " 2 ");

    // comments trait
    assert!(comments.has_leading(start_pos + 5));
    assert!(!comments.has_leading(start_pos + 7));

    assert_eq!(comments.get_leading(start_pos + 5).unwrap()[0].text, " 1");
    assert!(comments.get_leading(start_pos + 7).is_none());

    assert!(!comments.has_trailing(start_pos + 5));
    assert!(comments.has_trailing(start_pos + 7));

    assert!(comments.get_trailing(start_pos + 5).is_none());
    assert_eq!(comments.get_trailing(start_pos + 7).unwrap()[0].text, " 2 ");
  }

  fn get_single_threaded_comments(
    text: &str,
  ) -> (SingleThreadedComments, StartSourcePos) {
    let module = parse_module(ParseParams {
      specifier: "file.ts".to_string(),
      text_info: SourceTextInfo::from_string(text.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .expect("expects a module");
    (
      module.comments().as_single_threaded(),
      module.text_info().range().start,
    )
  }
}
