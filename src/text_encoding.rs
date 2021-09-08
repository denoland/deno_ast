pub const BOM_CHAR: char = '\u{FEFF}';

/// Strips the byte order mark if it exists from the provided text in place.
pub fn strip_bom_mut(text: &mut String) {
  if text.starts_with(BOM_CHAR) {
    text.drain(..BOM_CHAR.len_utf8());
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn strip_bom_mut_with_bom() {
    let mut text = format!("{}text", BOM_CHAR);
    strip_bom_mut(&mut text);
    assert_eq!(text, "text");
  }

  #[test]
  fn strip_bom_mut_without_bom() {
    let mut text = "text".to_string();
    strip_bom_mut(&mut text);
    assert_eq!(text, "text");
  }
}
