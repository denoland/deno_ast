pub const BOM_CHAR: char = '\u{FEFF}';

/// Strips the byte order mark if it exists from the provided text in place.
pub fn strip_bom_mut(text: &mut String) {
  if text.starts_with(BOM_CHAR) {
    text.drain(..BOM_CHAR.len_utf8());
  }
}

/// Strips the byte order mark from the provided text if it exists.
#[cfg(feature = "transpiling")]
pub fn strip_bom(text: &str) -> &str {
  if text.starts_with(BOM_CHAR) {
    &text[BOM_CHAR.len_utf8()..]
  } else {
    text
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

  #[test]
  #[cfg(feature = "transpiling")]
  fn strip_bom_with_bom() {
    let text = format!("{}text", BOM_CHAR);
    assert_eq!(strip_bom(&text), "text");
  }

  #[test]
  #[cfg(feature = "transpiling")]
  fn strip_bom_without_bom() {
    let text = "text".to_string();
    assert_eq!(strip_bom(&text), "text");
  }
}
