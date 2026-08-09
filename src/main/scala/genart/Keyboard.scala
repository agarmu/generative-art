package genart

enum Key:
  case Character(value: String)
  case ArrowLeft, ArrowRight, ArrowUp, ArrowDown
  case Space, Enter, Escape, Tab, Backspace, Delete, Insert
  case Home, End, PageUp, PageDown
  case Shift, Control, Alt, Meta, CapsLock, NumLock, ScrollLock
  case Function(number: Int)

object Key:
  private[genart] def fromName(name: String): Option[Key] = name match
    case "ArrowLeft"  => Some(Key.ArrowLeft)
    case "ArrowRight" => Some(Key.ArrowRight)
    case "ArrowUp"    => Some(Key.ArrowUp)
    case "ArrowDown"  => Some(Key.ArrowDown)
    case " "          => Some(Key.Space)
    case "Enter"      => Some(Key.Enter)
    case "Escape"     => Some(Key.Escape)
    case "Tab"        => Some(Key.Tab)
    case "Backspace"  => Some(Key.Backspace)
    case "Delete"     => Some(Key.Delete)
    case "Insert"     => Some(Key.Insert)
    case "Home"       => Some(Key.Home)
    case "End"        => Some(Key.End)
    case "PageUp"     => Some(Key.PageUp)
    case "PageDown"   => Some(Key.PageDown)
    case "Shift"      => Some(Key.Shift)
    case "Control"    => Some(Key.Control)
    case "Alt"        => Some(Key.Alt)
    case "Meta"       => Some(Key.Meta)
    case "CapsLock"   => Some(Key.CapsLock)
    case "NumLock"    => Some(Key.NumLock)
    case "ScrollLock" => Some(Key.ScrollLock)
    case value if value.length > 1 && value.startsWith("F") =>
      value
        .drop(1)
        .toIntOption
        .filter(n => n >= 1 && n <= 24)
        .map(Key.Function.apply)
    case value if value.length == 1 && !value.head.isControl =>
      Some(Key.Character(value.head.toLower.toString))
    case _ => None

final case class KeyModifiers(
    shift: Boolean = false,
    control: Boolean = false,
    alt: Boolean = false,
    meta: Boolean = false
)

enum KeyEvent:
  case Pressed(
      key: Key,
      modifiers: KeyModifiers = KeyModifiers(),
      repeat: Boolean = false
  )
  case Released(key: Key, modifiers: KeyModifiers = KeyModifiers())
  case Cancelled
