package genart.processing

import genart.{Key, KeyEvent, KeyModifiers}
import java.awt.event.{KeyEvent as AwtKey}
import processing.event.{KeyEvent as NativeKey}

private[processing] final class ProcessingKeyboardInput(emit: KeyEvent => Unit):
  private var pressed = Map.empty[(Int, Int), Key]

  private def identity(event: NativeKey): (Int, Int) =
    val location = event.getNative match
      case native: AwtKey => native.getKeyLocation
      case _              => 0
    (event.getKeyCode, location)

  private def modifiers(event: NativeKey): KeyModifiers =
    KeyModifiers(
      event.isShiftDown,
      event.isControlDown,
      event.isAltDown,
      event.isMetaDown
    )

  private def key(event: NativeKey): Option[Key] = event.getKeyCode match
    case AwtKey.VK_LEFT        => Some(Key.ArrowLeft)
    case AwtKey.VK_RIGHT       => Some(Key.ArrowRight)
    case AwtKey.VK_UP          => Some(Key.ArrowUp)
    case AwtKey.VK_DOWN        => Some(Key.ArrowDown)
    case AwtKey.VK_SPACE       => Some(Key.Space)
    case AwtKey.VK_ENTER       => Some(Key.Enter)
    case AwtKey.VK_ESCAPE      => Some(Key.Escape)
    case AwtKey.VK_TAB         => Some(Key.Tab)
    case AwtKey.VK_BACK_SPACE  => Some(Key.Backspace)
    case AwtKey.VK_DELETE      => Some(Key.Delete)
    case AwtKey.VK_INSERT      => Some(Key.Insert)
    case AwtKey.VK_HOME        => Some(Key.Home)
    case AwtKey.VK_END         => Some(Key.End)
    case AwtKey.VK_PAGE_UP     => Some(Key.PageUp)
    case AwtKey.VK_PAGE_DOWN   => Some(Key.PageDown)
    case AwtKey.VK_SHIFT       => Some(Key.Shift)
    case AwtKey.VK_CONTROL     => Some(Key.Control)
    case AwtKey.VK_ALT         => Some(Key.Alt)
    case AwtKey.VK_META        => Some(Key.Meta)
    case AwtKey.VK_CAPS_LOCK   => Some(Key.CapsLock)
    case AwtKey.VK_NUM_LOCK    => Some(Key.NumLock)
    case AwtKey.VK_SCROLL_LOCK => Some(Key.ScrollLock)
    case code if code >= AwtKey.VK_F1 && code <= AwtKey.VK_F12 =>
      Some(Key.Function(code - AwtKey.VK_F1 + 1))
    case code if code >= AwtKey.VK_F13 && code <= AwtKey.VK_F24 =>
      Some(Key.Function(code - AwtKey.VK_F13 + 13))
    case code =>
      val character = event.getKey
      if character != '\uffff' && !character.isControl then
        Key.fromName(character.toString)
      else if code >= AwtKey.VK_A && code <= AwtKey.VK_Z then
        Some(Key.Character(('a' + code - AwtKey.VK_A).toChar.toString))
      else None

  def down(event: NativeKey): Unit = synchronized {
    val code = identity(event)
    pressed.get(code).orElse(key(event)).foreach { value =>
      val repeat = event.isAutoRepeat || pressed.contains(code)
      pressed = pressed.updated(code, value)
      emit(KeyEvent.Pressed(value, modifiers(event), repeat))
    }
  }

  def up(event: NativeKey): Unit = synchronized {
    if !event.isAutoRepeat then
      val code = identity(event)
      pressed.get(code).foreach { value =>
        pressed -= code
        emit(KeyEvent.Released(value, modifiers(event)))
      }
  }

  def cancel(): Unit = synchronized {
    pressed = Map.empty
    emit(KeyEvent.Cancelled)
  }
