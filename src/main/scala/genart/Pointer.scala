package genart

import genart.lib.math.Vec

enum PointerButton:
  case Primary, Middle, Secondary

enum PointerEvent:
  case Moved(position: Vec[2])
  case Pressed(position: Vec[2], button: PointerButton)
  case Released(position: Vec[2], button: PointerButton)
  case Cancelled
