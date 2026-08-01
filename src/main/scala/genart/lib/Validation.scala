package genart.lib

def requireFinite(value: Double, name: String = "value"): Unit =
  require(!value.isNaN && !value.isInfinity, s"$name must be finite")

def requireUnit(value: Double, name: String = "value"): Unit =
  require(value >= 0.0 && value <= 1.0, s"$name must be finite and in [0,1]")
