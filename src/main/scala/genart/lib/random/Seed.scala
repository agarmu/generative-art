package genart.lib.random

final case class Seed private (private[genart] val bits: Long):
  def toHex: String =
    (60 to 0 by -4).iterator
      .map(shift => Seed.HexDigits(((bits >>> shift) & 15L).toInt))
      .mkString

  def derive(label: String): Seed =
    val prefix =
      (56 to 0 by -8).iterator.map(shift => ((bits >>> shift) & 255L).toInt)
    val hash = (prefix ++ Seed.utf8(label)).foldLeft(0xcbf29ce484222325L) {
      (hash, byte) => (hash ^ byte.toLong) * 0x100000001b3L
    }
    Seed.fromLong(Rng.mix(hash))
  override def toString: String = s"Seed($toHex)"

object Seed:
  private val HexDigits = "0123456789abcdef"

  def fromLong(bits: Long): Seed = Seed(bits)

  def fromHex(hex: String): Seed =
    require(hex.length == 16, "seed must contain exactly 16 hexadecimal digits")
    fromLong(hex.foldLeft(0L) { (bits, char) =>
      val digit = char match
        case c if c >= '0' && c <= '9' => c - '0'
        case c if c >= 'a' && c <= 'f' => c - 'a' + 10
        case c if c >= 'A' && c <= 'F' => c - 'A' + 10
        case _                         =>
          throw new IllegalArgumentException(
            "seed must contain only hexadecimal digits"
          )
      (bits << 4) | digit.toLong
    })

  private def utf8(label: String): Iterator[Int] =
    Iterator
      .unfold(0) { index =>
        Option.when(index < label.length) {
          val char = label(index).toInt
          if char >= 0xd800 && char <= 0xdbff then
            require(
              index + 1 < label.length,
              "label contains an unpaired surrogate"
            )
            val low = label(index + 1).toInt
            require(
              low >= 0xdc00 && low <= 0xdfff,
              "label contains an unpaired surrogate"
            )
            (0x10000 + ((char - 0xd800) << 10) + low - 0xdc00, index + 2)
          else
            require(
              char < 0xdc00 || char > 0xdfff,
              "label contains an unpaired surrogate"
            )
            (char, index + 1)
        }
      }
      .flatMap {
        case point if point < 0x80  => Iterator.single(point)
        case point if point < 0x800 =>
          Iterator(0xc0 | (point >>> 6), 0x80 | (point & 0x3f))
        case point if point < 0x10000 =>
          Iterator(
            0xe0 | (point >>> 12),
            0x80 | ((point >>> 6) & 0x3f),
            0x80 | (point & 0x3f)
          )
        case point =>
          Iterator(
            0xf0 | (point >>> 18),
            0x80 | ((point >>> 12) & 0x3f),
            0x80 | ((point >>> 6) & 0x3f),
            0x80 | (point & 0x3f)
          )
      }
