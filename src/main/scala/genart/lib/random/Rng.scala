package genart.lib.random

import genart.lib.requireFinite
import genart.lib.math.lerp
import scala.annotation.tailrec

final case class Rng private (private val state: Long):
  def nextLong: (Long, Rng) =
    val next = Rng(state + 0x9e3779b97f4a7c15L)
    (Rng.mix(next.state), next)

  def nextDouble: (Double, Rng) =
    val (bits, next) = nextLong
    ((bits >>> 11).toDouble / 9007199254740992.0, next)

  def uniform(low: Double, high: Double): (Double, Rng) =
    requireFinite(low, "low")
    requireFinite(high, "high")
    require(low < high, "low must be less than high")
    val (amount, next) = nextDouble
    (
      math.max(low, math.min(lerp(low, high, amount), math.nextDown(high))),
      next
    )

  def nextInt(bound: Int): (Int, Rng) =
    require(bound > 0, "bound must be positive")
    @tailrec def sample(rng: Rng): (Int, Rng) =
      val (random, next) = rng.nextLong
      val bits = random >>> 1
      val value = bits % bound
      if bits - value + (bound - 1).toLong < 0L then sample(next)
      else (value.toInt, next)
    sample(this)

object Rng:
  def apply(seed: Seed): Rng = new Rng(seed.bits)

  private[genart] def mix(value: Long): Long =
    val first = (value ^ (value >>> 30)) * 0xbf58476d1ce4e5b9L
    val second = (first ^ (first >>> 27)) * 0x94d049bb133111ebL
    second ^ (second >>> 31)
