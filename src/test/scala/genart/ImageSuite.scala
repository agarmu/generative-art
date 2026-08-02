package genart

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

class ImageSuite extends munit.FunSuite:
  private def image =
    new Image(2, 2):
      protected def read(x: Int, y: Int): Color =
        Color.rgb(x.toDouble, y.toDouble, 0.0)

  test(
    "image reads delegate valid coordinates and reject out-of-bounds access"
  ) {
    val image = new Image(2, 2):
      protected def read(x: Int, y: Int): Color =
        Color.rgb(x.toDouble, y.toDouble, 0.0)
    assert(image(0, 1) == Color.rgb(0.0, 1.0, 0.0))
    assert(image(1, 0) == Color.rgb(1.0, 0.0, 0.0))
    for (x, y) <- List((-1, 0), (0, -1), (2, 0), (0, 2)) do
      intercept[IndexOutOfBoundsException](image(x, y))
  }

  test("images require positive dimensions") {
    intercept[IllegalArgumentException] {
      new Image(0, 1):
        protected def read(x: Int, y: Int): Color = Color.rgb(0.0, 0.0, 0.0)
    }
  }

  test("loading stays pending until decoding finishes") {
    val decoded = Promise[Image]()
    val loader = new ImageLoader:
      protected def decode(source: String): Future[Image] =
        assert(source == "photo.png")
        decoded.future
    val pending = loader.load("photo.png")
    assert(pending.value.isEmpty)
    val expected = image
    decoded.success(expected)
    pending.map(actual => assert(actual eq expected))
  }

  test("invalid sources and decoder failures fail the returned future") {
    val failure = new RuntimeException("decode")
    val failed = new ImageLoader:
      protected def decode(source: String): Future[Image] =
        Future.failed(failure)
    assert(failed.load("missing.png").value.get.failed.get eq failure)
    intercept[IllegalArgumentException](failed.load("").value.get.get)
    val throwing = new ImageLoader:
      protected def decode(source: String): Future[Image] = throw failure
    assert(throwing.load("broken.png").value.get.failed.get eq failure)
  }
