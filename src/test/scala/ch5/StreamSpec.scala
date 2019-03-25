package ch5
import org.scalatest.FlatSpec

object StreamSpec extends FlatSpec {
  "A Stream" should "be able to convert to list" in {
    val stream  = Stream(1,2)
    val list = stream.toList
    assert(list.head == 1)
  }
}
