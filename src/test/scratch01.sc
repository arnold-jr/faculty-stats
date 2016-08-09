import scala.collection.breakOut

val deb = List("a","b","c")

deb match {
  case List(a) => a
  case List(a, b, c) => b
}
deb.indexOf ("b")

val dict: Map[String, Int]  = (deb zip List(1, 2, 3))(breakOut)
dict("a")