
import net.liftweb.json.JsonDSL._
import net.liftweb.json._

import scala.collection.breakOut
import scala.reflect.ClassTag
import scala.xml._


val customParser =
  XML.withSAXParser(new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl()
    .newSAXParser())
val fpath = "/Users/joshuaarnold/Documents/MyApps/faculty-stats/resources" +
  "/html/allFaculty/A_names.html"
val elem = customParser.loadFile(fpath)

val tables = elem \\ "table" filter (x => (x \ "@id" text)
  == "ctl00_ContentPlaceHolder1_dlFaculty")

val rows = tables \\ "tr"

val deb  = rows.head

val json =
  ("name" -> "a") ~
    ("focus" -> "b") ~
    ("nhood" -> "c")

val myMap: Map[String, String] = Map("a" -> "one")

val foo = myMap.updated("b", "two")

val bar = foo ++ Map("c" -> "three") ++ Map()

/**
  * Parses the tbody element from a Degrees or Titles table
  *
  * @param tbody element containing sequence of <tr>
  * @return Map[String, List[String]) of column header -> values
  */
def parseTableBody(tbody: NodeSeq): Map[String, List[String]] = {

  val trs = tbody \\ "tr"

  // Stores all th and td values in column major order and transposes
  val values = (for {
    tr <- trs
    if tr \\ "td" nonEmpty
  } yield (tr \\ "td" toList) map (_ text) ) toList

  val cols = (for {
    th <- trs \\ "th"
  } yield th.text) toList

  val lists = (cols :: values) transpose

  val acc0: Map[String,List[String]] = Map()
  val maps =
    (lists map {
      case List() => Map()
      case x :: xs => Map(x -> xs)
    }).foldLeft(acc0)(_ ++ _)
  maps
}

def extract[T: ClassTag](t: T): Map[String, List[String]] = {
  t match {
    case n: Node => {
      val id = (n \ "@id" text)
      n match {
        case <span>{ s }</span> => id match {
          case "ctl00_ContentPlaceHolder1_dlFaculty_ctl00_lblName" =>
            Map("name" -> List(s.text))
          case "ctl00_ContentPlaceHolder1_dlFaculty_ctl00_Label2" =>
            Map("year" -> List(s.text))
          case _ =>
            Map()
        }
        case <table>{ _* }</table> => id match {
          case _ => {
            //Map("?" -> List(n \\ "tr" text))
            parseTableBody(n)
          }
        }
        case _ => extract(n child)
      }
    }
    case s: Seq[Node@unchecked] => s match {
      case Seq() => Map()
      case Seq(x, xs@_*) => extract(x) ++ extract(xs)
    }
    case _ => throw new java.util.InputMismatchException("must be node ")
  }
}

println(pretty(render(extract(deb))))

val joe = (deb \\ "table" head ) \\ "tr"

