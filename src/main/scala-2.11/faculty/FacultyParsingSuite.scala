package faculty
/**
  * Created by joshuaarnold on 8/8/16.
  */

import common._
import java.util.regex.Pattern

import net.liftweb.json.JsonDSL._
import net.liftweb.json._

import scala.collection.breakOut
import scala.reflect.ClassTag
import scala.xml._

object FacultyParsingSuite {

  def parseEngineeringFaculty() = {

    class Faculty(val name: String, val focus: String, val nhood: String) {

      def getJson(): String = {
        val json =
          ("name" -> name) ~
            ("focus" -> focus) ~
            ("nhood" -> nhood)

        compactRender(json)
      }

    }


    object Faculty {

      def apply(node: Node) = {
        def extract[T: ClassTag](t: T): List[String] = {
          t match {
            case n: Node => n match {
              case <a>{_*}</a> => List(n text)
              case <strong>{_*}</strong> => List(n text)
              case <td>{_*}</td> => extract(n child)
              case _ => extract(n child)
            }
            case s: Seq[Node@unchecked] => s match {
              case Seq() => Nil
              case Seq(x, xs@_*) => extract(x) ++ extract(xs)
            }
            case _ => throw new java.util.InputMismatchException("must be node ")
          }
        }

        val keys = extract(node)

        val keysRE = ((for {
          k <- keys
        } yield (Pattern quote k concat "(.*)")) mkString).r

        val nKeys = keys length
        val nodeText = node text
        val fields = (for {
          m <- keysRE.findAllIn(nodeText).matchData
          g <- 1 to nKeys
        } yield m.group(g)) toList

        val dict: Map[String, String] = (keys zip fields)(breakOut)
        val out = new Faculty(keys(0),
          dict.getOrElse("Research Focus:", ""),
          dict.getOrElse("Intellectual Neighborhoods:", ""))
        out
      }
    }



    val fpath = "/Users/joshuaarnold/Documents/MyApps/faculty-stats/resources" +
      "/html/engineeringFaculty/engineeringFaculty.html"
    val elem = customParser.xmlToHtml.loadFile(fpath)

    val tables = elem \\ "table" filter (x => (x \ "@id" text) == "peoplelisting")

    val meat = tables \\ "tr" \\ "td" filter (x => x \ "h4" nonEmpty)

    val jsonLines = meat map (x => Faculty(x)) map (_.getJson())

    outputWriter("tmp/engineeringFaculty.json", jsonLines)(x => x.concat("\n"))

    jsonLines foreach println
  }




  def parseAllFaculty(fpath: String) = {


    /**
      * Parses the table element from a Degrees or Titles table
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

      val acc0: Map[String, List[String]] = Map()
      val maps =
        (lists map {
          case List() => Map()
          case x :: xs => Map(x.toLowerCase() -> xs)
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
            case <table>{ _* }</table> => parseTableBody(n)
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

    def parseOuterTable(outerTable: NodeSeq): String = {
      val jin = extract(outerTable) map { case (s, xs) =>
        if (xs.length == 1)
          (s, xs.head) else (s, xs) }

      val (a, b) = extract(outerTable) partition(_._2.length==1)

      val j1 = parse(compactRender(a map {case (s, xs) => (s, xs.head)}))
      val j2 = parse(compactRender(b))

      val out = compactRender(j1 merge j2)

      out
    }

    val customParser =
      XML.withSAXParser(new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl()
        .newSAXParser())

    val elem = customParser.loadFile(fpath)

    val tables = elem \\ "table" filter (x => (x \ "@id" text)
      == "ctl00_ContentPlaceHolder1_dlFaculty")

    val rows = tables \\ "tr"

    val jsonLines = rows map parseOuterTable filterNot (_ == "{}")

    outputWriter("tmp/allFaculty.json", jsonLines)(x => x.concat("\n"))
  }


  object customParser {
    val xmlToHtml =
      XML.withSAXParser(new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl()
        .newSAXParser())
  }


  def main(args: Array[String]): Unit = {
    // parseEngineeringFaculty()


    val fpath = "/Users/joshuaarnold/Documents/MyApps/faculty-stats/resources" +
      "/html/allFaculty/A_names.html"

    parseAllFaculty(fpath)
  }


}
