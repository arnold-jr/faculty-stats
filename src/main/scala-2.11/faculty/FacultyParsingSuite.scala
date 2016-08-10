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
  def main(args: Array[String]): Unit = {
    parseXML()
  }

  def parseXML() = {
    val customParser =
      XML.withSAXParser(new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl()
      .newSAXParser())
    val fpath = "/Users/joshuaarnold/Documents/MyApps/faculty-stats/resources" +
      "/html/engineeringFaculty/engineeringFaculty.html"
    val elem = customParser.loadFile(fpath)

    val tables = elem \\ "table" filter (x => (x \ "@id" text) == "peoplelisting")

    val meat = tables \\ "tr" \\ "td" filter (x => x \ "h4" nonEmpty)

    val jsonLines = meat map(x => Faculty(x)) map (_.getJson())

    outputWriter("tmp/engineeringFaculty.json", jsonLines)(x => x.concat("\n"))

    jsonLines foreach println
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

  class Faculty(val name: String, val focus: String, val nhood: String) {

    def getJson(): String = {
      val json =
          ("name" -> name) ~
          ("focus" -> focus) ~
          ("nhood" -> nhood)

      compactRender(json)
    }

  }

}
