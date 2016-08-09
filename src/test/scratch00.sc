import scala.xml._
import scala.xml.transform._
import scala.reflect.{ClassTag, classTag}
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import java.util.regex.{Pattern}

val foo = XML.withSAXParser(new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl()
  .newSAXParser())
val fpath = "/Users/joshuaarnold/Documents/MyApps/faculty-stats/resources" +
  "/html/engineeringFaculty/engineeringFaculty.html"
val elem = foo.loadFile(fpath)

val tables = elem \\ "table" filter (x => (x \ "@id" text) == "peoplelisting")

val meat = tables \\ "tr" \\ "td" filter (x => x \ "h4" nonEmpty)

val names = meat map (_ \\ "a" text)
val allText = meat map (_ text)

(names zip allText) map ({ case (n,t) => (n, t.stripPrefix(n)) })


val removeIt = new RewriteRule {
  override def transform(n: Node): NodeSeq = n match {
    case e: Elem if e \ "h4" nonEmpty => NodeSeq.Empty
    case n => n
  }
}

// new RuleTransformer(removeIt).transform(meat)

val deb = meat.head
val joe = deb.child


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
    val nodeText = node text

    val keysRE = ((for {
      k <- keys
    } yield (Pattern quote k concat "(.*)")) mkString).r

    val fields = (for {
      m <- keysRE.findAllIn(nodeText).matchData
      g <- 1 to 3
    } yield m.group(g)) toList

    (keys, fields)
  }
}

class Faculty(val name: String, val focus: String, val nhood: String)

val debbie = Faculty(deb)

val mark = new Faculty("Mark","resiliency","big data")

val json =
  ("faculty" ->
    ("name" -> mark.name) ~
    ("focus" -> mark.focus) ~
    ("nhood" -> mark.nhood)
    )

println(pretty(render(json)))
println(compactRender(json))
compactRender(json)
render(json)

def getFields(keys: List[String], s: String): List[String] = {
  val keyRE = ((for (i <- keys) yield i concat "(.*)") mkString) r

  s match {
    case keyRE(f1) => List(f1)
    case keyRE(f1, f2) => List(f1,f2)
    case keyRE(f1, f2, f3) => List(f1,f2,f3)
    case _ => Nil
  }
}
val a = "arm"
val b = "arm enia"

val keys = List("ar","m e")
val keyRE = ((for (i <- keys) yield i concat "(.*)") mkString).r

b match {
  case keyRE(f1, f2) => (f1, f2)
  case _ => "null"
}

getFields(keys, b)
val nodeText = deb text
val nKeys = List("Mark Abkowitz", "Research Focus:", "Intellectual Neighborhoods:")
val nKeyRE = ((for {
  i <- nKeys
} yield Pattern quote(i) concat "(.*)") mkString).r
nodeText match {
  case nKeyRE(f1) => List(f1)
  case nKeyRE(f1, f2) => List(f1,f2)
  case nKeyRE(f1, f2, f3) => List(f1,f2,f3)
  case x => x
}

val matches = (for {
  m <- nKeyRE.findAllIn(nodeText).matchData
  g <- 1 to 3
} yield m.group(g)) toList

matches.length