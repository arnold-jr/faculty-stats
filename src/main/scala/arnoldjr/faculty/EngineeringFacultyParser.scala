package arnoldjr.faculty

import scala.xml.{Elem, Node, NodeSeq}

/**
  * Created by joshuaarnold on 7/10/17.
  */
object EngineeringFacultyParser extends App {

  /**
    * Create a Faculty instance given a <tr> element
    *
    * @param row
    * @return
    */
  def parseRow(row: Node): String = {

    val name = row \\ "a" text

    val columns = (row \\ "td").filter(td => (td \ "h4").length > 0)

    // TODO: check length of columns == 1
    val pairs = columns.head.child.filter(c => !List("h4", "br").contains(c.label))
        .map(n => n text)
        .sliding(2)

    val faculty = pairs.foldLeft(EngrFaculty(name)){
      case (b: EngrFaculty, Seq(k: String, v: String)) =>
      k match {
        case "Research Focus:" => b.copy(focus = v)
        case "Intellectual Neighborhoods:" => b.copy(neighborhood = v)
        case _ => b
      }
      case (b: EngrFaculty, _) => b
    }
    faculty.toString
  }

  def getRows(root: Node): NodeSeq =
    (elem \\
      "table"
      filter (tb => (tb \ "@id" text) == "peoplelisting")) \\
      "tr"

  val elem: Elem = HTMLParser.loadFile(
    getClass
        .getResource("/html/engineeringFaculty/engineeringFaculty.html")
        .getPath
  )

  for (r <- getRows(elem)) println(parseRow(r))

}
