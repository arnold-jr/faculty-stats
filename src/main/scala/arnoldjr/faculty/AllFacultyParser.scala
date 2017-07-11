package arnoldjr.faculty

import scala.util.Try
import scala.xml.{Elem, Node, NodeSeq, Text}

/**
  * Created by joshuaarnold on 7/11/17.
  */
object AllFacultyParser extends App {

  /**
    * Extract <td> text from a <table>
    *
    * @param table <table> element with <tr class != "ResultsHeader">
    * @return List of Lists of <td> text
    */
  def extractFromTable(table: Node): List[List[String]] = {
    (table \ "tr")
        .filter(tr => (tr \ "@class" text) != "ResultsHeader")
        .map(tr => (tr \ "td").map(td => td text).toList)
        .toList
  }

  /**
    * Create a Faculty instance given a <td> element
    *
    * @param row
    * @return
    */
  def parseRow(row: Node): AllFaculty = {

    /**
      * Perform depth-first search to extract content from an XML node
      *
      * @param node XML node to be matched against
      * @param acc faculty record
      * @return faculty record
      */
    def extract(node: Node, acc: AllFaculty): AllFaculty = {
      node match {
        case sp @ <span>{_ *}</span> =>
          val id = sp \ "@id" text

          if (id.endsWith("_lblName")) {
            acc.copy(name = sp.text)
          } else if (id.endsWith("_Label2")) {
            acc.copy(yearHired = sp.text.toInt)
          } else {
            acc
          }
        case table @ <table>{ch @ _*}</table> =>
          val id = table \ "@id" text

          if (id endsWith "_gvTitles") {
            val titleSchools = extractFromTable(table)
                .flatMap{
                  case t :: s :: Nil => Some(t, s)
                  case _ => None
                }
            acc.copy(titleSchools = titleSchools)
          } else if (id endsWith "_gvDegrees") {
            val degrees = extractFromTable(table)
                .flatMap{
                  case d :: i :: l :: y :: Nil => {
                    val year: Int = Try(y.toInt) getOrElse -1
                    Some(d, i, l, year)
                  }
                  case _ => None
                }
            acc.copy(degrees = degrees)
          } else {
            acc
          }
        case _ =>
          if (node.child.isEmpty) {
            acc
          } else {
            // Visit all children and extract
            node.child.foldLeft(acc)((b, n) => extract(n, b))
          }
      }
    }

    extract(row, AllFaculty())
  }

  def getRows(root: Node): NodeSeq = (elem \\ "td").filter(n => {
    val spanElems = n \ "span"
    spanElems.length match {
      case 0 => false
      case _ => spanElems.head.attribute("class") match {
        case Some(Text("SectionHead")) => true
        case _ => false
      }
    }
  })

  val elem: Elem = HTMLParser.loadFile(
    getClass
        .getResource("/html/allFaculty/A_names.html")
        .getPath
  )

  val rows = getRows(elem)

  println(rows.head)
  for (r <- getRows(elem)) println(parseRow(r))

}

