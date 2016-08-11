import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import scala.xml.Elem


val joe = <tbody>
  <tr class="ResultsHeader">
    <th scope="col">Degree</th> <th scope="col">Institution</th> <th scope="col">Location</th> <th scope="col">Year Received</th>
  </tr> <tr class="ResultsRow">
    <td>M.D.</td> <td>Tufts University School of Medicine</td> <td>Boston, Massachusetts</td> <td>1991</td>
  </tr> <tr class="ResultsAltRow">
    <td>B.A.</td> <td>Brown University</td> <td>Providence, Rhode Island</td> <td>1987</td>
  </tr>
</tbody>

val trs = joe \\ "tr"

val values = (for {
  tr <- trs
  if tr \\ "td" nonEmpty
} yield (tr \\ "td" toList) map (_ text) ) toList

val cols = (for {
  th <- trs \\ "th"
} yield th.text) toList

val deb = (cols :: values) transpose

val acc0: Map[String,List[String]] = Map()
val lesa =
  (deb map { case x :: xs => Map(x -> xs) })
  .foldLeft(acc0)(_ ++ _)

compact(render(lesa))

println(pretty(render(lesa)))

def parseTableBody(tbody: scala.xml.Elem) = {

  val trs = tbody \\ "tr"

  // Stores all th and td values in column major order and transposes
  val values = (for {
    tr <- trs
    if tr \\ "td" nonEmpty
  } yield (tr \\ "td" toList) map (_ text) ) toList

  val cols = (for {
    th <- trs \\ "th"
  } yield th.text) toList

  val listValues = (cols :: values) transpose

  val acc0: Map[String,List[String]] = Map()
  val mapValues =
    (listValues map { case x :: xs => Map(x -> xs) })
      .foldLeft(acc0)(_ ++ _)
  mapValues
}
