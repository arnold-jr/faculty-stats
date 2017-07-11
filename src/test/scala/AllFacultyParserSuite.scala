import org.scalatest.WordSpec
import arnoldjr.faculty.AllFacultyParser._

/**
  * Created by joshuaarnold on 7/11/17.
  */
class AllFacultyParserSuite extends WordSpec {

  "extractFromTable" should {

    "return an empty list" in {
      assert(
        extractFromTable(<table></table>) == List.empty[List[String]]
      )
    }

    "return a the correct list of lists" in {
      val tb = <table>
        <caption>
          Appointments
        </caption><tr class="ResultsHeader">
        <th scope="col">Title</th><th scope="col">School</th>
      </tr><tr class="ResultsRow">
        <td>Associate Professor of Neurological Surgery</td>
        <td>School of Medicine</td>
      </tr><tr class="ResultsAltRow">
        <td>Assistant Professor of Orthopaedic Surgery and Rehabilitation</td>
        <td>School of Medicine</td>
      </tr>
      </table>

      assert(
        extractFromTable(tb) == List(
          List("Associate Professor of Neurological Surgery",
            "School of Medicine"
          ),
          List("Assistant Professor of Orthopaedic Surgery and Rehabilitation",
            "School of Medicine"
          )
        )
      )
    }


  }
}
