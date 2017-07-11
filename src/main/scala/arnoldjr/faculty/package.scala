package arnoldjr

/**
  * Created by joshuaarnold on 7/11/17.
  */
package object faculty {

  final case class EngrFaculty(name: String = "",
                               focus: String = "",
                               neighborhood: String = "")

  // (title, school)
  type Title = (String, String)

  // (degree, institution, location, year)
  type Degree = (String, String, String, Int)

  final case class AllFaculty(name: String = "",
                              yearHired: Int = -1,
                              titleSchools: List[Title] = List.empty[Title],
                              degrees: List[Degree] = List.empty[Degree])
}
