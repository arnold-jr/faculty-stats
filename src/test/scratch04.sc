import java.io.File

val deb = "ctl00_ContentPlaceHolder1_dlFaculty_ctl00_lblName"


val pattern = "(ctl00_ContentPlaceHolder1_dlFaculty_ctl\\d+_lblName)".r

pattern.findFirstIn(deb)


deb match {
  case pattern(_) => "yes"
  case _ => "oh"
}

val parentDir = new File("/Users/joshuaarnold/Documents/MyApps/" +
  "faculty-stats/resources/html/allFaculty/")

val fpaths = parentDir.listFiles.filter(_.isFile).toList