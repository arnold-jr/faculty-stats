import arnoldjr.faculty.HTMLParser
import scala.xml.Node

val elem = HTMLParser.loadFile("/Users/joshuaarnold/MyApps/faculty-stats/src/" +
    "main/resources/html/engineeringFaculty/engineeringFaculty.html")

val anchors = (elem \\
    "table"
    filter (tb => (tb \ "@id" text) == "peoplelisting")) \\
    "tr" \\
    "a"

for (a <- anchors) yield a.text

