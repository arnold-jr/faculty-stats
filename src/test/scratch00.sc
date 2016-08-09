import scala.xml.XML
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl


val foo = XML.withSAXParser(new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl()
  .newSAXParser())
val fpath = "/Users/joshuaarnold/Documents/MyApps/faculty-stats/resources" +
  "/html/engineeringFaculty/engineeringFaculty.xml"
val elem = foo.loadFile(fpath)

val tables = elem \\ "table" \ "@id"


