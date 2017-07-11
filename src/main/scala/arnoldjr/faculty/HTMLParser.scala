package arnoldjr.faculty

import scala.xml.{Elem, XML}
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl

/**
  * Created by joshuaarnold on 7/10/17.
  */
object HTMLParser {
  val customParser =
    XML.withSAXParser(new SAXFactoryImpl().newSAXParser())

  def loadFile(path: String): Elem = customParser.loadFile(path)
}
