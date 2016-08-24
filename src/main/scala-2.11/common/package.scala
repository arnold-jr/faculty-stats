/**
  * Created by joshuaarnold on 8/9/16.
  */

import scala.tools.nsc.io.{File, Path}

package object common {

  def outputWriter[T](fName: String, seq: Seq[T])(f: T => String): Unit = {
    File(fName).writeAll(seq map f mkString "\n" )
  }
}
