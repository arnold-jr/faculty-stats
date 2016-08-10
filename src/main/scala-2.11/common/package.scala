/**
  * Created by joshuaarnold on 8/9/16.
  */

import java.io._

package object common {
  def outputWriter[T](fName: String, seq: Seq[T])(f: T => String): Unit = {
    val file = new File(fName)
    val bw = new BufferedWriter(new FileWriter(file))
    for (x <- seq) {
      bw.write(f(x))
    }
    bw.close()
  }

}
