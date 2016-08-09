/**
  * Created by joshuaarnold on 8/9/16.
  */
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql.{Row, SparkSession}
import org.apache.spark.sql.types.{StructType, StructField}
import org.apache.spark.ml.feature.Word2Vec
import java.io._

import scala.xml._
import scala.util.Try


object AnalyzeFaculty {



  def main(args: Array[String]) {
    if (args.length != 4) {
      System.err.println(
        "Usage: SimpleApp <master> <postsFile> <votesFile> <usersFile>")
      System.exit(1)
    }
    val postsFile = args(1)
    val votesFile = args(2)
    val usersFile = args(3)

    SparkContextSingleton.setMaster(args(0))


    // getUpvoteRatioByFavorites(postsFile, votesFile)

    synonyms(postsFile)

    SparkContextSingleton.stopSparkContext()
  }

  def synonyms(postsFile: String): Unit = {

    val spark = SparkSessionSingleton.getInstance(SparkContextSingleton.conf)
    import spark.implicits._


    lazy val tagsDF = spark.read.json("tmp/engineeringFaculty.json")


    // Learn a mapping from words to Vectors.
    val word2Vec = new Word2Vec()
      .setInputCol("tags")
      .setOutputCol("result")
      .setVectorSize(100)
      .setMinCount(0)
    val model = word2Vec.fit(tagsDF)
    val resultDF = model.findSynonyms("hadoop", 50)

    if (SparkContextSingleton.debug) {
      resultDF take 50 foreach println
    }

    /*
    outputWriter("tmp/hadoodSynonyms.csv",
      resultDF take 50)({
      r: Row => r(0) + "," + r(1) + ",\n"
    })
    */
  }


  object SparkContextSingleton {
    val conf = new SparkConf()
      .setAppName("Simple Application")
      .setMaster("local[*]")
    val sc = new SparkContext(conf)
    val debug = true

    def setMaster(master: String) = conf.setMaster(master)

    def stopSparkContext() = sc.stop()
  }


  /** Lazily instantiated singleton instance of SparkSession */
  object SparkSessionSingleton {

    @transient private var instance: SparkSession = _

    def getInstance(sparkConf: SparkConf): SparkSession = {
      if (instance == null) {
        instance = SparkSession
          .builder
          .config(sparkConf)
          .getOrCreate()
      }
      instance
    }
  }

}
