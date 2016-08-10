package faculty

/**
  * Created by joshuaarnold on 8/9/16.
  */
import common._
import org.apache.spark.sql.{Row, SparkSession}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.ml.feature.{Word2Vec, StopWordsRemover}

object AnalyzeFaculty {

  def main(args: Array[String]) {
    if (args.length != 2) {
      System.err.println(
        "Usage: AnalyzeFaculty <master> <engineeringFacultyFile>")
      System.exit(1)
    }
    val engrFile = args(1)

    SparkContextSingleton.setMaster(args(0))

    synonyms(engrFile)

    SparkContextSingleton.stopSparkContext()
  }

  /**
    * Learns a mapping from words to Vectors
    *
    * @param engrFile String specifying path to json-lined resource file
    */
  def synonyms(engrFile: String): Unit = {

    val spark = SparkSessionSingleton.getInstance(SparkContextSingleton.conf)
    lazy val facultyDF = spark.read.json("tmp/engineeringFaculty.json")

    import spark.implicits._

    facultyDF.show()
    println(facultyDF.count())

    // Register the DataFrame as a temporary view
    facultyDF.createOrReplaceTempView("faculty")

    lazy val focusDF = spark.sql("SELECT focus FROM faculty WHERE LENGTH(focus)>0")
    focusDF.show()
    println(focusDF.count())

    // Transform String to Tuple1[List[String]]
    val focusSplitDF = focusDF.map(r => Tuple1(r(0).toString
      .replaceAll("""[\p{Punct}]"""," ").split("\\s+").filterNot(_ == ""))
    ).toDF("focus")

    focusSplitDF.show()
    println(focusSplitDF.count())

    // Remove stop words
    val remover = new StopWordsRemover()
      .setInputCol("focus")
      .setOutputCol("filtered")

    val dataSet = remover.transform(focusSplitDF)


    // Learn a mapping from words to Vectors.
    val word2Vec = new Word2Vec()
      .setInputCol("filtered")
      .setOutputCol("result")
      .setVectorSize(100)
      .setMinCount(0)
    val model = word2Vec.fit(dataSet)
    val resultDF = model.findSynonyms("data", 50)

    if (SparkContextSingleton.debug) {
      resultDF take 50 foreach println
    }

    outputWriter("tmp/dataSynonyms.csv",
      resultDF take 50)({
      r: Row => r(0) + "," + r(1) + ",\n"
    })
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
