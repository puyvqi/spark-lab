package scala

import org.apache.spark.{SparkConf, SparkContext}

object sparkw {
  def main(agrs: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("l").setMaster("local")
    val sc = SparkContext.getOrCreate(conf)
    val rdd = sc.parallelize(Seq(1, 2, 3, 4, 5))
    rdd.foreach(println(_))
    val rdd2 = sc.parallelize(Seq(1, 1, 1, 1))
    rdd2.foreach(println(_))
    var l: Int = 0
    val res = rdd.map { case i =>
      l+=1
      println("te" + l)

      i
    }
    res.foreach(println(_))
  }

}
