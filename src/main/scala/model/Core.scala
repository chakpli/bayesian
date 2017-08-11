package model

import scala.math.pow
import scala.math.E
import scala.math.log
import scala.collection.mutable.HashMap
import utils.Math
import scala.collection.mutable.ArrayBuffer

/**
 * @author bli
 * @date Aug 2, 2017
 */
 
abstract class Probability[D, T, P](data: D) {
  var params:Option[T] = None
  
  def train(): Unit = {
    params = Some(trainParams())
  }
  
  def trainParams(): T
  
  def predict(dataToPredict: P): Double = {
    val p = params match {case None => throw new Exception("no model"); case Some(p) => p}
    _predict(p, dataToPredict) match {case None => throw new Exception("no prediction"); case Some(c)=>c}
  }
  
  def _predict(p: T, dataToPredict: P):Option[Double]
} 

case class ProbabilityXGivenNC(data: Map[String, Array[Array[Int]]], byPercentile: Boolean, bin:Int=5, binSz:Int=1, initProb:Double=0.5) extends 
Probability[Map[String, Array[Array[Int]]], Map[String, (Array[Int], Array[Double])], (String, Array[Int])](data){

  
  def _predict(p: Map[String, (Array[Int], Array[Double])], dataToPredict: (String, Array[Int])):Option[Double] ={
    val category = dataToPredict._1
    val percentile = p.get(category) match {case None => return None; case Some(c) => c._1}
    //print("percentile: ")
    //percentile.map(x => print(x + " "))
    //println()
    val estimate = p.get(category) match {case None => return None; case Some(c) => c._2}
    //print("estimate: ")
    //estimate.map(x => print(x + " "))
    //println()
    val data = dataToPredict._2
    val n = data.length
    
    var prob = Math.logFactorial(n)
    var count:Option[Array[Int]] = None //= new Array[Int](bin)
    
    data.zipWithIndex.map {
      case (v, i) =>
        val _idx = byPercentile match {
          case true =>
            if (count.isEmpty) count = Some(new Array[Int](percentile.length))
            Math.search(v, percentile)
          case false =>
            if (count.isEmpty) count = Some(new Array[Int](bin))
            val p = percentile
            if (v < p(0)) 0
            else if (v > p(p.length - 1)) p.length - 1
            else v / binSz
        }
        count get _idx += 1
    }

    count.get.zipWithIndex.map{case (v, i) => prob += (log (pow(estimate(i), v))-Math.logFactorial(v))}
    Some(prob)
  }
  
  
  def trainParams(): Map[String, (Array[Int], Array[Double])] = {
    val percentile = HashMap[String, Array[Int]]()
    val estimate = HashMap[String, Array[Double]]()

    byPercentile match {
      case true =>
        val __data = HashMap[String, HashMap[Int, Int]]()
        data.map {
          case (x, y) => {
            __data.get(x) match { case None => __data.put(x, new HashMap[Int, Int]())}
            y.map(a => a.map(_a => __data.get(x).get.get(_a) match {
              case None => __data.get(x).get.put(_a, 1)
              case Some(s) => __data.get(x).get.put(_a, s+1)}))
          }
        }

        __data.map {
          case (x, hm) =>
            val a = new ArrayBuffer[(Int, Int)]()
            hm.map { case (k, v) => a.append((v, k)) }
            val (_percentile, _estimate) = Math.buildHDR(initProb, a.toArray)
            percentile.put(x, _percentile)
            estimate.put(x, _estimate)
        }
      case false =>
        data.map { case (k, v) => percentile.put(k, (0 until bin).map(i=>i*binSz).toArray) }
    }

    byPercentile match {
      case true =>
      case false =>
        data.map {
          case (x, y) => {
            val sumCount = new Array[Int](bin)
            val totalprob = new Array[Double](bin)
            var totalCount = 0

            y.map(
              i => {
                val count = new Array[Int](bin)
                i.map(
                  j => {
                    val p = percentile.get(x).get
                    val idx =

                      if (j < p(0)) 0
                      else if (j > p(p.length - 1)) p.length - 1
                      else j / binSz

                    count(idx) += 1
                    totalCount += 1
                  })
                count.zipWithIndex.map { case (v, idx) => sumCount(idx) += count(idx) }
              })

            sumCount.zipWithIndex.map { case (v, idx) => totalprob(idx) = (sumCount(idx) + 1) * 1.0 / (totalCount + bin) }
            estimate.put(x, totalprob)
          }
        }
    }
    
    val param = HashMap[String, (Array[Int], Array[Double])]()
    estimate.map{case (x,y) => param.put(x, (percentile.get(x).get,y))}
    //println("xnc params trained: ")
    //param.map{ case (k, v) => println("cat:" + k); print("percentile:") ; v._1.map(x => print(x + " ")); println(); print("estimate:") ; v._2.map(x => print(x + " ")); println(); }
    param.toMap
  }
}

case class ProbabilityNGivenC(data: Map[String, (Int, Int)]) extends 
Probability[Map[String, (Int, Int)], Map[String, Double], (String, Int)](data){

  /**
   * @param data sum and count data by category
   */
  def trainParams() = {
    val _map = new HashMap[String,Double]()
    data.map{case (k,(sum,total)) => _map.put(k, sum*1.0/total - 1)}
    println("nc params trained: " + _map)
    _map.toMap
  }
  
  /*
   * @param dataToPredict category and integer
   */
  def _predict(p: Map[String, Double], dataToPredict: (String, Int)):Option[Double] = {
    val lamda = p.get(dataToPredict._1) match {case None=> return None; case Some(s) => s} 
    Some(-lamda + (dataToPredict._2 -1) * log(lamda)  - Math.logFactorial(dataToPredict._2 -1))
  }
}

case class ProbabilityC(data: Map[String, Int]) extends Probability[Map[String, Int], Map[String, Double], String](data){
  
  /**
   * @param data count data by category
   */
  def trainParams() = {
    var total = 0.0D
    data.map{case (_,v) => total += v }
    val _map = new HashMap[String,Double]()
    data.map{case (k,v) => _map.put(k, v/total)}
    println("c params trained: " + _map)
    _map.toMap
  }
  
  def _predict(p: Map[String, Double], dataToPredict: String) = {
    p.get(dataToPredict) match {case None=> None; case Some(c)=> Some(log(c))}
    
  }
}