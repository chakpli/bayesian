package utils

import util.control.Breaks._
import scala.math.log
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedHashMap

/**
 * @author bli
 * @date Aug 1, 2017
 */
 
object Math {
  def factorial(n: Int): Int = {
    if (n == 0)
      1

    var fact = 1
    (1 to n).map(i =>
      fact *= i)
    fact
  }
  
  def logFactorial(n: Int): Double = {
    if (n == 0 || n == 1)
      0

    var fact = 0.0
    (2 to n).map(i =>
      fact += log(i))
    fact
  }
  
  def buildHDR(initProb:Double, d:Array[Int]): (Array[Int], Array[Double]) = {
    val hm = new HashMap[Int, Int]()
    d.map(i => hm.get(i) match {case None => hm.put(i, 1); case Some(s) => hm.put(i, s+1)})
    val a = new ArrayBuffer[(Int, Int)]()
    hm.map{case (k, v) => a.append((v, k))} 
    buildHDR(initProb, a.toArray) 
  }
  
  /**
   * @param d Array of (count, value)
   */
  def buildHDR(initProb:Double, d:Array[(Int, Int)]): (Array[Int], Array[Double]) = {
    
    val l = new LinkedHashMap[Int, Double]()
    val dSorted = d.sorted.reverse
    
    var sum = 0.0
    dSorted.map(i => sum+= i._1)
    
    var curSum = 0
    var count = 0
    var countCut = Int.MaxValue
    val ary = new ArrayBuffer[Int]()

    for (idx <- (0 until dSorted.length)) {
      val d = dSorted(idx)
      count += 1
      curSum += d._1
      ary.append(d._2)
      val currentProb = curSum / sum
      println(currentProb)
      if (currentProb >= initProb || count >= countCut || idx == dSorted.length - 1) {
        ary.map(i => l.put(i, currentProb))
        ary.clear()
        curSum = 0
        countCut = count
        count = 0
      }
    }
    val a = l.toArray
    val _a = a.sorted
    val interval = new Array[Int](a.length + 1)
    interval(a.length) = Int.MaxValue
    val estimate = new Array[Double](a.length + 1)
    estimate(0) = _a(0)._2
    
    _a.zipWithIndex.map{
      case ((k, v), idx) => interval(idx) = k; estimate(idx+1) = v}
    
    print("interval:")
    interval.map(i => print(i + " "))
    println
    print("estimate:")
    estimate.map(i => print(i + " "))
    println
    
    (interval , estimate)
  } 
  
  def search(s: Int, a: Array[Int]): Int = {
    var right = a.length - 1
    var left = 0
    var mid = (right + left) / 2
    
    breakable {
      while (true) {
        if (s == a(mid) || (s > a(mid) && s < a(mid + 1))) break
        else if (s < a(0)) {return 0}
        
        if (s > a(mid)) {left = mid}
        else {right = mid}
        mid = (right + left) / 2
      }
    }
    mid + 1
  }
  
  def avg(s: Array[Int]): Double = {
    var sum = 0.0
    s.map(i => sum += i)
    sum/s.length
  } 
  
  def max(s: Array[Double]): (Int, Double) = {
    var idx = 0
    var max = s(idx)
    
    (1 until s.length).map(x => if(s(x) > max) {idx = x; max = s(idx)})
    (idx, max)
  }
  
  def main(arg: Array[String]):Unit = {
    val (interval, estimate) = buildHDR(0.5, Array(3,3,3,2,1,4,3,1,2,2))
    println("interval:")
    interval.map(i => print(i + " "))
    println("estimate:")
    estimate.map(i => print(i + " "))
    println("p for 0: " + estimate(search(0, interval)))
    println("p for 1: " + estimate(search(1, interval)))
    println("p for 2: " + estimate(search(2, interval)))
    println("p for 3: " + estimate(search(3, interval)))
    println("p for 4: " + estimate(search(4, interval)))
    println("p for 5: " + estimate(search(5, interval)))
    
    /*
     * 0 -> 0
     * 2 -> 1
     * 3 -> 1
     * 4 -> 2
     * 6 -> 3
     * 7 -> 3
     */
    println(search(0, Array(2,4,6,Int.MaxValue)))
    println(search(2, Array(2,4,6,Int.MaxValue)))
    println(search(3, Array(2,4,6,Int.MaxValue)))
    println(search(4, Array(2,4,6,Int.MaxValue)))
    println(search(6, Array(2,4,6,Int.MaxValue)))
    println(search(7, Array(2,4,6,Int.MaxValue)))
    println(search(11, Array(3,6,9,13,19, Int.MaxValue)))

    
  }
  
}