package model

import scala.collection.mutable.HashMap
import scala.math._
import utils.Math
import scala.collection.mutable.ArrayBuffer
import model.DataModel.Query
import scala.collection.mutable.HashSet

/**
 * @author bli
 * @date Aug 1, 2017
 */

case class Trainer() {
  var labels:Option[Array[String]] = None
  var probabilityC: Option[ProbabilityC] = None
  var probabilityNGivenC: Option[ProbabilityNGivenC] = None
  var ProbabilityXGivenNCPosition: Option[ProbabilityXGivenNC] = None
  var ProbabilityXGivenNCClicks: Option[ProbabilityXGivenNC] = None
  var ProbabilityXGivenNCViews: Option[ProbabilityXGivenNC] = None
  
  def train(query: Array[Query]): Unit = {
    val catCountMap = HashMap[String, Int]()
    val nSumCountMap = HashMap[String, (Int, Int)]()
    val positionMap = HashMap[String, ArrayBuffer[Array[Int]]]()
    val clicksMap = HashMap[String, ArrayBuffer[Array[Int]]]()
    val viewsMap = HashMap[String, ArrayBuffer[Array[Int]]]()
    val _positionMap = HashMap[String, Array[Array[Int]]]()
    val _clicksMap = HashMap[String, Array[Array[Int]]]()
    val _viewsMap = HashMap[String, Array[Array[Int]]]()

    var _labels = HashSet[String]()
    
    query.map(
      q => q.impressions.map(
        i => {
          val n = i.engagements.length
          val label = i.label.get
          _labels.add(label)
          catCountMap.get(label) match {
            case None    => catCountMap.put(label, 1)
            case Some(c) => catCountMap.put(label, c + 1)
          }
          nSumCountMap.get(label) match {
            case None =>
              nSumCountMap.put(label, (n, 1))
            case Some(c) => nSumCountMap.put(label, (c._1 + n, c._2 + 1))
          }
          val position = new Array[Int](n)
          val clicks = new Array[Int](n)
          val views = new Array[Int](n)
          i.engagements.zipWithIndex.map{case (j, idx) => 
            position(idx) = j.position
            clicks(idx) = j.clicks
            views(idx) = j.views
            
          }
          positionMap.get(label) match {
            case None =>
              val ab = new ArrayBuffer[Array[Int]]()
              ab.append(position)
              positionMap.put(label, ab)
            case Some(c) => c.append(position)
          }
          positionMap.map{case (k,v) => _positionMap.put(k, v.toArray)}
          
          clicksMap.get(label) match {
            case None =>
              val ab = new ArrayBuffer[Array[Int]]()
              ab.append(clicks)
              clicksMap.put(label, ab)
            case Some(c) => c.append(clicks)
          }
          clicksMap.map{case (k,v) => _clicksMap.put(k, v.toArray)}
          
          viewsMap.get(label) match {
            case None =>
              val ab = new ArrayBuffer[Array[Int]]()
              ab.append(views)
              viewsMap.put(label, ab)
            case Some(c) => c.append(views)
          }
          viewsMap.map{case (k,v) => _viewsMap.put(k, v.toArray)}
          
        }))
        
    val labelAry = new Array[String](_labels.size)
    _labels.zipWithIndex.map{case (v, idx) => labelAry(idx) = v}
    
    labels = Some(labelAry)
    println("training pc...")
    probabilityC = Some(ProbabilityC(catCountMap.toMap))
    probabilityC.get.train()
    println("training pnc...")
    probabilityNGivenC = Some(ProbabilityNGivenC(nSumCountMap.toMap))
    probabilityNGivenC.get.train()
    println("training ppnc...")
    ProbabilityXGivenNCPosition = Some(ProbabilityXGivenNC(_positionMap.toMap, false, bin=10, binSz=3))
    ProbabilityXGivenNCPosition.get.train()
    println("training pcnc...")
    ProbabilityXGivenNCClicks = Some(ProbabilityXGivenNC(_clicksMap.toMap, false, bin=5, binSz=2))
    ProbabilityXGivenNCClicks.get.train()
    println("training pvnc...")
    ProbabilityXGivenNCViews = Some(ProbabilityXGivenNC(_viewsMap.toMap, false, bin=5, binSz=5))
    ProbabilityXGivenNCViews.get.train()
  }
}