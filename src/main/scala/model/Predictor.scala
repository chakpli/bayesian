package model

import model.DataModel.Query
import model.DataModel.Engagment
import utils.Math
import scala.collection.mutable.ArrayBuffer
import scala.math._
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter

/**
 * @author bli
 * @date Aug 1, 2017
 */
 
case class Predictor(t: Trainer) {
  
  def computePrediction(engagements: Array[Engagment]): String = {
    val labels = t.labels match { case None => throw new Exception("no labels"); case Some(s) => s}
    val probabilityC = t.probabilityC match { case None => throw new Exception("no model PC"); case Some(s) => s}
    val probabilityNGivenC = t.probabilityNGivenC match { case None => throw new Exception("no model PN|C"); case Some(s) => s}
    val ProbabilityXGivenNCPosition = t.ProbabilityXGivenNCPosition match { case None => throw new Exception("no model PPosition|N,C"); case Some(s) => s}
    val ProbabilityXGivenNCClicks = t.ProbabilityXGivenNCClicks match { case None => throw new Exception("no model PClicks|N,C"); case Some(s) => s}
    val ProbabilityXGivenNCviews = t.ProbabilityXGivenNCViews match { case None => throw new Exception("no model PViews|N,C"); case Some(s) => s}
    
    val positions = new Array[Int](engagements.length)
    val clicks = new Array[Int](engagements.length)
    val views = new Array[Int](engagements.length)
    engagements.zipWithIndex.map{case (v, i) => 
      positions(i) = v.position
      clicks(i) = v.clicks
      views(i) = v.views}
    
    val probs = new Array[Double](labels.length)
    
    labels.zipWithIndex.map{case (v,i)=>{

      val pc = probabilityC.predict(v)
      //println("cat: " + v + " logpc:" + pc)
      val pnc = probabilityNGivenC.predict((v, engagements.length))
      //println("cat: " + v + " logpnc:" + pnc)
      val ppnc = ProbabilityXGivenNCPosition.predict((v, positions))
      //println("cat: " + v + " logppnc:" + ppnc)
      val pcnc = ProbabilityXGivenNCClicks.predict((v, clicks))
      //println("cat: " + v + " logpcnc:" + pcnc)
      val pvnc = ProbabilityXGivenNCviews.predict((v, views))
      //println("cat: " + v + " logpvnc:" + pvnc)
      probs(i) = pc + pnc + ppnc + pcnc + pvnc
      //println("cat: " + v + " logprob:" + probs(i))
      }}

    labels(Math.max(probs)._1)
  }

  def predict(query: Array[Query]): Unit = {
    val absoluteError = new ArrayBuffer[Double]()
    var correctCount = 0
    
    var bw:BufferedWriter = null

    try{
    query.map(
      q => q.impressions.map(
        i => {
          val engagements = i.engagements
          val prediction = computePrediction(engagements)
          val positions = new Array[Int](engagements.length)
          val clicks = new Array[Int](engagements.length)
          val views = new Array[Int](engagements.length)
          engagements.zipWithIndex.map {
            case (v, i) =>
              positions(i) = v.position
              clicks(i) = v.clicks
              views(i) = v.views
          }
          println("queryid:" + q.query + " resultid:" + i.result + " avg position:" + Math.avg(positions) + " avg clicks" + Math.avg(clicks) + " avg views" + Math.avg(views))
          println("queryid:" + q.query + " resultid:" + i.result + " prediction:" + prediction + " vs original label:" + i.label)
          i.label match {
            case None => 
              if (bw ==null){
                val f = new File("./new_label.txt")
                f.delete()
                bw = new BufferedWriter(new FileWriter(f))
              }
              bw.write("queryid:" + q.query + " resultid:" + i.result + " avg position:" + Math.avg(positions) + " avg clicks" + Math.avg(clicks) + " avg views" + Math.avg(views) +"\n")
              bw.write("queryid:" + q.query + " resultid:" + i.result + " prediction:" + prediction + "\n")
            case Some(l) =>
              absoluteError.append(abs(prediction.toDouble - l.toDouble))
              if (prediction == l) correctCount += 1
          }
        }))
    }finally{
      if (bw != null)
        bw.close()
    }
    
    absoluteError.length match {
      case 0 =>
      case _ =>
        var t = 0.0
        absoluteError.map(d => t += d)
        println("mean absolute error:" + t / absoluteError.length)
        println("accuracy:" + correctCount * 1.0 / absoluteError.length)
    }
  }
}