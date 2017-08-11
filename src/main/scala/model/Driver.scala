package model

import utils.Parser


/**
 * @author bli
 * @date Aug 2, 2017
 */
 
object Driver {
  def main(arg: Array[String]):Unit = {

    val trainData = Parser.parseData("./train.json")
    val t = Trainer()
    t.train(trainData)
    
    Predictor(t).predict(trainData) //insample prediction
    
    val testData = Parser.parseData("./test.json")
    Predictor(t).predict(testData) //outsample prediction
  }
}