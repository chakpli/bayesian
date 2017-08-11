package utils

import scala.io.Source
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse
import org.json4s.jvalue2extractable
import org.json4s.string2JsonInput
import model.DataModel.Query

/**
 * @author bli
 * @date Aug 1, 2017
 */

object Parser {
  
  def parseData(path:String):Array[Query] = {
    val data = Source.fromFile(path).getLines.mkString
    
    implicit val formats = DefaultFormats
    parse(data).extract[Array[Query]]
  }
  
  def main(arg: Array[String]):Unit = {
    parseData("./test.json")
  }
}