package model

/**
 * @author bli
 * @date Aug 1, 2017
 */

object DataModel {

  case class Query(query: String, impressions: Array[Result])

  case class Result(result: String, engagements: Array[Engagment], label: Option[String])

  case class Engagment(position: Int, clicks: Int, views: Int)

}