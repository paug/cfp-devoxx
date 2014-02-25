/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013 Association du Paris Java User Group.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package controllers

import play.api.mvc._
import models._
import play.api.libs.json.{JsNull, Json}
import play.api.i18n.Messages
import scala.concurrent.Future
import play.api.mvc.SimpleResult

/**
 * A real REST api for men.
 * Created by nicolas on 25/02/2014.
 */

object RestAPI extends Controller {

  def index = UserAgentAction {
    implicit request =>
      Ok(views.html.RestAPI.index())
  }

  def profile(docName:String)=Action{
    implicit request=>

      docName match {
        case "speaker"=>Ok("Documentation on speaker")
        case "list-of-speakers"=>Ok("This resource describe a list of Speakers, using Links.")
        case "talk"=>Ok("Documentation on talk")
        case "conference"=>Ok("A conference object, has an eventCode, a label and links to other CFP objects such as speakers or talks.")
        case "conferences"=>Ok("This resource describe a list of Conferences, using Links.")
        case other=>NotFound("Sorry, no documentation for this profile")
      }
  }

  def showAllConferences() = UserAgentAction {
    implicit request =>
      import Conference.confFormat

      val conferences = Conference.all
      val etag = conferences.hashCode.toString

      request.headers.get(IF_NONE_MATCH) match {
        case Some(tag) if tag == etag => {
          NotModified
        }
        case other => {
          val jsonObject = Json.toJson(
            Map(
              "content" -> Json.toJson("All conferences"),
              "links"-> Json.toJson{
                 Conference.all.map{
                  conference:Conference=>
                    conference.link
                }
              }
            )
          )
          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,"Links"-> ("<"+routes.RestAPI.profile("conferences").absoluteURL().toString+">; rel=\"profile\""))
        }
      }
  }

  def redirectToConferences = UserAgentAction {
    implicit request =>
      Redirect(routes.RestAPI.showAllConferences())
  }

  def showConference(eventCode: String) = UserAgentAction {
    implicit request =>
      import Conference.confFormat

      Conference.find(eventCode).map {
        conference: Conference =>

          val etag = conference.eventCode.toString

          request.headers.get(IF_NONE_MATCH) match {
            case Some(tag) if tag == etag => {
              NotModified
            }
            case other => {
              val jsonObject = Json.toJson(
                Map(
                  "eventCode" -> Json.toJson(conference.eventCode),
                  "label" -> Json.toJson(conference.label),
                  "links" -> Json.toJson(List(
                    Link(
                      routes.RestAPI.showSpeakers(conference.eventCode).absoluteURL().toString,
                      routes.RestAPI.profile("speaker").absoluteURL().toString,
                      "See all speakers"),
                    Link(routes.RestAPI.profile("conference").absoluteURL().toString,"profile","Profile documentation for Conference")
                  ))
                )
              )
              Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag)
            }
          }
      }.getOrElse(NotFound("Conference not found"))
  }

  def showSpeakers(eventCode: String) = UserAgentAction {
    implicit request =>
      import Speaker.speakerFormat

      val speakers = Speaker.allSpeakersWithAcceptedTerms().sortBy(_.cleanName)
      val etag = speakers.hashCode.toString

      request.headers.get(IF_NONE_MATCH) match {
        case Some(tag) if tag == etag => {
          NotModified
        }
        case other => {

          val updatedSpeakers = speakers.map {
            speaker: Speaker =>
              Map(
                "uuid" -> Json.toJson(speaker.uuid),
                "firstName" -> speaker.firstName.map(Json.toJson(_)).getOrElse(JsNull),
                "lastName" -> speaker.name.map(Json.toJson(_)).getOrElse(JsNull),
                "avatarURL" -> speaker.avatarUrl.map(u => Json.toJson(u.trim())).getOrElse(JsNull),
                "links" -> Json.toJson(List(
                  Link(routes.RestAPI.showSpeaker(eventCode, speaker.uuid).absoluteURL().toString,
                    routes.RestAPI.profile("speaker").absoluteURL().toString,
                    speaker.cleanName)
                )
                )
              )
          }

          val jsonObject = Json.toJson(updatedSpeakers)

          Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag,
                                              "Links"-> ("<"+routes.RestAPI.profile("speaker-short").absoluteURL().toString+">; rel=\"profile\"")
                                              )
        }
      }
  }

  def redirectToSpeakers(eventCode: String) = UserAgentAction {
    implicit request =>
      Redirect(routes.RestAPI.showSpeakers(eventCode))
  }


  def showSpeaker(eventCode: String, uuid: String) = UserAgentAction {
    implicit request =>
      import Speaker.speakerFormat

      Speaker.findByUUID(uuid).map {
        speaker =>
          val etag = speaker.hashCode.toString

          request.headers.get(IF_NONE_MATCH) match {
            case Some(tag) if tag == etag => {
              NotModified
            }
            case other => {
              val acceptedProposals = ApprovedProposal.allAcceptedTalksForSpeaker(speaker.uuid)

              val updatedTalks = acceptedProposals.map {
                proposal: Proposal =>
                  val allSpeakers = proposal.allSpeakerUUIDs.flatMap {
                    uuid => Speaker.findByUUID(uuid)
                  }

                  Map(
                    "id" -> Json.toJson(proposal.id),
                    "link" -> Json.toJson(
                      Link(routes.RestAPI.showTalk(eventCode, proposal.id).absoluteURL().toString,
                      routes.RestAPI.profile("talk").absoluteURL().toString,
                      proposal.title
                      )

                    ),
                    "title" -> Json.toJson(proposal.title),
                    "track" -> Json.toJson(Messages(proposal.track.label)),
                    "talkType" -> Json.toJson(Messages(proposal.talkType.id)),
                    "speakers" -> Json.toJson(allSpeakers.map {
                      speaker =>
                        Map(
                          "link" -> Json.toJson(
                            Link(routes.RestAPI.showSpeaker(eventCode, speaker.uuid).absoluteURL().toString,
                            routes.RestAPI.profile("speaker").absoluteURL().toString,
                            speaker.cleanName
                          )),
                          "name" -> Json.toJson(speaker.cleanName)
                        )
                    })

                  )
              }

              val updatedSpeaker =
                Map(
                  "uuid" -> Json.toJson(speaker.uuid),
                  "firstName" -> speaker.firstName.map(Json.toJson(_)).getOrElse(JsNull),
                  "lastName" -> speaker.name.map(Json.toJson(_)).getOrElse(JsNull),
                  "avatarURL" -> speaker.avatarUrl.map(u => Json.toJson(u.trim())).getOrElse(JsNull),
                  "blog" -> speaker.blog.map(u => Json.toJson(u.trim())).getOrElse(JsNull),
                  "company" -> speaker.company.map(u => Json.toJson(u.trim())).getOrElse(JsNull),
                  "lang" -> speaker.lang.map(u => Json.toJson(u.trim())).getOrElse(Json.toJson("fr")),
                  "bio" -> Json.toJson(speaker.bio),
                  "twitter" -> speaker.company.map(u => Json.toJson(u.trim())).getOrElse(JsNull),
                  "acceptedTalks" -> Json.toJson(updatedTalks)
                )

              val jsonObject = Json.toJson(updatedSpeaker)
              Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag)
            }
          }
      }.getOrElse(NotFound("Speaker not found"))
  }

  def showTalk(eventCode: String, proposalId: String) = UserAgentAction {
    implicit request =>
      Proposal.findById(proposalId).map {
        proposal =>
          val etag = proposal.hashCode.toString

          request.headers.get(IF_NONE_MATCH) match {
            case Some(tag) if tag == etag => {
              NotModified
            }
            case other => {
              val allSpeakers = proposal.allSpeakerUUIDs.flatMap {
                uuid => Speaker.findByUUID(uuid)
              }

              val updatedProposal =
                Map(
                  "id" -> Json.toJson(proposal.id),
                  "title" -> Json.toJson(proposal.title),
                  "lang" -> Json.toJson(proposal.lang),
                  "summaryAsHtml" -> Json.toJson(proposal.summaryAsHtml),
                  "summary" -> Json.toJson(proposal.summary),
                  "track" -> Json.toJson(Messages(proposal.track.label)),
                  "talkType" -> Json.toJson(Messages(proposal.talkType.id)),
                  "speakers" -> Json.toJson(allSpeakers.map {
                    speaker =>
                      Map(
                        "link" -> Json.toJson(
                          Link(
                          routes.RestAPI.showSpeaker(eventCode, speaker.uuid).absoluteURL().toString,
                          routes.RestAPI.profile("speaker").absoluteURL().toString,
                          speaker.cleanName
                          )
                        ),
                        "name" -> Json.toJson(speaker.cleanName)
                      )
                  })
                )
              val jsonObject = Json.toJson(updatedProposal)
              Ok(jsonObject).as(JSON).withHeaders(ETAG -> etag)
            }
          }
      }.getOrElse(NotFound("Proposal not found"))
  }

  def redirectToTalks(eventCode: String) = UserAgentAction {
    implicit request =>
      Redirect(routes.RestAPI.showTalks(eventCode))
  }

  def showTalks(eventCode: String) = UserAgentAction {
    implicit request =>
      NotImplemented("Not yet implemented")
  }

}

object UserAgentAction extends ActionBuilder[Request] with play.api.http.HeaderNames {
  override protected def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[SimpleResult]): Future[SimpleResult] = {
    request.headers.get(USER_AGENT).collect {
      case some => {
        block(request)
      }
    }.getOrElse {
      Future.successful(play.api.mvc.Results.Forbidden("User-Agent is required to interact with Devoxx France API"))
    }
  }
}

case class Link(href:String, rel:String, title:String)

object Link{
    implicit val linkFormat=Json.format[Link]
}

case class Conference(eventCode:String, label:String, link:Link)

object Conference{

  implicit val confFormat = Json.format[Conference]

  def devoxxFrance2014(implicit req:RequestHeader)=Conference("devoxxFR2014",
                                  "Devoxx France 2014, 16 au 18 avril 2014",
                                  Link(
                                    routes.RestAPI.showConference("devoxxFR2014").absoluteURL().toString,
                                    routes.RestAPI.profile("conference").absoluteURL().toString,
                                    "See more details about Devoxx France 2014"
                                  ))

  def all(implicit req:RequestHeader)={
    List(devoxxFrance2014)
  }

  // Super fast, super crade, super je m'en fiche pour l'instant
  def find(eventCode:String)(implicit req:RequestHeader):Option[Conference]=Option(devoxxFrance2014)

}