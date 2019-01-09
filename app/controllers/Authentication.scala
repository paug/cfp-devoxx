/**
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

import java.math.BigInteger
import java.security.SecureRandom

import models._
import notifiers.TransactionalEmails
import org.apache.commons.codec.binary.Base64
import org.apache.commons.codec.digest.DigestUtils
import org.apache.commons.lang3.{RandomStringUtils, StringUtils}
import org.joda.time.{DateTime, DateTimeZone}
import pdi.jwt.{Jwt, JwtAlgorithm}
import play.api.Play
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.i18n.Messages
import play.api.libs.Crypto
import play.api.libs.json.Json
import play.api.libs.ws._
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * Signup and Signin.
  *
  * Author: nicolas martignole
  * Created: 27/09/2013 09:59
  */
object Authentication extends Controller {
  val loginForm = Form(tuple("email" -> (email verifying nonEmpty), "password" -> nonEmptyText))

  def forgetPassword = Action {
    implicit request =>
      Ok(views.html.Authentication.forgetPassword(emailForm))
  }

  val emailForm = Form("email" -> (email verifying(nonEmpty, maxLength(50))))

  def doForgetPassword() = Action {
    implicit request =>
      emailForm.bindFromRequest.fold(
        errorForm => BadRequest(views.html.Authentication.forgetPassword(errorForm)),
        validEmail => {
          if (Webuser.isEmailRegistered(validEmail)) {
            val resetURL = routes.Authentication.resetPassword(Crypto.sign(validEmail.toLowerCase.trim), new String(Base64.encodeBase64(validEmail.toLowerCase.trim.getBytes("UTF-8")), "UTF-8")).absoluteURL(ConferenceDescriptor.isHTTPSEnabled)
            TransactionalEmails.sendResetPasswordLink(validEmail, resetURL)
            Redirect(routes.Application.index()).flashing("success" -> Messages("forget.password.confirm"))
          } else {
            Redirect(routes.Authentication.forgetPassword()).flashing("error" -> Messages("forget.password.notfound"))
          }
        })
  }

  def resetPassword(t: String, a: String) = Action {
    implicit request =>
      val email = new String(Base64.decodeBase64(a), "UTF-8")
      if (Crypto.sign(email) == t) {
        val futureMaybeWebuser = Webuser.findByEmail(email)
        futureMaybeWebuser.map {
          w =>
            val newPassword = Webuser.changePassword(w) // it is generated
            Ok(views.html.Authentication.resetPassword(loginForm.fill((w.email, newPassword)), newPassword))
        }.getOrElse {
          Redirect(routes.Application.index()).flashing("error" -> "Sorry, this email is not registered in your system.")
        }
      } else {
        Redirect(routes.Application.index()).flashing("error" -> "Sorry, we could not validate your authentication token. Are you sure that this email is registered?")
      }
  }

  def login(visitor: Boolean) = Action {
    implicit request =>
      loginForm.bindFromRequest.fold(
        invalidForm => {
          if (visitor)
            BadRequest(views.html.Application.homeVisitor(invalidForm))
          else
            BadRequest(views.html.Application.home(invalidForm))
        },
        validForm =>
          Webuser.checkPassword(validForm._1, validForm._2) match {
            case Some(webUser) if visitor =>
              val cookie = createCookie(webUser)
              Redirect(routes.Publisher.homePublisher()).withSession("uuid" -> webUser.uuid).withCookies(cookie)
            case Some(webUser) =>
              val cookie = createCookie(webUser)
              Redirect(routes.CallForPaper.homeForSpeaker()).flashing("warning" -> Messages("cfp.reminder.proposals")).withSession("uuid" -> webUser.uuid).withCookies(cookie)
            case None if visitor =>
              Redirect(routes.Application.homeVisitor()).flashing("error" -> Messages("login.error"))
            case None =>
              Redirect(routes.Application.home()).flashing("error" -> Messages("login.error"))
          }
      )
  }

  def logout = Action {
    implicit request =>
      val discardingCookie = DiscardingCookie("cfp_rm", "/", None, secure = false)
      Redirect(routes.Application.index()).discardingCookies(discardingCookie).withNewSession
  }

  def githubLogin(visitor: Boolean) = Action {
    implicit request =>
      Play.current.configuration.getString("github.client_id").map {
        clientId: String =>
          val redirectUri = routes.Authentication.callbackGithub(visitor).absoluteURL(ConferenceDescriptor.isHTTPSEnabled)
          val gitUrl = "https://github.com/login/oauth/authorize?scope=user:email&client_id=" + clientId + "&state=" + Crypto.sign("ok") + "&redirect_uri=" + redirectUri
          Redirect(gitUrl)
      }.getOrElse {
        InternalServerError("github.client_id is not set in application.conf")
      }
  }

  //POST https://github.com/login/oauth/access_token
  val oauthForm = Form(tuple("code" -> text, "state" -> text))
  val accessTokenForm = Form("access_token" -> text)

  def callbackGithub(visitor: Boolean) = Action.async {
    implicit request =>
      oauthForm.bindFromRequest.fold(invalidForm => {
        Future.successful(BadRequest(views.html.Application.home(invalidForm)).flashing("error" -> "Invalid form"))
      }, {
        case (code, state) if state == Crypto.sign("ok") =>
          val auth = for (clientId <- Play.current.configuration.getString("github.client_id");
                          clientSecret <- Play.current.configuration.getString("github.client_secret")) yield (clientId, clientSecret)
          auth.map {
            case (clientId, clientSecret) =>
              val url = "https://github.com/login/oauth/access_token"
              val wsCall = WS.url(url).post(Map("client_id" -> Seq(clientId), "client_secret" -> Seq(clientSecret), "code" -> Seq(code)))
              wsCall.map {
                result =>
                  result.status match {
                    case 200 =>
                      val b = result.body
                      try {
                        val accessToken = b.substring(b.indexOf("=") + 1, b.indexOf("&"))
                        Redirect(routes.Authentication.createFromGithub(visitor)).withSession("access_token" -> accessToken)
                      } catch {
                        case e: IndexOutOfBoundsException =>
                          Redirect(routes.Application.index()).flashing("error" -> "access token not found in query string")
                      }
                    case _ =>
                      Redirect(routes.Application.index()).flashing("error" -> ("Could not complete Github OAuth, got HTTP response" + result.status + " " + result.body))
                  }
              }
          }.getOrElse {
            Future.successful(InternalServerError("github.client_secret is not configured in application.conf"))
          }
        case other => Future.successful(BadRequest(views.html.Application.home(loginForm)).flashing("error" -> "Invalid state code"))
      })
  }

  val importSpeakerForm = Form(tuple(
    "email" -> email,
    "firstName" -> nonEmptyText(maxLength = 50),
    "lastName" -> nonEmptyText(maxLength = 50),
    "bio" -> nonEmptyText(maxLength = 750),
    "company" -> optional(text),
    "twitter" -> optional(text),
    "blog" -> optional(text),
    "avatarUrl" -> optional(text),
    "qualifications" -> nonEmptyText(maxLength = 380000),
    "linkedin" -> optional(text)
  ))

  val newWebuserForm: Form[Webuser] = Form(
    mapping(
      "email" -> (email verifying nonEmpty),
      "firstName" -> nonEmptyText(maxLength = 50),
      "lastName" -> nonEmptyText(maxLength = 50)
    )(Webuser.createSpeaker)(Webuser.unapplyForm))

  val newVisitorForm: Form[Webuser] = Form(
    mapping(
      "email" -> (email verifying nonEmpty),
      "firstName" -> nonEmptyText(maxLength = 50),
      "lastName" -> nonEmptyText(maxLength = 50)
    )(Webuser.createVisitor)(Webuser.unapplyForm))

  val webuserForm = Form(
    mapping(
      "uuid" -> ignored(""),
      "email" -> (email verifying nonEmpty),
      "firstName" -> nonEmptyText(maxLength = 50),
      "lastName" -> nonEmptyText(maxLength = 50),
      "password" -> nonEmptyText(maxLength = 50),
      "profile" -> nonEmptyText
    ) {
      (uuid, email, firstName, lastName, password, profile) =>
        Webuser(uuid,
          email,
          firstName,
          lastName,
          password,
          profile)
    } {
      w =>
        Some(
          (w.uuid,
            w.email,
            w.firstName,
            w.lastName,
            w.password,
            w.profile)
        )
    }
  )

  def createFromGithub(visitor: Boolean) = Action.async {
    implicit request =>

      val url = "https://api.github.com/user?access_token=" + request.session.get("access_token").getOrElse("")
      val futureResult = WS.url(url).withHeaders("User-agent" -> ("CFP " + ConferenceDescriptor.current().conferenceUrls.cfpHostname), "Accept" -> "application/json").get()
      futureResult.map {
        result =>
          result.status match {
            case 200 =>
              val json = Json.parse(result.body)
              val resultParse = for (email <- json.\("email").asOpt[String].toRight("github.importprofile.error.emailnotfound").right;
                                     name <- json.\("name").asOpt[String].toRight("github.importprofile.error.namenotfound").right)
                yield (email, name)

              resultParse.fold(missingField =>
                Redirect(routes.Application.home()).flashing(
                  "error" -> List("github.importprofile.error", missingField, "github.importprofile.error.advice").map(Messages(_)).mkString(" ")), {
                case (emailS, nameS) =>
                  /* bio : "Recommendation: Do not use this attribute. It is obsolete." http://developer.github.com/v3/ */
                  val bioS = json.\("bio").asOpt[String].getOrElse("")
                  val avatarUrl = Option("http://www.gravatar.com/avatar/" + DigestUtils.md5Hex(emailS))
                  val company = json.\("company").asOpt[String]
                  val blog = json.\("blog").asOpt[String]

                  // Try to lookup the speaker
                  Webuser.findByEmail(emailS).map {
                    w =>
                      val cookie = createCookie(w)
                      if (visitor) {
                        Redirect(routes.Favorites.welcomeVisitor()).withSession("uuid" -> w.uuid).withCookies(cookie)
                      } else {
                        Redirect(routes.CallForPaper.homeForSpeaker()).flashing("warning" -> Messages("cfp.reminder.proposals")).withSession("uuid" -> w.uuid).withCookies(cookie)
                      }
                  }.getOrElse {
                    // Create a new one but ask for confirmation
                    val (firstName, lastName) = if (nameS.indexOf(" ") != -1) {
                      (nameS.substring(0, nameS.indexOf(" ")), nameS.substring(nameS.indexOf(" ") + 1))
                    } else {
                      (nameS, nameS)
                    }

                    if (visitor) {
                      val newWebuser = Webuser.createVisitor(emailS, firstName, lastName)
                      Ok(views.html.Authentication.confirmImportVisitor(newWebuserForm.fill(newWebuser)))

                    } else {
                      val defaultValues = (emailS, firstName, lastName, StringUtils.abbreviate(bioS, 750), company, None, blog, avatarUrl, "No experience", None)

                      Ok(views.html.Authentication.confirmImport(importSpeakerForm.fill(defaultValues)))
                    }
                  }
              })
            case other =>
              play.Logger.error("Unable to complete call " + result.status + " " + result.statusText + " " + result.body)
              BadRequest("Unable to complete the Github User API call")
          }
      }

  }

  def saveNewSpeaker = Action {
    implicit request =>
      newWebuserForm.bindFromRequest.fold(
        invalidForm => BadRequest(views.html.Authentication.prepareSignup(invalidForm)),
        validForm => {
          Webuser.saveNewWebuserEmailNotValidated(validForm)
          TransactionalEmails.sendValidateYourEmail(validForm.email, routes.Authentication.validateYourEmailForSpeaker(Crypto.sign(validForm.email.toLowerCase.trim), new String(Base64.encodeBase64(validForm.email.toLowerCase.trim.getBytes("UTF-8")), "UTF-8")).absoluteURL(ConferenceDescriptor.isHTTPSEnabled))
          Ok(views.html.Authentication.created(validForm.email))
        }
      )
  }

  def saveNewVisitor = Action {
    implicit request =>
      newVisitorForm.bindFromRequest.fold(
        invalidForm => BadRequest(views.html.Authentication.prepareSignupVisitor(invalidForm)),
        validForm => {
          Webuser.saveNewWebuserEmailNotValidated(validForm)
          TransactionalEmails.sendValidateYourEmail(validForm.email, routes.Authentication.validateYourEmailForVisitor(Crypto.sign(validForm.email.toLowerCase.trim), new String(Base64.encodeBase64(validForm.email.toLowerCase.trim.getBytes("UTF-8")), "UTF-8")).absoluteURL(ConferenceDescriptor.isHTTPSEnabled))
          Ok(views.html.Authentication.created(validForm.email))
        }
      )
  }

  def validateYourEmailForSpeaker(t: String, a: String) = Action {
    implicit request =>
      val email = new String(Base64.decodeBase64(a), "UTF-8")
      if (Crypto.sign(email) == t) {
        val futureMaybeWebuser = Webuser.findNewUserByEmail(email)
        futureMaybeWebuser.map {
          webuser =>
            val uuid = Webuser.saveAndValidateWebuser(webuser) // it is generated
            val someLang = request.acceptLanguages.headOption.map(_.code)
            Speaker.save(Speaker.createSpeaker(uuid, email, webuser.lastName, "", someLang,None, Some("http://www.gravatar.com/avatar/" + Webuser.gravatarHash(webuser.email)), None, None, webuser.firstName, "No experience",None, None, None, None, None, None, None))
            TransactionalEmails.sendAccessCode(webuser.email, webuser.password)
            Redirect(routes.CallForPaper.editProfile()).flashing("success" -> ("Your account has been validated. Your new access code is " + webuser.password + " (case-sensitive)")).withSession("uuid" -> webuser.uuid)
        }.getOrElse {
          Redirect(routes.Application.index()).flashing("error" -> "Sorry, this email is not registered in your system.")
        }
      } else {
        Redirect(routes.Application.index()).flashing("error" -> "Sorry, we could not validate your authentication token. Are you sure that this email is registered?")
      }
  }

  def validateYourEmailForVisitor(t: String, a: String) = Action {
    implicit request =>
      val email = new String(Base64.decodeBase64(a), "UTF-8")
      if (Crypto.sign(email) == t) {
        val futureMaybeWebuser = Webuser.findNewUserByEmail(email)
        futureMaybeWebuser.map {
          webuser =>
            val newUUID = Webuser.saveAndValidateWebuser(webuser) // it is generated
            TransactionalEmails.sendAccessCode(webuser.email, webuser.password)
            val cookie = createCookie(webuser)
            Redirect(routes.Favorites.welcomeVisitor()).withSession("uuid" -> newUUID).withCookies(cookie).flashing("success" -> ("Your account has been validated. Your new access code is " + webuser.password + " (case-sensitive)")).withSession("uuid" -> webuser.uuid)
        }.getOrElse {
          Redirect(routes.Application.index()).flashing("error" -> "Sorry, your invitation has expired.")
        }
      } else {
        Redirect(routes.Application.index()).flashing("error" -> "Sorry, we could not validate your authentication token. Are you sure that this email is registered?")
      }
  }

  def validateImportedSpeaker = Action {
    implicit request =>
      importSpeakerForm.bindFromRequest.fold(
        invalidForm => BadRequest(views.html.Authentication.confirmImport(invalidForm)).flashing("error" -> "Please check your profile, invalid webuser."),
        validFormData => {
          val email = validFormData._1
          val firstName = validFormData._2
          val lastName = validFormData._3
          val bio = validFormData._4
          val company = validFormData._5
          val twitter = validFormData._6
          val blog = validFormData._7
          val avatarUrl = validFormData._8
          val qualifications = validFormData._9
          val linkedIn = validFormData._10

          val validWebuser = if (Webuser.isEmailRegistered(email)) {
            // An existing webuser might have been created with a different play.secret key
            // or from a Golden ticket/visitor profile
            val existingUUID = Webuser.getUUIDfromEmail(email).getOrElse(Webuser.generateUUID(email))
            Webuser.findByUUID(existingUUID).getOrElse(Webuser.createSpeaker(email, firstName, lastName))
          } else {
            val newWebuser = Webuser.createSpeaker(email, firstName, lastName)
            Webuser.saveAndValidateWebuser(newWebuser)
            newWebuser
          }

          val lang = request.acceptLanguages.headOption.map(_.code)
          val newSpeaker = Speaker.createSpeaker(
            validWebuser.uuid,
            email,
            validWebuser.lastName,
            StringUtils.abbreviate(bio, 750),
            lang,
            twitter,
            avatarUrl,
            company,
            blog,
            validWebuser.firstName,
            qualifications,
            None,
            None,
            None,
            None,
            linkedIn,
            None,
            None
          )
          Speaker.save(newSpeaker)
          Webuser.addToSpeaker(validWebuser.uuid)

          Ok(views.html.Authentication.validateImportedSpeaker(validWebuser.email, validWebuser.password)).withSession("uuid" -> validWebuser.uuid).withCookies(createCookie(validWebuser))
        }
      )
  }

  def validateImportedVisitor = Action {
    implicit request =>
      newWebuserForm.bindFromRequest.fold(
        invalidForm => BadRequest(views.html.Authentication.confirmImportVisitor(invalidForm)).flashing("error" -> "Please check your profile, invalid webuser."),
        webuserForm => {
          val validWebuser = Webuser.createVisitor(webuserForm.email, webuserForm.firstName, webuserForm.lastName)
          Webuser.saveAndValidateWebuser(validWebuser)
          Ok(views.html.Authentication.validateImportedVisitor(validWebuser.email, validWebuser.password)).withSession("uuid" -> validWebuser.uuid).withCookies(createCookie(validWebuser))
        }
      )
  }

  // See LinkedIn documentation https://developer.linkedin.com/documents/authentication
  def linkedinLogin(visitor: Boolean) = Action {
    implicit request =>
      Play.current.configuration.getString("linkedin.client_id").map {
        clientId: String =>
          val redirectUri = routes.Authentication.callbackLinkedin().absoluteURL(ConferenceDescriptor.isHTTPSEnabled)
          val state = new BigInteger(130, new SecureRandom()).toString(32)
          val gitUrl = "https://www.linkedin.com/uas/oauth2/authorization?client_id=" + clientId + "&scope=r_basicprofile%20r_emailaddress&state=" + Crypto.sign(state) + "&redirect_uri=" + redirectUri + "&response_type=code"
          Redirect(gitUrl).withSession("state" -> state)
      }.getOrElse {
        InternalServerError("linkedin.client_id is not set in application.conf")
      }
  }

  def callbackLinkedin = Action.async {
    implicit request =>
      oauthForm.bindFromRequest.fold(invalidForm => {
        Future.successful {
          BadRequest(views.html.Application.home(invalidForm)).flashing("error" -> "Invalid form")
        }
      }, {
        case (code, state) if state == Crypto.sign(request.session.get("state").getOrElse("")) =>
          val auth = for (clientId <- Play.current.configuration.getString("linkedin.client_id");
                          clientSecret <- Play.current.configuration.getString("linkedin.client_secret")) yield (clientId, clientSecret)
          auth.map {
            case (clientId, clientSecret) =>
              val url = "https://www.linkedin.com/uas/oauth2/accessToken"
              val redirect_uri = routes.Authentication.callbackLinkedin().absoluteURL(ConferenceDescriptor.isHTTPSEnabled)
              val wsCall = WS.url(url).withHeaders("Accept" -> "application/json", "Content-Type" -> "application/x-www-form-urlencoded")
                .post(Map("client_id" -> Seq(clientId), "client_secret" -> Seq(clientSecret), "code" -> Seq(code), "grant_type" -> Seq("authorization_code"), "redirect_uri" -> Seq(redirect_uri)))
              wsCall.map {
                result =>
                  result.status match {
                    case 200 =>
                      val b = result.body
                      val json = Json.parse(result.body)
                      val token = json.\("access_token").as[String]
                      Redirect(routes.Authentication.createFromLinkedin()).withSession("linkedin_token" -> token)
                    case _ =>
                      Redirect(routes.Application.index()).flashing("error" -> ("error with LinkedIn OAuth2.0 : got HTTP response " + result.status + " " + result.body))
                  }
              }
          }.getOrElse {
            Future.successful {
              InternalServerError("linkedin.client_id and linkedin.client_secret are not configured in application.conf")
            }
          }
        case other => Future.successful {
          BadRequest(views.html.Application.home(loginForm)).flashing("error" -> "Invalid state code")
        }
      })
  }

  def createFromLinkedin = Action.async {
    implicit request =>
      request.session.get("linkedin_token").map {
        access_token =>
          //for Linkedin profile
          val url = "https://api.linkedin.com/v1/people/~:(id,first-name,last-name,email-address,picture-url,summary,public-profile-url)?format=json&oauth2_access_token=" + access_token

          val futureResult = WS.url(url).withHeaders(
            "User-agent" -> ("CFP " + ConferenceDescriptor.current().conferenceUrls.cfpHostname),
            "Accept" -> "application/json"
          ).get()

          futureResult.map {
            result =>
              result.status match {
                case 200 =>
                  val json = Json.parse(result.body)
                  val email = json.\("emailAddress").as[String]
                  val firstName = json.\("firstName").asOpt[String]
                  val lastName = json.\("lastName").asOpt[String]
                  val photo = json.\("pictureUrl").asOpt[String]
                  val summary = json.\("summary").asOpt[String]
                  val linkedin = json.\("public-profile-url").asOpt[String]

                  // Try to lookup the speaker
                  Webuser.findByEmail(email).map {
                    w =>
                      val cookie = createCookie(w)
                      Redirect(routes.CallForPaper.homeForSpeaker()).flashing("warning" -> Messages("cfp.reminder.proposals")).withSession("uuid" -> w.uuid).withCookies(cookie)
                  }.getOrElse {
                    val defaultValues = (email, firstName.getOrElse("?"), lastName.getOrElse("?"), summary.getOrElse("?"), None, None, None, photo, "No experience", linkedin)
                    Ok(views.html.Authentication.confirmImport(importSpeakerForm.fill(defaultValues)))
                  }

                case other =>
                  play.Logger.error("Unable to complete call " + result.status + " " + result.statusText + " " + result.body)
                  BadRequest("Unable to complete the LinkedIn User API call")
              }
          }
      }.getOrElse {
        Future.successful {
          Redirect(routes.Application.index()).flashing("error" -> "Your LinkedIn Access token has expired, please reauthenticate")
        }
      }
  }

  // See Google documentation https://developers.google.com/accounts/docs/OAuth2Login
  def googleLogin(visitor: Boolean) = Action {
    implicit request =>
      Play.current.configuration.getString("google.client_id").map {
        clientId: String =>
          val redirectUri = routes.Authentication.callbackGoogle().absoluteURL(ConferenceDescriptor.isHTTPSEnabled)
          val state = new BigInteger(130, new SecureRandom()).toString(32)
          val gitUrl = "https://accounts.google.com/o/oauth2/auth?client_id=" + clientId + "&scope=openid%20email%20profile&state=" + Crypto.sign(state) + "&redirect_uri=" + redirectUri + "&response_type=code"
          Redirect(gitUrl).withSession("state" -> state)
      }.getOrElse {
        InternalServerError("google.client_id is not set in application.conf")
      }
  }

  implicit val googleFormat = Json.format[GoogleToken]

  def callbackGoogle = Action.async {
    implicit request =>
      oauthForm.bindFromRequest.fold(invalidForm => {
        Future.successful {
          BadRequest(views.html.Application.home(invalidForm)).flashing("error" -> "Invalid form")
        }
      }, {
        case (code, state) if state == Crypto.sign(request.session.get("state").getOrElse("")) =>
          val auth = for (clientId <- Play.current.configuration.getString("google.client_id");
                          clientSecret <- Play.current.configuration.getString("google.client_secret")) yield (clientId, clientSecret)
          auth.map {
            case (clientId, clientSecret) =>
              val url = "https://accounts.google.com/o/oauth2/token"
              val redirect_uri = routes.Authentication.callbackGoogle().absoluteURL(true)
              val wsCall = WS.url(url).withHeaders("Accept" -> "application/json", "User-Agent" -> ("CFP " + ConferenceDescriptor.current().conferenceUrls.cfpHostname)).post(Map("client_id" -> Seq(clientId), "client_secret" -> Seq(clientSecret), "code" -> Seq(code), "grant_type" -> Seq("authorization_code"), "redirect_uri" -> Seq(redirect_uri)))
              wsCall.map {
                result =>
                  result.status match {
                    case 200 =>
                      val googleToken = Json.parse(result.body).as[GoogleToken]
                      Redirect(routes.Authentication.createFromGoogle()).withSession("google_token" -> googleToken.access_token)
                    case _ =>
                      Redirect(routes.Application.index()).flashing("error" -> ("error with Google OAuth2.0 : got HTTP response " + result.status + " " + result.body))
                  }
              }
          }.getOrElse {
            Future.successful {
              InternalServerError("google.client_id is not configured in application.conf")
            }
          }
        case other => Future.successful(BadRequest(views.html.Application.home(loginForm)).flashing("error" -> "Invalid state code"))
      })
  }

  def createFromGoogle = Action.async {
    implicit request =>
      request.session.get("google_token").map {
        access_token =>
          //for Google account
          val url = "https://www.googleapis.com/oauth2/v3/userinfo?access_token=" + access_token

          // For Google+ profile
          //val url = "https://www.googleapis.com/plus/v1/people/me?access_token=" + access_token+"&"
          val futureResult = WS.url(url).withHeaders(
            "User-agent" -> ("CFP " + ConferenceDescriptor.current().conferenceUrls.cfpHostname),
            "Accept" -> "application/json"
          ).get()

          futureResult.map {
            result =>
              result.status match {
                case 200 =>
                  //Ok(result.body).as("application/json")
                  val json = Json.parse(result.body)

                  val email = json.\("email").as[String]
                  val firstName = json.\("given_name").asOpt[String]
                  val lastName = json.\("family_name").asOpt[String]
                  val blog = json.\("profile").asOpt[String]
                  val photo = json.\("picture").asOpt[String]

                  // Try to lookup the speaker
                  Webuser.findByEmail(email).map {
                    w =>
                      val cookie = createCookie(w)
                      Redirect(routes.CallForPaper.homeForSpeaker()).flashing("warning" -> Messages("cfp.reminder.proposals")).withSession("uuid" -> w.uuid).withCookies(cookie)
                  }.getOrElse {
                    val defaultValues = (email, firstName.getOrElse("?"), lastName.getOrElse("?"), "", None, None, blog, photo, "No experience", None)
                    Ok(views.html.Authentication.confirmImport(importSpeakerForm.fill(defaultValues)))
                  }
                case other =>
                  play.Logger.error("Unable to complete call " + result.status + " " + result.statusText + " " + result.body)
                  BadRequest("Unable to complete the Google User API call due to "+result.status)
              }
          }
      }.getOrElse {
        Future.successful {
          Redirect(routes.Application.index()).flashing("error" -> "Your Google Access token has expired, please reauthenticate")
        }
      }
  }

  def prepareSignup(visitor: Boolean) = Action {
    implicit request =>
      if (visitor) {
        if (ConferenceDescriptor.isMyDevoxxActive) {
          play.Logger.info("Redirecting to MyDevoxx")
          Redirect(ConferenceDescriptor.myDevoxxURL(),
            Map(
              "redirect_uri" -> Seq(routes.Authentication.jwtCallback(Crypto.sign("secure_callback_" + DateTime.now(DateTimeZone.forID("Europe/Brussels")).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(0))).absoluteURL(ConferenceDescriptor.isHTTPSEnabled))
            ),
            SEE_OTHER
          )

        } else {
          Ok(views.html.Authentication.prepareSignupVisitor(newVisitorForm))
        }
      } else {
        Ok(views.html.Authentication.prepareSignup(newWebuserForm))
      }
  }

  def jwtCallback(token: String) = Action.async {
    implicit request =>
      if (token == Crypto.sign("secure_callback_" + DateTime.now(DateTimeZone.forID("Europe/Brussels")).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(0))) {
        request.getQueryString("jwtToken") match {
          case None =>
            Future.successful(Unauthorized("No JWT Token"))
          case Some(jwtToken) =>

              Jwt.decode(jwtToken, ConferenceDescriptor.jwtSharedSecret(), Seq(JwtAlgorithm.HS256)) match {
              case Success(decodedString) =>
                val json = Json.parse(decodedString)
                val firstName = (json \ "firstName").as[String]
                val lastName = (json \ "lastName").as[String]
                val uuid = (json \ "userId").as[String]
                val _email = (json \ "email").as[String]

                // Check if the user is not already a speaker or a valid webuser
                val maybeWebuser = Webuser.findByEmail(_email)

                val (webuser:Webuser, newUUID:String) = maybeWebuser match {
                  case w if w.isDefined =>
                    Webuser.addToDevoxxians(maybeWebuser.get.uuid)
                    (maybeWebuser.get, maybeWebuser.get.uuid)
                  case other=>
                    val webuser = Webuser.createDevoxxian(_email, Some("MY_DEVOXX_FR"), Some("00000"))
                    val newUUID = Webuser.saveAndValidateWebuser(webuser)
                    Webuser.addToDevoxxians(newUUID)
                    (webuser,newUUID)
                }

                val cookie = createCookie(webuser)
                Future.successful(
                  Redirect(
                    request.headers.toSimpleMap.getOrElse(REFERER, routes.Publisher.homePublisher().absoluteURL(ConferenceDescriptor.isHTTPSEnabled))
                  ).flashing("success" -> Messages("mydevoxx.authenticated")).withSession("uuid" -> newUUID).withCookies(cookie)
                )
              case Failure(_) => Future.successful(Unauthorized("Not Authorized - token is invalid"))
            }
        }
      } else {
        Future.successful(Unauthorized("Invalid secure token"))
      }
  }

  private def createCookie(webuser: Webuser) = {
    Cookie("cfp_rm"
      , value = Crypto.encryptAES(webuser.uuid)
      , maxAge = Some(588000)
      , secure = ConferenceDescriptor.isHTTPSEnabled
      , httpOnly = true)
  }
}

case class GoogleToken(access_token: String, token_type: String, expires_in: Long, id_token: String)