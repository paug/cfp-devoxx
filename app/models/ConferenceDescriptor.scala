package models

import java.util.Locale

import org.apache.commons.lang3.RandomStringUtils
import org.joda.time.{DateTime, DateTimeZone, Period}
import play.api.Play

/**
  * ConferenceDescriptor.
  * This might be the first file to look at, and to customize.
  * Idea behind this file is to try to collect all configurable parameters for a conference.
  *
  * For labels, please do customize messages and messages.fr
  *
  * Note from Nicolas : the first version of the CFP was much more "static" but hardly configurable.
  *
  * @author Frederic Camblor, BDX.IO 2014
  */

case class ConferenceUrls(faq: String, registration: String, confWebsite: String, cfpHostname: String) {
  def cfpURL(): String = {
    if (Play.current.configuration.getBoolean("cfp.activateHTTPS").getOrElse(false)) {
      s"https://$cfpHostname"
    } else {
      s"http://$cfpHostname"
    }
  }

}

case class ConferenceTiming(
                             datesI18nKey: String,
                             speakersPassDuration: Integer,
                             preferredDayEnabled: Boolean,
                             firstDayFr: String,
                             firstDayEn: String,
                             datesFr: String,
                             datesEn: String,
                             cfpOpenedOn: DateTime,
                             cfpClosedOn: DateTime,
                             scheduleAnnouncedOn: DateTime,
                             days: Iterator[DateTime]
                           )

case class ConferenceSponsor(showSponsorProposalCheckbox: Boolean, sponsorProposalType: ProposalType = ProposalType.UNKNOWN)

case class TrackDesc(id: String, imgSrc: String, i18nTitleProp: String, i18nDescProp: String)

case class ProposalConfiguration(id: String, slotsCount: Int,
                                 givesSpeakerFreeEntrance: Boolean,
                                 freeEntranceDisplayed: Boolean,
                                 htmlClass: String,
                                 hiddenInCombo: Boolean = false,
                                 chosablePreferredDay: Boolean = false,
                                 impliedSelectedTrack: Option[Track] = None)

object ProposalConfiguration {

  val UNKNOWN = ProposalConfiguration(id = "unknown", slotsCount = 0, givesSpeakerFreeEntrance = false, freeEntranceDisplayed = false,
    htmlClass = "", hiddenInCombo = true, chosablePreferredDay = false)

  def parse(propConf: String): ProposalConfiguration = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.find(p => p.id == propConf).getOrElse(ProposalConfiguration.UNKNOWN)
  }

  def totalSlotsCount = ConferenceDescriptor.ConferenceProposalConfigurations.ALL.map(_.slotsCount).sum

  def isDisplayedFreeEntranceProposals(pt: ProposalType): Boolean = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.freeEntranceDisplayed).headOption.getOrElse(false)
  }

  def getProposalsImplyingATrackSelection = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.impliedSelectedTrack.nonEmpty)
  }

  def getHTMLClassFor(pt: ProposalType): String = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.htmlClass).headOption.getOrElse("unknown")
  }

  def isChosablePreferredDaysProposals(pt: ProposalType): Boolean = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.chosablePreferredDay).headOption.getOrElse(false)
  }

  def doesProposalTypeGiveSpeakerFreeEntrance(pt: ProposalType): Boolean = {
    ConferenceDescriptor.ConferenceProposalConfigurations.ALL.filter(p => p.id == pt.id).map(_.givesSpeakerFreeEntrance).headOption.getOrElse(false)
  }
}

case class ConferenceDescriptor(eventCode: String,
                                confUrlCode: String,
                                frLangEnabled: Boolean,
                                fromEmail: String,
                                committeeEmail: String,
                                bccEmail: Option[String],
                                bugReportRecipient: String,
                                conferenceUrls: ConferenceUrls,
                                timing: ConferenceTiming,
                                hosterName: String,
                                hosterWebsite: String,
                                hashTag: String,
                                conferenceSponsor: ConferenceSponsor,
                                locale: List[Locale],
                                localisation: String,
                                maxProposalSummaryCharacters: Int = 1200
                               )

object ConferenceDescriptor {

  /**
    * TODO configure here the kind of talks you will propose
    */
  object ConferenceProposalTypes {
    val KEY = ProposalType(id = "a_key", label = "key.label")

    val CONF = ProposalType(id = "b_conf", label = "conf.label")

    val QUICK = ProposalType(id = "c_quick", label = "quick.label")

    val WORKSHOP = ProposalType(id = "d_workshop", label = "workshop.label")

    val DEMO = ProposalType(id = "e_demo", label = "demo.label")

    val ALL = List(KEY, CONF, QUICK, WORKSHOP, DEMO)

    def valueOf(id: String): ProposalType = id match {
      case "a_key" => KEY
      case "b_conf" => CONF
      case "c_quick" => QUICK
      case "d_workshop" => WORKSHOP
      case "e_demo" => DEMO
    }

  }

  // TODO Configure here the slot, with the number of slots available, if it gives a free ticket to the speaker, some CSS icons
  object ConferenceProposalConfigurations {
    val KEY = ProposalConfiguration(id = "a_key", slotsCount = 3, givesSpeakerFreeEntrance = true, freeEntranceDisplayed = false, htmlClass = "icon-microphone",
      chosablePreferredDay = true)
    val CONF = ProposalConfiguration(id = "b_conf", slotsCount = ConferenceSlots.all.count(_.name.equals(ConferenceProposalTypes.CONF.id)), givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-microphone",
      chosablePreferredDay = true)
    val QUICK = ProposalConfiguration(id = "c_quick", slotsCount = ConferenceSlots.all.count(_.name.equals(ConferenceProposalTypes.QUICK.id)), givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-fast-forward",
      chosablePreferredDay = true)
    val WORKSHOP = ProposalConfiguration(id = "d_workshop", slotsCount = ConferenceSlots.all.count(_.name.equals(ConferenceProposalTypes.WORKSHOP.id)), givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-laptop",
      chosablePreferredDay = true)
    val DEMO = ProposalConfiguration(id = "e_demo", slotsCount = ConferenceSlots.all.count(_.name.equals(ConferenceProposalTypes.DEMO.id)), givesSpeakerFreeEntrance = true, freeEntranceDisplayed = true, htmlClass = "icon-group",
      chosablePreferredDay = true)

    val ALL = List(KEY, CONF, QUICK, WORKSHOP, DEMO)

    def doesItGivesSpeakerFreeEntrance(proposalType: ProposalType): Boolean = {
      ALL.filter(_.id == proposalType.id).exists(_.givesSpeakerFreeEntrance)
    }
  }

  // TODO Configure here your Conference's tracks.
  object ConferenceTracks {
    val ANDROID_DEV = Track("anddev", "anddev.label")
    val ANDROID_EVERYWHERE = Track("andeverywhere", "andeverywhere.label")
    val UXUI = Track("uxui", "uxui.label")
    val UNKNOWN = Track("unknown", "unknown track")
    val ALL = List(ANDROID_DEV, ANDROID_EVERYWHERE, UXUI, UNKNOWN)
  }

  // TODO configure the description for each Track
  // TODO Choose and put assets
  object ConferenceTracksDescription {
    val ANDROID_DEV = TrackDesc(ConferenceTracks.ANDROID_DEV.id, "/assets/androidmakersfr2018/images/icon_dev.png", "track.anddev.title", "track.anddev.desc")
    val ANDROID_EVERYWHERE = TrackDesc(ConferenceTracks.ANDROID_EVERYWHERE.id, "/assets/androidmakersfr2018/images/icon_iot.png", "track.andeverywhere.title", "track.andeverywhere.desc")
    val UXUI = TrackDesc(ConferenceTracks.UXUI.id, "/assets/androidmakersfr2018/images/icon_uxui.png", "track.uxui.title", "track.uxui.desc")

    val ALL = List(ANDROID_DEV, ANDROID_EVERYWHERE, UXUI)

    def findTrackDescFor(t: Track): TrackDesc = {
      ALL.find(_.id == t.id).getOrElse(ANDROID_DEV)
    }
  }

  // TODO If you want to use the Devoxx Scheduler, you can describe here the list of rooms, with capacity for seats
  object ConferenceRooms {

    // Tip : I use the ID to sort-by on the view per day... So if the exhibition floor id is "aaa" it will be
    // the first column on the HTML Table

    // Do not change the ID's once the program is published
    val AMPHI_A = Room("a_track", "Amphi A", 300, "theatre", "rien")
    val AMPHI_B = Room("b_track", "Amphi B", 200, "theatre", "rien")
    val WORKSHOP = Room("c_workshop", "Workshop", 60, "classroom", "rien")
    val HALL_EXPO = Room("d_hall", "Exhibition floor", 600, "special", "")

    val allRooms = List(HALL_EXPO, AMPHI_A, AMPHI_B, WORKSHOP)

    val allRoomsAsIdsAndLabels:Seq[(String,String)] = allRooms.map(a=>(a.id,a.name)).sorted

    val keynoteRoom = List(AMPHI_A)

    val allRoomsConf = List(AMPHI_A, AMPHI_B)

    val allRoomsQuickiesMonday = List(AMPHI_B)

    val allRoomsWorkshop = List(WORKSHOP)
  }

  // TODO if you want to use the Scheduler, you can configure the breaks
  object ConferenceSlotBreaks {
    val registration = SlotBreak("reg", "Registration", "Accueil", ConferenceRooms.HALL_EXPO)
    val petitDej = SlotBreak("dej", "Breakfast", "Accueil et petit-déjeuner", ConferenceRooms.HALL_EXPO)
    val coffee = SlotBreak("coffee", "Coffee Break", "Pause café", ConferenceRooms.HALL_EXPO)
    val lunch = SlotBreak("lunch", "Lunch", "Pause déjeuner", ConferenceRooms.HALL_EXPO)
    val shortBreak = SlotBreak("chgt", "Break", "Pause courte", ConferenceRooms.HALL_EXPO)
    val exhibition = SlotBreak("exhib", "Exhibition", "Exhibition", ConferenceRooms.HALL_EXPO)
    val meetAndGreet = SlotBreak("meet", "Meet & Greet", "Exhibition", ConferenceRooms.HALL_EXPO)
  }

  // TODO The idea here is to describe in term of Agenda, for each rooms, the slots. This is required only for the Scheduler
  object ConferenceSlots {

    val firstDay = "2019-04-23"
    val secondDay = "2019-04-24"

    // WORKSHOPS
    val workshopSlotsMonday: List[Slot] = {

      val workshopSlotsMondaySlot1 = ConferenceRooms.allRoomsWorkshop.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "monday",
            new DateTime(s"${firstDay}T10:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T13:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r1)
      }
      val workshopSlotsMondaySlot2 = ConferenceRooms.allRoomsWorkshop.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "monday",
            new DateTime(s"${firstDay}T14:25:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T17:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r2)
      }
      workshopSlotsMondaySlot1 ++ workshopSlotsMondaySlot2
    }

    val workshopSlotsTuesday: List[Slot] = {
      val workshopSlotsTuesdaySlot1 = ConferenceRooms.allRoomsWorkshop.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T09:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T10:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r1)
      }
      val workshopSlotsTuesdaySlot2 = ConferenceRooms.allRoomsWorkshop.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T10:55:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T12:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r2)
      }
      val workshopSlotsTuesdaySlot3 = ConferenceRooms.allRoomsWorkshop.map {
        r3 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T14:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T16:25:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r3)
      }
      workshopSlotsTuesdaySlot1 ++ workshopSlotsTuesdaySlot2 ++ workshopSlotsTuesdaySlot3
    }

    // QUICKIES
    val quickiesSlotsMonday: List[Slot] = {

      val quickiesMondaySlot1 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T10:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T11:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r1)
      }
      val quickiesMondaySlot2 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T11:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T11:25:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r2)
      }
      val quickiesMondaySlot3 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r3 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T11:25:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T11:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r3)
      }
      val quickiesMondaySlot4 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r4 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T11:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T12:15:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r4)
      }
      val quickiesMondaySlot5 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r5 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T12:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T12:55:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r5)
      }
      val quickiesMondaySlot6 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r6 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T12:55:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T13:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r6)
      }
      val quickiesMondaySlot7 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r7 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T13:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T13:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r7)
      }
      val quickiesMondaySlot8 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r8 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T13:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T14:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r8)
      }
      val quickiesMondaySlot9 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r9 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T14:25:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T14:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r9)
      }
      val quickiesMondaySlot10 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r10 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T14:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T15:15:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r10)
      }
      val quickiesMondaySlot11 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r11 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T15:15:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T15:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r11)
      }
      val quickiesMondaySlot12 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r12 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T15:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T16:05:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r12)
      }
      val quickiesMondaySlot13 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r13 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T16:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T16:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r13)
      }
      val quickiesMondaySlot14 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r14 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T16:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T17:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r14)
      }
      val quickiesMondaySlot15 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r15 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T17:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T17:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r15)
      }
      val quickiesMondaySlot16 = ConferenceRooms.allRoomsQuickiesMonday.map {
        r16 =>
          SlotBuilder(ConferenceProposalTypes.QUICK.id, "monday",
            new DateTime(s"${firstDay}T17:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T18:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r16)
      }
      quickiesMondaySlot1 ++ quickiesMondaySlot2 ++ quickiesMondaySlot3 ++ quickiesMondaySlot4 ++ quickiesMondaySlot5 ++ quickiesMondaySlot6 ++ quickiesMondaySlot7 ++ quickiesMondaySlot8 ++ quickiesMondaySlot9 ++ quickiesMondaySlot10 ++ quickiesMondaySlot11 ++ quickiesMondaySlot12 ++ quickiesMondaySlot13 ++ quickiesMondaySlot14 ++ quickiesMondaySlot15 ++ quickiesMondaySlot16
    }

    // CONFERENCE KEYNOTES
    val keynoteSlotsMonday: List[Slot] = {

      val keynoteWelcome = ConferenceRooms.keynoteRoom.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "monday",
            new DateTime(s"${firstDay}T09:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T09:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r1)
      }
      val keynoteMonday = ConferenceRooms.keynoteRoom.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "monday",
            new DateTime(s"${firstDay}T09:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T10:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r2)
      }

      keynoteWelcome ++ keynoteMonday
    }

    val keynoteSlotsTuesday: List[Slot] = {
      val keynoteClosing = ConferenceRooms.keynoteRoom.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.KEY.id, "tuesday",
            new DateTime(s"${secondDay}T09:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T10:05:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r1)
      }

      keynoteClosing
    }


    // CONFERENCE SLOTS
    val conferenceSlotsMonday: List[Slot] = {

      val conferenceMondaySlot1 = ConferenceRooms.allRoomsConf.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "monday",
            new DateTime(s"${firstDay}T10:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T11:25:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r1)
      }
      val conferenceMondaySlot2 = ConferenceRooms.allRoomsConf.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "monday",
            new DateTime(s"${firstDay}T11:25:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T12:15:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r2)
      }
      val conferenceMondaySlot3 = ConferenceRooms.allRoomsConf.map {
        r3 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "monday",
            new DateTime(s"${firstDay}T12:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T13:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r3)
      }
      val conferenceMondaySlot4 = ConferenceRooms.allRoomsConf.map {
        r4 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "monday",
            new DateTime(s"${firstDay}T13:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T14:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r4)
      }
      val conferenceMondaySlot5 = ConferenceRooms.allRoomsConf.map {
        r5 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "monday",
            new DateTime(s"${firstDay}T14:25:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T15:15:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r5)
      }
      val conferenceMondaySlot6 = ConferenceRooms.allRoomsConf.map {
        r6 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "monday",
            new DateTime(s"${firstDay}T15:15:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T16:05:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r6)
      }
      val conferenceMondaySlot7 = ConferenceRooms.allRoomsConf.map {
        r7 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "monday",
            new DateTime(s"${firstDay}T16:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T17:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r7)
      }
      val conferenceMondaySlot8 = ConferenceRooms.allRoomsConf.map {
        r8 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "monday",
            new DateTime(s"${firstDay}T17:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T18:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r8)
      }
      val conferenceMondaySlot9 = ConferenceRooms.allRoomsConf.map {
        r9 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "monday",
            new DateTime(s"${firstDay}T18:15:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${firstDay}T19:05:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r9)
      }
      conferenceMondaySlot1 ++ conferenceMondaySlot2 ++ conferenceMondaySlot3 ++ conferenceMondaySlot4 ++ conferenceMondaySlot5 ++ conferenceMondaySlot6 ++ conferenceMondaySlot7 ++ conferenceMondaySlot8 ++ conferenceMondaySlot9
    }

    val conferenceSlotsTuesday: List[Slot] = {

      val conferenceTuesdaySlot1 = ConferenceRooms.allRoomsConf.map {
        r1 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T09:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T09:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r1)
      }
      val conferenceTuesdaySlot2 = ConferenceRooms.allRoomsConf.map {
        r2 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T09:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T09:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r2)
      }
      val conferenceTuesdaySlot3 = ConferenceRooms.allRoomsConf.map {
        r3 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T09:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T10:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r3)
      }
      val conferenceTuesdaySlot4 = ConferenceRooms.allRoomsConf.map {
        r4 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T09:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T10:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r4)
      }
      val conferenceTuesdaySlot5 = ConferenceRooms.allRoomsConf.map {
        r5 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T10:55:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T11:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r5)
      }
      val conferenceTuesdaySlot6 = ConferenceRooms.allRoomsConf.map {
        r6 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T10:55:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T11:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r6)
      }
      val conferenceTuesdaySlot7 = ConferenceRooms.allRoomsConf.map {
        r7 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T11:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T12:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r7)
      }
      val conferenceTuesdaySlot8 = ConferenceRooms.allRoomsConf.map {
        r8 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T11:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T12:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r8)
      }
      val conferenceTuesdaySlot9 = ConferenceRooms.allRoomsConf.map {
        r9 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T12:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T13:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r9)
      }
      val conferenceTuesdaySlot10 = ConferenceRooms.allRoomsConf.map {
        r10 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T12:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T13:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r10)
      }
      val conferenceTuesdaySlot11 = ConferenceRooms.allRoomsConf.map {
        r11 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T13:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T14:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r11)
      }
      val conferenceTuesdaySlot12 = ConferenceRooms.allRoomsConf.map {
        r12 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T13:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T14:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r12)
      }
      val conferenceTuesdaySlot13 = ConferenceRooms.allRoomsConf.map {
        r13 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T14:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T15:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r13)
      }
      val conferenceTuesdaySlot14 = ConferenceRooms.allRoomsConf.map {
        r14 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T14:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T15:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r14)
      }
      val conferenceTuesdaySlot15 = ConferenceRooms.allRoomsConf.map {
        r15 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T15:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T16:25:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r15)
      }
      val conferenceTuesdaySlot16 = ConferenceRooms.allRoomsConf.map {
        r16 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T15:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T16:25:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r16)
      }
      val conferenceTuesdaySlot17 = ConferenceRooms.allRoomsConf.map {
        r17 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T16:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T17:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r17)
      }
      val conferenceTuesdaySlot18 = ConferenceRooms.allRoomsConf.map {
        r18 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T16:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T17:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r18)
      }
      val conferenceTuesdaySlot19 = ConferenceRooms.allRoomsConf.map {
        r19 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T17:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T18:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r19)
      }
      val conferenceTuesdaySlot20 = ConferenceRooms.allRoomsConf.map {
        r20 =>
          SlotBuilder(ConferenceProposalTypes.CONF.id, "tuesday",
            new DateTime(s"${secondDay}T17:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
            new DateTime(s"${secondDay}T18:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")), r20)
      }
      conferenceTuesdaySlot1 ++ conferenceTuesdaySlot2 ++ conferenceTuesdaySlot3 ++ conferenceTuesdaySlot4 ++ conferenceTuesdaySlot5 ++ conferenceTuesdaySlot6 ++ conferenceTuesdaySlot7 ++ conferenceTuesdaySlot8 ++ conferenceTuesdaySlot9 ++ conferenceTuesdaySlot10 ++ conferenceTuesdaySlot11 ++ conferenceTuesdaySlot12 ++ conferenceTuesdaySlot13 ++ conferenceTuesdaySlot14 ++ conferenceTuesdaySlot15 ++ conferenceTuesdaySlot16 ++ conferenceTuesdaySlot17 ++ conferenceTuesdaySlot18 ++ conferenceTuesdaySlot19 ++ conferenceTuesdaySlot20
    }

    // Registration, coffee break, lunch etc
    val mondayBreaks = List(
      SlotBuilder(ConferenceSlotBreaks.registration, "monday",
        new DateTime(s"${firstDay}T08:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
        new DateTime(s"${firstDay}T09:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")))
      , SlotBuilder(ConferenceSlotBreaks.shortBreak, "monday",
        new DateTime(s"${firstDay}T10:20:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
        new DateTime(s"${firstDay}T10:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")))
      , SlotBuilder(ConferenceSlotBreaks.shortBreak, "monday",
        new DateTime(s"${firstDay}T12:15:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
        new DateTime(s"${firstDay}T12:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")))
      , SlotBuilder(ConferenceSlotBreaks.shortBreak, "monday",
        new DateTime(s"${firstDay}T13:55:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
        new DateTime(s"${firstDay}T14:10:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")))
      , SlotBuilder(ConferenceSlotBreaks.shortBreak, "monday",
        new DateTime(s"${firstDay}T15:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
        new DateTime(s"${firstDay}T15:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")))
    )

    val tuesdayBreaks = List(
      SlotBuilder(ConferenceSlotBreaks.registration, "tuesday",
        new DateTime(s"${secondDay}T08:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
        new DateTime(s"${secondDay}T09:00:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")))
      , SlotBuilder(ConferenceSlotBreaks.shortBreak, "thursday",
        new DateTime(s"${secondDay}T10:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
        new DateTime(s"${secondDay}T10:55:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")))
      , SlotBuilder(ConferenceSlotBreaks.shortBreak, "thursday",
        new DateTime(s"${secondDay}T12:35:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
        new DateTime(s"${secondDay}T12:50:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")))
      , SlotBuilder(ConferenceSlotBreaks.shortBreak, "thursday",
        new DateTime(s"${secondDay}T14:30:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
        new DateTime(s"${secondDay}T14:45:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")))
      , SlotBuilder(ConferenceSlotBreaks.shortBreak, "thursday",
        new DateTime(s"${secondDay}T16:25:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")),
        new DateTime(s"${secondDay}T16:40:00.000+02:00").toDateTime(DateTimeZone.forID("Europe/Paris")))
    )

    val mondaySchedule: List[Slot] = {
      mondayBreaks ++ keynoteSlotsMonday ++ conferenceSlotsMonday ++ quickiesSlotsMonday ++ workshopSlotsMonday
    }

    val tuesdaySchedule: List[Slot] = {
      tuesdayBreaks ++ keynoteSlotsTuesday ++ conferenceSlotsTuesday ++ workshopSlotsTuesday
    }

    val wednesdaySchedule: List[Slot] = List.empty[Slot]

    val thursdaySchedule: List[Slot] = List.empty[Slot]

    val fridaySchedule: List[Slot] = List.empty[Slot]

    def all: List[Slot] = {
      mondaySchedule ++ tuesdaySchedule ++ wednesdaySchedule ++ thursdaySchedule ++ fridaySchedule
    }
  }

  def dateRange(from: DateTime, to: DateTime, step: Period): Iterator[DateTime] = Iterator.iterate(from)(_.plus(step)).takeWhile(!_.isAfter(to))

  val fromDay = new DateTime().withYear(2019).withMonthOfYear(4).withDayOfMonth(23)
  val toDay = new DateTime().withYear(2019).withMonthOfYear(4).withDayOfMonth(24)

  // TODO You might want to start here and configure first, your various Conference Elements
  def current() = ConferenceDescriptor(
    eventCode = "AndroidMakersFR2019",
    // You will need to update conf/routes files with this code if modified
    confUrlCode = "androidmakersfr2019",
    frLangEnabled = true,
    fromEmail = Play.current.configuration.getString("mail.from").getOrElse("contact@androidmakers.fr"),
    committeeEmail = Play.current.configuration.getString("mail.committee.email").getOrElse("cfp@androidmakers.fr"),
    bccEmail = Play.current.configuration.getString("mail.bcc"),
    bugReportRecipient = Play.current.configuration.getString("mail.bugreport.recipient").getOrElse("contact@androidmakers.fr"),
    conferenceUrls = ConferenceUrls(
      faq = "http://androidmakers.fr/faq/", // TODO get a faq section or point to an article
      registration = "https://www.eventbrite.com/e/android-makers-1st-edition-tickets-29579435889", //TODO update URL
      confWebsite = "http://androidmakers.fr/",
      cfpHostname = {
        val h=Play.current.configuration.getString("cfp.hostname").getOrElse("cfp.androidmakers.fr")
        if(h.endsWith("/")){
          h.substring(0,h.length - 1)
        }else{
          h
        }
      }
    ),
    timing = ConferenceTiming(
      datesI18nKey = "23 au 24 avril 2019",
      speakersPassDuration = 2,
      preferredDayEnabled = true,
      firstDayFr = "23 avril",
      firstDayEn = "april 23th",
      datesFr = "du 23 au 24 avril 2019",
      datesEn = "from 23rd to 24th of April, 2019",
      cfpOpenedOn = DateTime.parse("2018-12-01T00:00:00+01:00"),
      cfpClosedOn = DateTime.parse("2019-01-31T23:59:59+01:00"),
      scheduleAnnouncedOn = DateTime.parse("2019-02-14T00:00:00+01:00"),
      days = dateRange(fromDay, toDay, new Period().withDays(1))
    ),
    hosterName = "Clever-cloud", hosterWebsite = "http://www.clever-cloud.com/#AndroidMakersFR",
    hashTag = "#AndroidMakersFR",
    conferenceSponsor = ConferenceSponsor(showSponsorProposalCheckbox = true, sponsorProposalType = ConferenceProposalTypes.CONF)
    , List(Locale.FRENCH)
    , "Le Beffroi, Montrouge"
    , 1200 // French developers tends to be a bit verbose... we need extra space :-)
  )

  // It has to be a def, not a val, else it is not re-evaluated
  def isCFPOpen: Boolean = {
    Play.current.configuration.getBoolean("cfp.isOpen").getOrElse(false)
  }

  def isGoldenTicketActive: Boolean = Play.current.configuration.getBoolean("goldenTicket.active").getOrElse(false)

  def isTagSystemActive: Boolean = Play.current.configuration.getBoolean("cfp.tags.active").getOrElse(false)

  def isFavoritesSystemActive: Boolean = Play.current.configuration.getBoolean("cfp.activateFavorites").getOrElse(false)

  def isHTTPSEnabled = Play.current.configuration.getBoolean("cfp.activateHTTPS").getOrElse(false)

  // Reset all votes when a Proposal with state=SUBMITTED (or DRAFT) is updated
  // This is to reflect the fact that some speakers are eavluated, then they update the talk, and we should revote for it
  def isResetVotesForSubmitted = Play.current.configuration.getBoolean("cfp.resetVotesForSubmitted").getOrElse(false)

  // Set this to true temporarily
  // I will implement a new feature where each CFP member can decide to receive one digest email per day or a big email
  def notifyProposalSubmitted = Play.current.configuration.getBoolean("cfp.notifyProposalSubmitted").getOrElse(false)

  // For practical reason we want to hide the room and the time slot until the full agenda is published
  def isShowRoomAndTimeslot:Boolean = Play.current.configuration.getBoolean("cfp.showRoomAndTimeslot").getOrElse(false)

  def isShowRoom:Boolean = Play.current.configuration.getBoolean("cfp.showRoom").getOrElse(false)

  // My Devoxx is an OAuth provider on which a user can register
  def isMyDevoxxActive:Boolean = Play.current.configuration.getBoolean("mydevoxx.active").getOrElse(false)

  def myDevoxxURL():String = Play.current.configuration.getString("mydevoxx.url").getOrElse("https://my.devoxx.fr")

  // This is a JWT String shared secret that needs to be configured as a global environment variable
  def jwtSharedSecret() : String = Play.current.configuration.getString("mydevoxx.jwtSharedSecret").getOrElse("change me please")

  // Use Twilio (SMS service) to send notification to all speakers and to recieve also commands
  def isTwilioSMSActive():Boolean = Play.current.configuration.getBoolean("cfp.twilioSMS.active").getOrElse(false)

  def twilioAccountSid:String =  Play.current.configuration.getString("cfp.twilioSMS.accountSid").getOrElse("")

  def twilioAuthToken:String =  Play.current.configuration.getString("cfp.twilioSMS.authToken").getOrElse("")

  def twilioSenderNumber:String =  Play.current.configuration.getString("cfp.twilioSMS.senderNumber").getOrElse("")

  def twilioMockSMS:Boolean =  Play.current.configuration.getBoolean("cfp.twilioSMS.mock").getOrElse(true)

  def gluonAuthorization(): String = Play.current.configuration.getString("gluon.auth.token").getOrElse(RandomStringUtils.random(16))
  def gluonInboundAuthorization(): String = Play.current.configuration.getString("gluon.inbound.token").getOrElse(RandomStringUtils.random(16))
  def gluonUsername(): String = Play.current.configuration.getString("gluon.username").getOrElse("")
  def gluonPassword(): String = Play.current.configuration.getString("gluon.password").getOrElse(RandomStringUtils.random(16))

  def maxProposals(): Int = Play.current.configuration.getInt("cfp.max.proposals").getOrElse(15)
}
