package models.baseImpl

import services.{CityCalculator, PathogenCalculator, TableService}
import models.{CityInterface, GameInterface, PathogenInterface}
import play.api.libs.json._

case class Game(round: Int, points: Int, outcome: String, jsCityObject: JsValue, events: JsValue) extends GameInterface {

  /** **************************************************************************************************************
    * ***************************************** Reads *************************************************************
    * ***************************************************************************************************************/

  implicit val readCity: Reads[City] = new Reads[City] {
    def reads(js: JsValue): JsResult[City] = {
      val events: JsValue = if (js.as[JsObject].keys.contains("events")) {
        (js \ "events").as[JsValue]
      } else {
        null
      }
      val outbreaks: Map[PathogenInterface, Float] = events match {
        case a: JsArray => {
          val tempOutbrakes = a.value.filter { js => (js \ "type").as[String] == "outbreak" }
            .map(js => Map((js \ "pathogen").as[Pathogen].asInstanceOf[PathogenInterface] -> (js \ "prevalence").as[Float]))
          if (tempOutbrakes.length > 1) {
            tempOutbrakes.reduce(_ ++ _)
          } else if (tempOutbrakes.length == 1) {
            tempOutbrakes.apply(0)
          } else {
            null
          }
        }
        case _ => null
      }
      val vaccines: Vector[Vaccine] = events match {
        case a: JsArray => {
          a.value.filter { js => (js \ "type").as[String] == "vaccineDeployed" }
            .map(js => js.as[Vaccine]).toVector
        }
        case _ => Vector()
      }
      JsSuccess(City((js \ "name").as[String],
        (js \ "latitude").as[Float],
        (js \ "longitude").as[Float],
        (js \ "population").as[Int],
        (js \ "connections").as[Vector[String]],
        (js \ "economy").as[String],
        (js \ "government").as[String],
        (js \ "hygiene").as[String],
        (js \ "awareness").as[String],
        outbreaks,
        vaccines,
        events
      ))
    }
  }

  implicit val readPathogen: Reads[Pathogen] = new Reads[Pathogen] {
    def reads(js: JsValue): JsResult[Pathogen] = {
      JsSuccess(Pathogen((js \ "name").as[String],
        (js \ "infectivity").as[String],
        (js \ "mobility").as[String],
        (js \ "duration").as[String],
        (js \ "lethality").as[String]))
    }
  }

  implicit val readMedicine: Reads[Medicine] = new Reads[Medicine] {
    def reads(js: JsValue): JsResult[Medicine] = {
      JsSuccess(Medicine((js \ "pathogen").as[Pathogen]))
    }
  }

  implicit val readVaccine: Reads[Vaccine] = new Reads[Vaccine] {
    def reads(js: JsValue): JsResult[Vaccine] = {
      JsSuccess(Vaccine((js \ "pathogen").as[Pathogen]))
    }
  }

  /** *************************************************************************************************************
    * *********************************************** Vals ********************************************************
    * *************************************************************************************************************/

  override val cityNames: Vector[String] = allKeys(jsCityObject)
  override val cities: Map[String, City] = addCityToMap(cityNames.length - 1)
  override val pathogens: Vector[Pathogen] = events match {
    case a: JsArray => {
      a.value.filter(entry => (entry \ "type").as[String] == "pathogenEncountered")
        .map(entry => (entry \ "pathogen").as[Pathogen]).toVector
    }
    case _ => Vector()
  }
  override val medicines: Vector[Medicine] = events match {
    case a: JsArray => {
      a.value.filter(entry => (entry \ "type").as[String] == "medicationAvailable")
        .map(entry => entry.as[Medicine].completeResearch()).toVector ++
        a.value.filter(entry => (entry \ "type").as[String] == "medicationInDevelopment")
          .map(entry => entry.as[Medicine].setProgress(round - (entry \ "sinceRound").as[Int])).toVector
    }
    case _ => Vector()
  }
  override val vaccines: Vector[Vaccine] = events match {
    case a: JsArray =>
      a.value.filter(entry => (entry \ "type").as[String] == "vaccineAvailable")
        .map(entry => entry.as[Vaccine].completeResearch()).toVector ++
        a.value.filter(entry => (entry \ "type").as[String] == "vaccineInDevelopment")
          .map(entry => entry.as[Vaccine].setProgress(round - (entry \ "sinceRound").as[Int])).toVector
    case _ => Vector()
  }
  override val otherEvents: Vector[JsValue] = events match {
    case a: JsArray =>
      a.value.filter(entry => {
        val actualType = (entry \ "type").as[String]
        actualType != "vaccineAvailable" &&
          actualType != "vaccineInDevelopment" &&
          actualType != "medicationAvailable" &&
          actualType != "medicationInDevelopment" &&
          actualType != "pathogenEncountered"
      }).filter(entry => {
        (entry \ "sinceRound").as[Int] == round
      }).toVector
  }

  /** *************************************************************************************************************
    * ********************************************* Utility *******************************************************
    * *************************************************************************************************************/

  def addCityToMap(index: Int): Map[String, City] = {
    val name = cityNames.apply(index)
    val city = (jsCityObject \ name).as[City]
    if (index == 0) {
      Map(name -> city)
    } else {
      addCityToMap(index - 1).+(name -> city)
    }
  }

  def allKeys(json: JsValue): Vector[String] = json match {
    case o: JsObject => o.keys.toVector
    case _ => Vector()
  }

  override def prettyPrint(): String = {
    val totalPopulation = cities.values.map(city => city.population).sum

    "***********************************************************************************\n" +
      "*************************************** Round " + round + " **************************************\n" +
      "***********************************************************************************\n\n" +
      "Points: *" + points + "\t\t\tOutcome: *" + outcome + "*\n" +
      "Total Population: *" + totalPopulation + "*\n" +
      //"Pathogens:\n|\tTyp\t|\tVac\t|\tMed\t|\tCit\t|\tRate\t|\tInf\t|\tMob\t|\tDur\t|\tLet\t|" + prettyPrintAppendPathogens(0)
      "Pathogens:\n" +
      TableService().format(Vector(Vector("Name", "Typ", "Vaccine", "Medicine", "Cities", "Infected", "Inf", "Mob", "Dur", "Let")) ++
        pathogens.map(p => {
          val totalPathogenInfection = PathogenCalculator().getTotalInfectedDudes(this, p)
          val vac: String = if (PathogenCalculator().hasVaccineAvailable(this, p)) {
            "O"
          } else {
            "X"
          }
          val med = if (PathogenCalculator().hasMedicineAvailable(this, p)) {
            "O"
          } else {
            "X"
          }
          Vector(p.name, PathogenCalculator().getPathogenType(this, p), vac, med,
            CityCalculator().numberOfInfectedCities(this, p),
            BigDecimal(totalPathogenInfection.toFloat / totalPopulation.toFloat * 100)
              .setScale(3, BigDecimal.RoundingMode.HALF_DOWN).toFloat + " %",
            p.infectivity, p.mobility, p.duration, p.lethality)
        })) +
      "\n" +
      /*"Infected Cities:\n" +
      TableService().format(Vector(Vector("Name", "Population", "Infected", "Percent", "Pathogens")) ++
        cities.values.filter(c => c.pathogens != null).map(c => {
          val checkedName = c.name match {
            case arabic if arabic.chars().anyMatch(x => x >= 0x0600 && x <= 0x06E0) => "Not Displayable"
            case x => x
          }
          Vector(checkedName, c.population, c.pathogens.keys.map(p => c.population * c.pathogens.apply(p)).sum,
            BigDecimal(c.pathogens.keys.map(p => c.population * c.pathogens.apply(p)).sum / c.population.toFloat)
              .setScale(3, BigDecimal.RoundingMode.HALF_DOWN).toFloat + " %", c.pathogens.keys.map(p => p.name + "(" +
              BigDecimal(c.pathogens.apply(p) * 100).setScale(3, BigDecimal.RoundingMode.HALF_DOWN).toFloat + " %)").mkString(", "))
        })) +
      "\n" +*/
      "In Development:\n" +
      medicines.filter(m => m.status == "developing").map(m => "Medicine: " + m.pathogen.name + "\t\t(" + m.progress + ")").mkString("\n") +
      "\n" +
      vaccines.filter(v => v.status == "developing").map(v => "Vaccine: " + v.pathogen.name + "\t\t(" + v.progress + ")").mkString("\n") +
      "\n" +
      "Events encountered:\n" +
      otherEvents.map(e => (e \ "type").as[String]).mkString(", ") +
      "\n"
  }
}
