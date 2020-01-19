package services

import models.{GameInterface, PathogenInterface}

case class PathogenCalculator() {
  /*
  * #Has Vaccine#
  * @Function:
  *   hasVaccine
  * @Description:
  *   Checks if a given Pathogen has a Vaccine available or in Development
  * @Params:
  *   game: GameInterface => Actual Game Status
  *   pathogen: PathogenInterface => Pathogen to look up
  * */
  def hasVaccine(game: GameInterface, pathogen: PathogenInterface): Boolean = {
    game.vaccines.exists(v => v.pathogen == pathogen)
  }

  /*
  * #Has Medicine#
  * @Function:
  *   hasMedicine
  * @Description:
  *   Checks if a given Pathogen has a Medicine available or in Development
  * @Params:
  *   game: GameInterface => Actual Game Status
  *   pathogen: PathogenInterface => Pathogen to look up
  * */
  def hasMedicine(game: GameInterface, pathogen: PathogenInterface): Boolean = {
    game.medicines.exists(m => m.pathogen == pathogen)
  }

  /*
  * #Has Vaccine Available#
  * @Function:
  *   hasVaccineAvailable
  * @Description:
  *   Checks if a given Pathogen has a Medicine available
  * @Params:
  *   game: GameInterface => Actual Game Status
  *   pathogen: PathogenInterface => Pathogen to look up
  * */
  def hasVaccineAvailable(game: GameInterface, pathogen: PathogenInterface): Boolean = {
    game.vaccines.exists(v => v.pathogen == pathogen && v.status == "available")
  }

  /*
  * #Has Medicine Available#
  * @Function:
  *   hasVaccineAvailable
  * @Description:
  *   Checks if a given Pathogen has a Vaccine available
  * @Params:
  *   game: GameInterface => Actual Game Status
  *   pathogen: PathogenInterface => Pathogen to look up
  * */
  def hasMedicineAvailable(game: GameInterface, pathogen: PathogenInterface): Boolean = {
    game.medicines.exists(m => m.pathogen == pathogen && m.status == "available")
  }


  /*
  * #Prio Alpha Pathogen#
  * @Function:
  *   getPrioritisedAlphaPathogen
  * @Description:
  *   returns the most dangerous Pathogen which doesn't have neither a Medicine nor Vaccine Av or DEV
  * @Params:
  *   game: GameInterface => Actual Game Status
  * */
  def getPrioritisedAlphaPathogen(game: GameInterface): Option[PathogenInterface] = {
    val pathogens = game.pathogens.filter(p => !hasMedicine(game, p) && !hasVaccine(game, p))
    getPrioritisedPathogen(game, pathogens)
  }

  /*
  * #Prio Beta Pathogen#
  * @Function:
  *   getPrioritisedBetaPathogen
  * @Description:
  *   returns the most dangerous Pathogen which doesn't have a Medicine Av or DEV
  * @Params:
  *   game: GameInterface => Actual Game Status
  * */
  def getPrioritisedBetaPathogen(game: GameInterface): Option[PathogenInterface] = {
    val pathogens = game.pathogens.filter(p => !hasMedicine(game, p) && hasVaccine(game, p))
    getPrioritisedPathogen(game, pathogens)
  }

  /*
  * #Prio Delta Pathogen#
  * @Function:
  *   getPrioritisedDeltaPathogen
  * @Description:
  *   returns the most dangerous Pathogen which doesn't have a Vaccine Av or DEV
  * @Params:
  *   game: GameInterface => Actual Game Status
  * */
  def getPrioritisedDeltaPathogen(game: GameInterface): Option[PathogenInterface] = {
    val pathogens = game.pathogens.filter(p => hasMedicine(game, p) && !hasVaccine(game, p))
    getPrioritisedPathogen(game, pathogens)
  }

  /*
  * #Prio Omega Pathogen#
  * @Function:
  *   getPrioritisedOmegaPathogen
  * @Description:
  *   returns the most dangerous Pathogen which has a Vaccine and Medicine Av or DEV
  * @Params:
  *   game: GameInterface => Actual Game Status
  * */
  def getPrioritisedOmegaPathogen(game: GameInterface): Option[PathogenInterface] = {
    val pathogens = game.pathogens.filter(p => hasMedicine(game, p) && hasVaccine(game, p))
    getPrioritisedPathogen(game, pathogens)
  }

  def getPrioritisedPathogen(game: GameInterface, pathogens: Seq[PathogenInterface]): Option[PathogenInterface] = {
    if (pathogens.length == 1) {
      Some(pathogens.apply(0))
    } else if (pathogens.length > 1) {
      Some(pathogens.map(p => (pathogenValue(p), p)).reduce((a, b) => {
        if (a._1 >= b._1) {
          a
        } else {
          b
        }
      })._2)
    } else {
      None
    }
  }

  def getPrioritisedNemesisPathogens(game: GameInterface): Option[Vector[PathogenInterface]] = {
    val nonDefeatedPathogens = game.pathogens.filter(p => getTotalInfectedDudes(game, p) > 0)
    if (nonDefeatedPathogens.length == 1) {
      Some(nonDefeatedPathogens)
    } else if (nonDefeatedPathogens.length > 1) {
      Some(nonDefeatedPathogens.map(p => (getTotalInfectedDudes(game, p), p)).map(entry => entry._2))
    } else {
      None
    }
  }

  def getPrioritisedAlphaNemesisPathogens(game: GameInterface): Option[Vector[PathogenInterface]] = {
    val nonDefeatedPathogens = game.pathogens.filter(p => getTotalInfectedDudes(game, p) > 0).filter(p => !hasVaccine(game, p) && !hasMedicine(game, p))
    if (nonDefeatedPathogens.length == 1) {
      Some(nonDefeatedPathogens)
    } else if (nonDefeatedPathogens.length > 1) {
      Some(nonDefeatedPathogens.map(p => (getTotalInfectedDudes(game, p), p)).map(entry => entry._2))
    } else {
      None
    }
  }

  def getPathogenType(game: GameInterface, p: PathogenInterface): String = {
    if (!hasMedicine(game, p) && !hasVaccine(game, p)) {
      "Alpha"
    } else if (!hasMedicine(game, p) && hasVaccine(game, p)) {
      "Beta"
    } else if (hasMedicine(game, p) && !hasVaccine(game, p)) {
      "Delta"
    } else {
      "Omega"
    }
  }

  def getTotalInfectedDudes(game: GameInterface, p: PathogenInterface): Int = {
    CityCalculator().infectedCities(game, p).map(c => c.population * c.pathogens.apply(p)).sum.toInt
  }

  def getEstimatedLifetime(p: PathogenInterface): Int ={
    val numInf = mapIndicatorToNumeric(p.infectivity)
    val numLet = mapIndicatorToNumeric(p.lethality)
    val numDur = mapIndicatorToNumeric(p.duration)

    if(numInf + numLet >= 7) {
      2 // This pathogen will wipe out the whole city in an instance :/   Feels bad though
    } else {
      numDur * 2 + 2
    }
  }

  def pathogenValue(p: PathogenInterface): Int = {
    mapIndicatorToNumeric(p.infectivity) * 3 +
      mapIndicatorToNumeric(p.mobility) * 2 +
      mapIndicatorToNumeric(p.duration) +
      mapIndicatorToNumeric(p.lethality)
  }

  def mapIndicatorToNumeric(ind: String): Int = {
    ind match {
      case "--" => 0
      case "-" => 1
      case "o" => 2
      case "+" => 3
      case "++" => 4
      case _ => 0
    }
  }
}
