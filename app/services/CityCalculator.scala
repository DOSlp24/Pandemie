package services

import models.{CityInterface, GameInterface, PathogenInterface}

case class CityCalculator() {
  val NORMED_THRESHOLD: Number = 0.3

  def getTotalPopulation(game: GameInterface): Int = {
    game.cities.values.map(c => c.population).sum
  }

  def numberOfInfectedCities(game: GameInterface, pathogen: PathogenInterface): Int = {
    infectedCities(game, pathogen).length
  }

  def infectedCities(game: GameInterface, pathogen: PathogenInterface): Vector[CityInterface] = {
    game.cities.values.filter(c => {
      if (c.pathogens != null) {
        c.pathogens.keys.exists(p => p == pathogen)
      } else {
        false
      }
    }).toVector
  }

  def mostInfectedCity(game: GameInterface, pathogen: PathogenInterface): Option[CityInterface] = {
    val citiesInfected = game.cities.filter(c => c._2.pathogens != null).filter(c => c._2.pathogens.keys.exists(p => p == pathogen))
    if (citiesInfected.size == 1) {
      Some(citiesInfected.last._2)
    } else if (citiesInfected.size > 1) {
      Some(citiesInfected.reduce((a, b) => {
        if (a._2.pathogens.apply(pathogen) * a._2.population >= b._2.pathogens.apply(pathogen) * b._2.population) {
          a
        } else {
          b
        }
      })._2)
    } else {
      None
    }
  }

  def leastInfectedCity(game: GameInterface, pathogen: PathogenInterface): Option[CityInterface] = {
    val citiesInfected = game.cities.filter(c => c._2.pathogens != null).filter(c => c._2.pathogens.keys.exists(p => p == pathogen))
    if (citiesInfected.size == 1) {
      Some(citiesInfected.last._2)
    } else if (citiesInfected.size > 1) {
      Some(citiesInfected.reduce((a, b) => {
        if (a._2.pathogens.apply(pathogen) * a._2.population <= b._2.pathogens.apply(pathogen) * b._2.population) {
          a
        } else {
          b
        }
      })._2)
    } else {
      None
    }
  }

  def getNormedInfectionRate(game: GameInterface, pathogen: PathogenInterface): Int = {
    val citiesInfected = game.cities.filter(c => c._2.pathogens != null)
      .filter(c => c._2.pathogens.keys.exists(p => p == pathogen))
      .count(c => c._2.pathogens.apply(pathogen) >= NORMED_THRESHOLD.floatValue())
    citiesInfected / 2
  }
}
