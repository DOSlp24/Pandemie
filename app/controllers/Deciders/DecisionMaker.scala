package controllers.Deciders

import models.GameInterface

trait DecisionMaker {

  def decide(game: GameInterface): String

  def endRound(): String = {
    """{"type": "endRound"}"""
  }

  def putUnderQuarantine(city: String, rounds: Int): String = {
    "{\"type\": \"putUnderQuarantine\", \"city\": \"" + city + "\", \"rounds\": \"" + rounds + "\"}"
  }

  def closeAirport(city: String, rounds: Int): String = {
    "{\"type\": \"closeAirport\", \"city\": \"" + city + "\", \"rounds\": \"" + rounds + "\"}"
  }

  def closeConnection(from: String, to: String, rounds: Int): String = {
    "{\"type\": \"closeConnection\", \"fromCity\": \"" + from + "\", \"toCity\": \"" + to + "\", \"rounds\": \"" + rounds + "\"}"
  }

  def devVaccine(pathogen: String): String = {
    "{\"type\": \"developVaccine\", \"pathogen\": \"" + pathogen + "\"}"
  }

  def depVaccine(pathogen: String, city: String): String = {
    "{\"type\": \"deployVaccine\", \"pathogen\": \"" + pathogen + "\", \"city\": \"" + city + "\"}"
  }

  def devMed(pathogen: String): String = {
    "{\"type\": \"developMedication\", \"pathogen\": \"" + pathogen + "\"}"
  }

  def depMed(pathogen: String, city: String): String = {
    "{\"type\": \"deployMedication\", \"pathogen\": \"" + pathogen + "\", \"city\": \"" + city + "\"}"
  }

  def exertInfluence(city: String): String = {
    "{\"type\": \"exertInfluence\", \"city\": \"" + city + "\"}"
  }

  def callElections(city: String): String = {
    "{\"type\": \"callElections\", \"city\": \"" + city + "\"}"
  }

  def applyHygienicMeasures(city: String): String = {
    "{\"type\": \"applyHygienicMeasures\", \"city\": \"" + city + "\"}"
  }

  def launchCampaign(city: String): String = {
    "{\"type\": \"launchCampaign\", \"city\": \"" + city + "\"}"
  }
}
