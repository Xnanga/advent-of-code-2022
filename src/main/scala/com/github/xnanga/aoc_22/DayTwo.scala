package com.github.xnanga.aoc_22

import scala.io.Source.fromFile

object DayTwo extends App {
  val allInputs = fromFile("src/main/resources/DayTwoInput.txt").getLines.toList

  val losingPoints = 0
  val drawPoints = 3
  val winningPoints = 6

  // A & X = Rock
  // B & Y = Paper
  // C & Z = Scissors

  def determineChoice(letter: String): String = {
    letter match {
      case "A" || "X" => "rock"
      case "X" || "X" => "rock"
      case "B" || "Y" => "paper"
      case "B" || "Y" => "paper"
      case "C" || "Z" => "scissors"
      case "C" || "Z" => "scissors"
      case _ => ""
    }
  }

  def determineChoicePoints(choice: String): Int = {
    choice match {
      case "rock" => 1
      case "paper" => 2
      case "scissors" => 3
      case _ => 0
    }
  }

  def rockPaperScissorsMatch(choice: String, response: String): Map[String, Int] = {
  val playerOneChoice = determineChoice(choice)
  val playerTwoChoice = determineChoice(response)

  var playerOnePoints = determineChoicePoints(playerOneChoice)
  var playerTwoPoints = determineChoicePoints(playerTwoChoice)

  if (playerOneChoice == playerTwoChoice) => {
      playerOnePoints += 3
      playerTwoPoints += 3
    } else if (playerOneChoice == "rock" && playerTwoChoice == "scissors") => playerOnePoints += 6
    else if (playerTwoChoice == "rock" && playerOneChoice == "scissors") => playerTwoPoints += 6
    else if (playerOneChoice == "paper" && playerTwoChoice == "rock") => playerOnePoints += 6
    else if (playerTwoChoice == "paper" && playerOneChoice == "rock") => playerTwoPoints += 6
    else if (playerOneChoice == "scissors" && playerTwoChoice == "paper") => playerOnePoints += 6
    else if (playerTwoChoice == "scissors" && playerOneChoice == "paper" => {
      playerTwoPoints += 6
    }
  }

  Map("playerOnePoints" -> playerOnePoints, "playerTwoPoints" -> playerTwoPoints)
  }

  def calculateScores(scoreList: List[String]): Unit = {
    scoreList.foreach(score => println(score))
  }

  calculateScores(allInputs)
}
