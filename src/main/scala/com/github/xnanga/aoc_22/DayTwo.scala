package com.github.xnanga.aoc_22

import scala.collection.immutable.HashMap
import scala.io.Source.fromFile

object DayTwo extends App {
  private val LOSING_POINTS = 0
  private val DRAW_POINTS = 3
  private val WINNING_POINTS = 6

  private val allInputs = fromFile("src/main/resources/DayTwoInput.txt").getLines.toList

  private def parseInput(input: String): List[Char] = {
    val inputNoWhitespace = input.filterNot(_.isWhitespace)
    List(inputNoWhitespace(0), inputNoWhitespace(1))
  }

  private val allParsedInputs = allInputs.toList.map(input => parseInput(input))

  private def determineOpponentChoice(letter: Char): String = {
    letter match {
      case 'A' => "rock"
      case 'B'=> "paper"
      case 'C' => "scissors"
      case _ => ""
    }
  }

  private def determineChoicePoints(choice: String): Int = {
    choice match {
      case "rock" => 1
      case "paper" => 2
      case "scissors" => 3
      case _ => 0
    }
  }

  private def determinePlayerChoice(opponentChoice: String, winner: String): String = {
    if (winner == "draw") {
      opponentChoice
    } else if (winner == "player") {
      opponentChoice match {
        case ("rock") => "paper"
        case ("paper") => "scissors"
        case ("scissors") => "rock"
      }
    } else if (winner == "opponent") {
      opponentChoice match {
        case ("rock") => "scissors"
        case ("paper") => "rock"
        case ("scissors") => "paper"
      }
    } else {
      "Error(s) with input"
    }
  }

  private def determineGameOutcome(letter: Char): String = {
    letter match {
      case 'X' => "lose"
      case 'Y' => "draw"
      case 'Z' => "win"
      case _ => ""
    }
  }

  private def determineWinner(gameOutcome: String): String = {
    gameOutcome match {
      case "draw" => "draw"
      case "win" => "player"
      case "lose" => "opponent"
      case _ => "Error(s) with input(s)"
    }
  }

  private def calculateScores(inputAndOutcomeList: List[Char]): List[Int] = {
    val opponentChoice = determineOpponentChoice(inputAndOutcomeList(0))
    val opponentChoicePoints = determineChoicePoints(opponentChoice)

    val decidedOutcome = determineGameOutcome(inputAndOutcomeList(1))
    val matchOutcome = determineWinner(decidedOutcome)

    val playerChoice = determinePlayerChoice(opponentChoice, matchOutcome)
    val playerChoicePoints = determineChoicePoints(playerChoice)

    if (matchOutcome == "draw") {
      List(playerChoicePoints + DRAW_POINTS, opponentChoicePoints + DRAW_POINTS)
    } else if (matchOutcome == "player") {
      List(playerChoicePoints + WINNING_POINTS, opponentChoicePoints)
    } else if (matchOutcome == "opponent") {
      List(playerChoicePoints, opponentChoicePoints + WINNING_POINTS)
    } else {
      println("Error(s) with score inputs")
      List(0, 0)
    }
  }

  private def runStrategyGuide(inputList: List[List[Char]]): HashMap[String, Int] = {
    var playerTotalScore = 0
    var opponentTotalScore = 0

    inputList.foreach(input => {
      val outcomeScores = calculateScores(input)
      playerTotalScore += outcomeScores(0)
      opponentTotalScore += outcomeScores(1)
    })

    HashMap("playerTotalScore" -> playerTotalScore, "opponentTotalScore" -> opponentTotalScore)
  }

  private val finalCalculatedScores = runStrategyGuide(allParsedInputs)
  println(s"Player Total Score: ${finalCalculatedScores("playerTotalScore")}\nOpponent Total Score: ${finalCalculatedScores("opponentTotalScore")}")
}
