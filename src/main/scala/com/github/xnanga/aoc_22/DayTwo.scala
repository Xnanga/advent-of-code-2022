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

  private def determineChoice(letter: Char): String = {
    letter match {
      case ('A' | 'X') => "rock"
      case ('B' | 'Y') => "paper"
      case ('C' | 'Z') => "scissors"
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

  private def determineWinner(playerOneChoice: String, playerTwoChoice: String): String = {
    if (playerOneChoice == playerTwoChoice) {
      "draw"
    } else if (playerOneChoice == "rock") {
      if (playerTwoChoice == "paper") {
        "playerTwo"
      } else {
        "playerOne"
      }
    } else if (playerOneChoice == "paper") {
      if (playerTwoChoice == "scissors") {
        "playerTwo"
      } else {
        "playerOne"
      }
    } else if (playerOneChoice == "scissors") {
      if (playerTwoChoice == "rock") {
        "playerTwo"
      } else {
        "playerOne"
      }
    } else {
      "error with input(s)"
    }
  }

  private def calculateScores(inputList: List[Char]): List[Int] = {
    val playerOneChoice = determineChoice(inputList(0))
    val playerOneChoicePoints = determineChoicePoints(playerOneChoice)
    val playerTwoChoice = determineChoice(inputList(1))
    val playerTwoChoicePoints = determineChoicePoints(playerTwoChoice)
    val matchOutcome = determineWinner(playerOneChoice, playerTwoChoice)

    if (matchOutcome == "draw") {
      List(playerOneChoicePoints + DRAW_POINTS, playerTwoChoicePoints + DRAW_POINTS)
    } else if (matchOutcome == "playerOne") {
      List(playerOneChoicePoints + WINNING_POINTS, playerTwoChoicePoints + LOSING_POINTS)
    } else if (matchOutcome == "playerTwo") {
      List(playerOneChoicePoints + LOSING_POINTS, playerTwoChoicePoints + WINNING_POINTS)
    } else {
      println("Error(s) with score inputs")
      List(0, 0)
    }
  }

  private def runStrategyGuide(inputList: List[List[Char]]): HashMap[String, Int] = {
    var playerOneTotalScore = 0
    var playerTwoTotalScore = 0

    inputList.foreach(input => {
      val outcomeScores = calculateScores(input)
      playerOneTotalScore += outcomeScores(0)
      playerTwoTotalScore += outcomeScores(1)
    })

    HashMap("playerOneScore" -> playerOneTotalScore, "playerTwoScore" -> playerTwoTotalScore)
  }

  private val finalCalculatedScores = runStrategyGuide(allParsedInputs)
  println(s"PlayerOneScore: ${finalCalculatedScores("playerOneScore")}\nPlayerTwoScore: ${finalCalculatedScores("playerTwoScore")}")
}
