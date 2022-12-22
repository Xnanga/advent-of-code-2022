package com.github.xnanga.aoc_22

import scala.io.Source.fromFile

object DayThree extends App {
  private val allRucksacks = fromFile("src/main/resources/DayThreeInput.txt").getLines.toList

  private def separateRucksackCompartments(rucksack: String): List[String] = {
    val compartmentSize = rucksack.length / 2;
    val compartmentOne = rucksack.slice(0, compartmentSize)
    val compartmentTwo = rucksack.slice(compartmentSize, rucksack.length)
    List(compartmentOne, compartmentTwo)
  }

  private def findCommonItemType(compartments: List[String], iterations: Int = 0): Char = {
    val compartmentOneArr = compartments(0).split("")
    val compartmentTwoArr = compartments(1).split("")
    val commonItemType = compartmentTwoArr.indexOf(compartmentOneArr(iterations))

    if (commonItemType != -1) {
      compartmentTwoArr(commonItemType).charAt(0)
    } else {
      findCommonItemType(compartments, iterations + 1)
    }
  }

  private def determinePriorityNumber(letter: Char): Int = {
    val lowercaseCharCodePoint = 1
    val uppercaseCharCodePoint = 27

    if (letter.isUpper == true) {
      letter.toInt - 'A' + uppercaseCharCodePoint
    } else if (letter.isLower == true) {
      letter.toInt - 'a' + lowercaseCharCodePoint
    } else {
      0
    }
  }

  private def calculateSumOfPriorities(rucksacks: List[String]): Int = {
    var accum = 0

    rucksacks.foreach(rucksack => {
      val compartments = separateRucksackCompartments(rucksack)
      val commonItemType = findCommonItemType(compartments)
      val priorityNumber = determinePriorityNumber(commonItemType)
      accum += priorityNumber
    })
    accum
  }

  private val sumOfAllPriorities = calculateSumOfPriorities(allRucksacks)
  println(s"The sum of all priorities is: ${sumOfAllPriorities}")
}
