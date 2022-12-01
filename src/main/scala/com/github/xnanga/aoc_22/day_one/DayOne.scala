package com.github.xnanga.aoc_22.day_one

import scala.collection.mutable.ListBuffer
import scala.io.Source.fromFile

object DayOne extends App {
  val individualCalorieList = fromFile("src/main/resources/DayOneInput.txt").getLines.toList.map((s: String) => s match {
    case "" => 0
    case _ => s.toInt
  })

  def addCaloriesTogether(calorieList: List[Int]): ListBuffer[Int] = {
    val totals = new ListBuffer[Int]
    var accum = 0
    calorieList.foreach(count => count match {
      case 0 => {
        totals += accum
        accum = 0
      }
      case _ => {
        accum += count
      }
    })
    totals
  }

  def addTopThreeHighestInts(list: ListBuffer[Int]): Int = {
    val orderedListDescending = list.sorted.reverse
    orderedListDescending(0) + orderedListDescending(1) + orderedListDescending(2)
  }

  val groupedCalorieList = addCaloriesTogether(individualCalorieList)
  val highestCalorieCount = groupedCalorieList.sorted.reverse.head
  val topThreeGroupedCaloriesAddedUp = addTopThreeHighestInts(groupedCalorieList)

  println(s"Part 1: Highest amount of calories ${highestCalorieCount}")
  println(s"Part 2: Top 3 grouped calorie counts added together ${topThreeGroupedCaloriesAddedUp}")
}
