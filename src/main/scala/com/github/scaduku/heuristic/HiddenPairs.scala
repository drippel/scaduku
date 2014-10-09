package com.github.scaduku.heuristic

import com.github.scaduku.{Cell, Grid}

import scala.collection.mutable.ListBuffer

class HiddenPairs extends NonRecursingHeuristic {

  def reduce( grid : Grid, cells : List[Cell] ) : Int = {
    findHiddenPairs(cells)
    0
  }

  def findHiddenPairs( cells : List[Cell] ) : List[(Cell,Cell)] = {

    val pairs = ListBuffer[(Cell,Cell)]()

    val counts = buildCountMap( unsolved( cells ) )
    val doubles = counts.filter( (t) => { t._2 == 2 } )


    val doubleCells = doubles.map( (d) => { (d,hasPossible( unsolved(cells), d._1 )) } )

    Console.println(doubleCells)

    val doubleCounts = Map

    pairs.toList
  }
}
