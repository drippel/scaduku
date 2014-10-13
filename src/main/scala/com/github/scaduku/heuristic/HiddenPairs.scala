package com.github.scaduku.heuristic

import com.github.scaduku.{Cell, Grid}

class HiddenPairs extends NonRecursingHeuristic {

  def reduce( grid : Grid, cells : List[Cell] ) : Int = {
    val hiddenPairs = findHiddenPairs(cells)
    for( hp <- hiddenPairs ){
      reduceHiddenPairs( hp._1, hp._2 ) match {
        case 0 => {}
        case i : Int => { return i }
      }
    }
    0
  }

  def reduceHiddenPairs( cells : Set[Cell], pair : Set[Int] ) : Int = {

    var count = 0
    for( cell <- cells ) {
      val possibles = cell.possibleValues().toSet
      val diff = possibles.diff( pair )
      for( d <- diff ){
        cell.eliminate(d)
        count += 1
      }
    }

    count
  }

  def findHiddenPairs( cells : List[Cell] ) = {

    //
    val unsolvedCells = unsolved(cells)

    // get the possibles that have two cells left
    val doubles = findDoubles( unsolvedCells )

    // the cells containing the possibles with 2 left in the group
    val doubleCells = doubles.map( (d) => { ( d,hasPossible( unsolvedCells, d )) } )

    val sets = doubleCells.map( (m) => { m._2 }).toList
    val setMap = sets.groupBy( (s) => { s } )
    val pairsSets = setMap.filter( (s) => { s._2.length == 2 }).keys.toList

    for( ps <- pairsSets ) yield {
      val p1 = ps.head.possibleValues().toSet
      val p2 = ps.tail.head.possibleValues().toSet
      val common = p1.intersect(p2)
      val pair = common.intersect(doubles.toSet)
      (ps,pair)
    }
  }
}
