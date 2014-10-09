package com.github.scaduku.heuristic

import com.github.scaduku.{Cell, Grid}

import scala.collection.mutable

class SinglePossibilityReducer extends NonRecursingHeuristic {

  val simple = new SimpleReducer()

  override def reduce( grid : Grid, cells : List[Cell]) : Int = {

    // get all the unsolved cells
    val unsolvedCells = unsolved(cells)

    // get counts
    val countMap = buildCountMap(unsolvedCells)

    val singles = countMap.filter( ( pair ) => { pair._2 == 1 } )
    val singleValues = singles.map( (p) => { p._1 } )

    if( !singleValues.isEmpty ){
      val s = singleValues.head
      val cell = unsolvedCells.filter( (c) => { c.possibleValues().contains(s) } ).head
      cell.set(s)
      // simple.eliminate(grid)
      return 1
    }

    0
  }



}
