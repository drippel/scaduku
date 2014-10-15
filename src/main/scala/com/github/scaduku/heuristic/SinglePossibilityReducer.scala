package com.github.scaduku.heuristic

import com.github.scaduku.{Group, Cell, Grid}

import scala.collection.mutable

class SinglePossibilityReducer extends NonRecursingHeuristic {

  val simple = new SimpleReducer()

  override def reduce( grid : Grid, group : Group ) : Int = {

    // get all the unsolved cells
    val unsolvedCells = group.unsolved

    // get counts
    val countMap = buildCountMap(unsolvedCells)

    val singles = countMap.filter( ( pair ) => { pair._2 == 1 } )
    val singleValues = singles.map( (p) => { p._1 } )

    if( !singleValues.isEmpty ){
      val s = singleValues.head
      val cell = unsolvedCells.filter( (c) => { c.isPossible(s) } ).head
      cell.set(s)
      return 1
    }

    0
  }

}
