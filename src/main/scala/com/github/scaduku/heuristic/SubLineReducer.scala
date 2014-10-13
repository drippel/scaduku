package com.github.scaduku.heuristic

import com.github.scaduku.{Cell, Grid}

class SubLineReducer extends Heuristic {

  override def eliminate(grid : Grid) : Int = {
    reduceSubs( grid, grid.subs.toSet )
  }

  def reduceSubs( grid : Grid, subs : Set[List[Cell]], accum : Int = 0  ) : Int = {

    if( accum > 0 || subs.isEmpty ){
      accum
    }
    else {
      val sub = subs.head
      val rows = sub.map((c) => { grid.findRow(c) }).toSet
      val cols = sub.map((c) => { grid.findCol(c) }).toSet
      val reduced = reduceSub(grid, sub, (rows ++ cols))
      reduceSubs( grid, subs.tail, reduced )
    }
  }

  def reduceSub( grid : Grid, sub : List[Cell], lines : Set[List[Cell]], accum : Int = 0 ) : Int = {

    if( accum > 0 || lines.isEmpty ) {
      accum
    }
    else {
      val reduced = reduceSubInner( grid, sub, lines.head )
      reduceSub( grid, sub, lines.tail, reduced )
    }
  }

  def reduceSubInner( grid : Grid, sub : List[Cell], line : List[Cell] ) : Int = {

    val doubles  = findDoubles( unsolved( line ) )

    val doubleMap = doubles.map( (d) => {
      val cs = hasPossible( line, d )
      (d,cs)
    })

    val doublesInSub = doubleMap.filter( (m) => {
      val cs = m._2
      cs.forall( (c) => {
        sub.contains(c)
      })
    })

    //
    val cellsNotInLine = sub.toSet.diff( line.toSet ).toList

    // is the double also a possible in the cellsNotInLine?
    for( dis <- doublesInSub ) {
      val possiblesNotInLine = hasPossible( cellsNotInLine, dis._1 )
      if( !possiblesNotInLine.isEmpty ){
        possiblesNotInLine.foreach( (c) => { c.eliminate(dis._1 ) } )
        return possiblesNotInLine.size
      }
    }

    0
  }
}
