package com.github.scaduku.heuristic

import com.github.scaduku.{Cell, Grid}

abstract class NonRecursingHeuristic extends Heuristic {

def eliminate( grid : Grid ) : Int = {
    reduceGrid(grid)
  }

  def reduceGrid( grid : Grid ) : Int = {

    for( row <- grid.rows ){
      val c = reduce( grid, row )
      if( c > 0 ){
        return c
      }
    }

    for( col <- grid.cols ){
      val c = reduce( grid, col )
      if( c > 0 ){
        return c
      }
    }

    for( sub <- grid.subs ){
      val c = reduce( grid, sub )
      if( c > 0 ){
        return c
      }
    }

    0
  }

  def reduce( grid : Grid, cells : List[Cell] ) : Int

}
