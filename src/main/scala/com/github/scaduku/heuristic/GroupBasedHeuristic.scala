package com.github.scaduku.heuristic

import com.github.scaduku.{Group, Cell, Grid}

abstract class GroupBasedHeuristic extends Heuristic {

def eliminate( grid : Grid ) : Int = {
    reduce(grid)
  }

  def reduce( grid : Grid, sum : Int = 0 ) : Int = {

    val count = reduceGrid(grid)
    if( count == 0 ){
      sum
    }
    else {
      reduce( grid, ( count + sum ) )
    }

  }

  def reduceGrid( grid : Grid ) : Int = {

    var count = 0

    val rowCounts = grid.rows.map( (r) => { reduce( grid, r) } )
    count = rowCounts.foldRight(count)( (x,y) => { x + y } )
    val colCounts = grid.cols.map( (c) => { reduce( grid, c) } )
    count = colCounts.foldRight(count)( (x,y) => { x + y } )
    val subCounts = grid.subs.map( (s) => { reduce(grid, s) })
    count = subCounts.foldRight(count)( (x,y) => { x + y } )

    count
  }

  def reduce( grid : Grid, group : Group ) : Int

}
