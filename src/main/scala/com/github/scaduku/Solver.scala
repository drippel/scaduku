package com.github.scaduku

import com.github.scaduku.heuristic._

class Solver {
}

object Solver {

  def reduced( grid : Grid ) : Int = {

    val simple = new SimpleReducer()
    val single = new SinglePossibilityReducer()

    var eliminated = simple.eliminate(grid)

    var count = eliminated
    while( eliminated > 0 ){
      eliminated = single.eliminate(grid)
      count += eliminated
    }

    count
  }

  def reduce( grid : Grid ) : Int = {

    val simple = new SimpleReducer()
    val heuristics = List( new SimpleReducer(), new SinglePossibilityReducer(), new NakedPairs(), new HiddenPairs(), new SubLineReducer() )

    var eliminated = simple.eliminate(grid)

    var count = eliminated
    while( eliminated > 0 ){
      eliminated = heuristics.foldLeft(0)(
        (i,h) => {
          if( i == 0 ){
            h.eliminate(grid)
          }
          else {
           i
          }
        })
      count += eliminated
    }

    count
  }
}
