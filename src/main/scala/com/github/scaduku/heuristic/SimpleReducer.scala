package com.github.scaduku.heuristic

import com.github.scaduku.{Group, Cell, Grid}

class SimpleReducer extends GroupBasedHeuristic {

  def reduce( grid : Grid, group : Group ) : Int = {

    var count = 0

    val unsolvedCells = group.unsolved

    val solvedCells = group.solved
    val solvedValues = solvedCells.map((c) => { c.value() })

    if( !unsolvedCells.isEmpty ) {

      for( unsolvedCell <- unsolvedCells ){

        for( solvedValue <- solvedValues ){

          if( unsolvedCell.isPossible(solvedValue) ){
            unsolvedCell.eliminate(solvedValue)
            count += 1

          }
        }
      }
    }

    count
  }
}
