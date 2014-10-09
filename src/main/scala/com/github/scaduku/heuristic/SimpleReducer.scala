package com.github.scaduku.heuristic

import com.github.scaduku.{Cell, Grid}

class SimpleReducer extends GroupBasedHeuristic {

  def reduce( grid : Grid, cells : List[Cell] ) : Int = {

    var count = 0

    val unsolvedCells = unsolved( cells )

    val solvedCells = solved( cells )
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
