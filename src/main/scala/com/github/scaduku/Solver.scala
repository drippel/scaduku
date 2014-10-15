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



  def bruteForce( grid : Grid, solutions : List[Grid], level : Int = 0, findFirst : Boolean = true ) : List[Grid] = {

    if( !solutions.isEmpty && findFirst ){
      solutions
    }
    else {

      if( !grid.isValid() ) {
        // Console.println( level + " not valid")
        solutions
      }
      else {

        if( grid.isSolved() ) {
          // Console.println( level + " solved -----------------------------------------------------------")
          solutions ++ List(grid)
        }
        else {
          grid.unsolved() match {
            case Nil => {
              // this was the last unset cell
              // Console.println( level + " done -------------------------------------------------------------")
              solutions
            }
            case head :: tail => {

              val possibles = head.possibleValues()
              // Console.println( level + " head: " + grid.coords( head ) + " " + possibles )

              val moreSols = for( p <- possibles; if (canSet(grid, head, p)) ) yield {

                val copy = grid.clone()
                val coords = grid.coords(head)
                val toSet = copy.findCell(coords._1, coords._2)
                // Console.println( level + " setting: " + coords._1 +","+ coords._2 +" to " + p )
                toSet.set(p)
                val sr = new SimpleReducer()
                var removed = sr.eliminate(copy)
                while( removed > 0 ) {
                  removed = sr.eliminate(copy)
                }
                // Printer.print(copy)
                //Printer.print(grid)
                // Printer.possibles(copy)
                val tmpSols = bruteForce(copy, solutions, (level + 1))
                if( !tmpSols.isEmpty && findFirst ){
                  return tmpSols
                }

                tmpSols

              }

              solutions ++ moreSols.flatten
            }
          }
        }
      }
    }

  }

  def canSet( grid : Grid, cell : Cell, value : Int ) : Boolean = {

    if( hasValue( grid.findRow(cell), value ) ){
      false
    }
    else {
      if( hasValue( grid.findCol(cell), value ) ){
        false
      }
      else {
        if( hasValue( grid.findSub(cell), value ) ) {
          false
        }
        else {
          true
        }
      }
    }

  }

  def hasValue( cells : List[Cell], value : Int ) : Boolean = {
    val solved = cells.filter( (c) => { c.solved() } ).map( (c) => { c.value() } )
    solved.contains(value)
  }

}
