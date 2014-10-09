package com.github.scaduku

class Printer {

}

object Printer {

  def print( grid : Grid ) = {

    Console.print("-------------\n")
    for( i <- 0 until 9 ){
      Console.print("|")
      for( j <- 0 until 9 ) {
        Console.print( grid.cells(i)(j) )
        if( (j + 1) % 3 == 0 ){
          Console.print("|")
        }
      }
      Console.print("\n")
      if( (i + 1) % 3 == 0 ){
        Console.print("-------------\n")
      }
    }
  }

  def possibles( grid : Grid ) = {

    for( i <- 0 until 9 ){
      for( j <- 0 until 9 ) {
        Console.print( "[("+ (j+1) +","+ (i+1)+") ")
        val cell = grid.cells(i)(j)
        for( i <- 0 until cell.possibles.length ){
          if( cell.possibles(i) == 1 ){
            Console.print( (i+1))
          }
        }
        Console.print("\n")
      }
    }
  }

  def possibles( cells : List[Cell] ) = {

        for( cell <- cells ){
          Console.print( "[")
          for( i <- 0 until cell.possibles.length )
          if( cell.possibles(i) == 1 ){
            Console.print( (i+1))
          }
          Console.print( "]\n")
        }
  }
}
