package com.github.scaduku

/**
 * Created by drippel on 10/1/2014.
 */
class Test {

}

object Test {
  def main (args : Array[String]) {

    for( sx <- 0 until 3 ){
      for( sy <- 0 until 3 ){
        for( x <- 0 until 3 ) {
          for( y <- 0 until 3) {
            Console.println( sx +"*"+ x +"," +sy +"*"+ y  )
          }
        }
      }
    }

  }
}
