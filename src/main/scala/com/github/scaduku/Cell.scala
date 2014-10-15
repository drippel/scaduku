package com.github.scaduku

class Cell {

  val possibles = Array.fill(9){1}
  var hint = false

  def set(i : Int) = {
    clear()
    possibles(i-1) = 1
  }

  def clear() = {
    for( i <- 0 until possibles.length ){
      possibles(i) = 0
    }
  }

  def solved() : Boolean = {
    val ps = possibles.filter( (p) => { p == 1 } )
    ps.size == 1
  }

  def value() : Int = {
    if( solved() ){
      possibles.indexOf(1) + 1
    }
    else {
      -1
    }
  }

  def possibleValues() : List[Int] = {

    val ps = for( i <- 0 until possibles.length
      if( possibles(i) == 1 ) )
      yield { ( i + 1 ) }

    ps.toList
  }

  def eliminate(i : Int) = {
    possibles(i-1) = 0
  }

  def isPossible( p : Int ) : Boolean = {
    possibles(p-1) == 1
  }

  override def toString() = {
    if( solved() ){
      value().toString
    }
    else {
      " "
      // "[" + possibleValues().mkString(",") + "]"
    }
  }


  override def clone() : Cell = {
    val copy = new Cell()

    copy.hint = hint

    for( i <- 0 until 9){
      copy.possibles(i) = possibles(i)
    }

    copy
  }
}
