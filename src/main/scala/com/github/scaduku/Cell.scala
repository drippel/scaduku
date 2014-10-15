package com.github.scaduku

import scala.collection.mutable.ListBuffer

class Cell {

  var possibles = ListBuffer(1,2,3,4,5,6,7,8,9)
  var hint = false

  def set(i : Int) = { possibles = ListBuffer(i) }

  def solved() : Boolean = { possibles.length == 1 }

  def value() : Int = {
    if( solved() ){
      possibles.head
    }
    else {
      -1
    }
  }

  def eliminate(i : Int) = { possibles -= i }

  def isPossible( p : Int ) : Boolean = { possibles.contains(p) }

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
    copy.possibles = possibles.clone()
    copy
  }
}
