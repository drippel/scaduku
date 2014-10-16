package com.github.scaduku

import scala.collection.mutable.ListBuffer

class Group {

  val cells = ListBuffer[Cell]()

  def solved() : List[Cell] = {
    cells.filter( (c) => { c.solved() } ).toList
  }

  def unsolved() : List[Cell] = {
    cells.filter( (c) => { !c.solved() } ).toList
  }

  def hasPossible( possible : Int ) : Set[Cell] = {
    cells.filter((c) => { c.isPossible( possible ) }).toSet
  }

  def toSet() : Set[Cell] = { cells.toSet  }

  def hasValue( value : Int ) : Boolean = {
    val values = solved().map( (c) => { c.value() } )
    values.contains(value)
  }
}

object Group {

  def fromList( list : List[Cell] ) : Group = {
    val g = new Group()
    g.cells ++= list
    g
  }

  def toList( group : Group ) : List[Cell] = { group.cells.toList }
}
