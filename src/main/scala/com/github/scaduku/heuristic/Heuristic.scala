package com.github.scaduku.heuristic

import com.github.scaduku.{Cell, Grid}

import scala.collection.mutable.HashMap

abstract class Heuristic {
  def eliminate( grid : Grid ) : Int

  def unsolved( cells : List[Cell] ) : List[Cell] = {
    cells.filter( (c) => { !c.solved() } )
  }

  def solved( cells : List[Cell] ) : List[Cell] = {
    cells.filter( (c) => { c.solved() } )
  }

  def buildCountMap( cells : List[Cell] ) : Map[Int,Int] = {
    val counts = HashMap[Int,Int]()
    for( c <- cells ){
      val ps = c.possibleValues()
      for( p <- ps ) {
        counts.get(p) match {
          case Some(i) => { counts.put( p, (i+1))}
          case None => { counts.put( p, (1))}
        }
      }
    }

    counts.toMap
  }

  def hasPossible( cells : List[Cell], possible : Int ) : Set[Cell] = {
    cells.filter((c) => { c.possibleValues().contains(possible) }).toSet
  }
}
