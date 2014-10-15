package com.github.scaduku.heuristic

import com.github.scaduku.{Group, Cell, Grid}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class NakedPairs extends Heuristic {
  override def eliminate(grid : Grid) : Int = { reduce( grid ) }

  def reduce( grid : Grid ) : Int = {

    for( r <- grid.rows ){
      reduce( grid, r ) match {
        case 0 => {}
        case i : Int => { return i }
      }
    }

    for( r <- grid.cols ){
      reduce( grid, r ) match {
        case 0 => {}
        case i : Int => { return i }
      }
    }

    for( r <- grid.subs ){
      reduce( grid, r ) match {
        case 0 => {}
        case i : Int => { return i }
      }
    }

    0
  }

  def reduce( grid : Grid, group : Group ) : Int = {
    val nps = findNakedPairs(group)
    for( np <- nps ){

      val set1 = Set( grid.findRow(np._2(0)), grid.findCol(np._2(0)), grid.findSub(np._2(0)))
      val set2 = Set( grid.findRow(np._2(1)), grid.findCol(np._2(1)), grid.findSub(np._2(1)))

      val inter = set1.intersect(set2).toList

      eliminatePossibles( np, inter ) match {
        case 0 => {}
        case i : Int => { return i }
      }

    }

    0
  }

  def eliminatePossibles( pairs : ((Int, Int), List[Cell]), subs : List[Group] ) : Int = {

    var count = 0

    val nums = pairs._1

    // we want to eliminate the numbers in the pair
    // from the possibles in the subs
    for( sub <- subs ){

      val cells = Group.toList( sub ).filter( (c) => { !pairs._2.contains(c) } )

      // if either of the numbers are possibles eliminate them
      for( cell <- cells ){

        count += eliminatePossible( nums._1, cell )
        count += eliminatePossible( nums._2, cell )

      }
    }

    count
  }

  def eliminatePossible( p : Int, cell : Cell ) : Int = {
    if( cell.isPossible( p ) ){
      cell.eliminate( p )
      1
    }
    else {
      0
    }

  }

  def findNakedPairs( group : Group ) : Map[(Int,Int),List[Cell]] = {

    val pairCells = Group.toList(group).filter( (c) => { c.possibles.length == 2 } )

    val pairMap = mutable.HashMap[ (Int,Int), List[Cell]]()

    for( cell <- pairCells ) {

      val pair = ( cell.possibles(0), cell.possibles(1) )
      pairMap.get( pair ) match {
        case Some(list) => {
          pairMap.put( (pair), ( list ++ List(cell) ) )
        }
        case None => { pairMap.put( (pair), List(cell) ) }
      }

    }

    val nakedPairs = pairMap.filter( (t) => { t._2.length == 2 } )

    nakedPairs.toMap

  }
}
