package com.github.scaduku

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Grid {

  val cells = Array.fill(9,9){ new Cell }

  var cols : List[Group] = initCols()
  var rows : List[Group] = initRows()
  var subs : List[Group] = initSubs()

  def initRows() = {
    val rs = new ListBuffer[Group]()
    for( x <- 0 until 9 ){
      var g = new Group()
      for( y <- 0 until 9 ){
        g.cells(y) = cells(x)(y)
      }
      rs += g
    }
    rs.toList
  }

  def initCols() = {
    val cs = new ListBuffer[Group]()
    for( x <- 0 until 9 ){
      var g = new Group()
      for( y <- 0 until 9 ){
        g.cells(y) = cells(y)(x)
      }
      cs += g
    }
    cs.toList
  }

  def initSubs() = {
    val cs = new ListBuffer[Group]()
    cs += Group.fromList( List(cells(0)(0),cells(1)(0),cells(2)(0), cells(0)(1),cells(1)(1),cells(2)(1), cells(0)(2),cells(1)(2),cells(2)(2) ) )
    cs += Group.fromList( List(cells(3)(0),cells(4)(0),cells(5)(0), cells(3)(1),cells(4)(1),cells(5)(1), cells(3)(2),cells(4)(2),cells(5)(2) ) )
    cs += Group.fromList( List(cells(6)(0),cells(7)(0),cells(8)(0), cells(6)(1),cells(7)(1),cells(8)(1), cells(6)(2),cells(7)(2),cells(8)(2) ) )


    cs += Group.fromList( List(cells(0)(3),cells(1)(3),cells(2)(3), cells(0)(4),cells(1)(4),cells(2)(4), cells(0)(5),cells(1)(5),cells(2)(5) ) )
    cs += Group.fromList( List(cells(3)(3),cells(4)(3),cells(5)(3), cells(3)(4),cells(4)(4),cells(5)(4), cells(3)(5),cells(4)(5),cells(5)(5) ) )
    cs += Group.fromList( List(cells(6)(3),cells(7)(3),cells(8)(3), cells(6)(4),cells(7)(4),cells(8)(4), cells(6)(5),cells(7)(5),cells(8)(5) ) )

    cs += Group.fromList( List(cells(0)(6),cells(1)(6),cells(2)(6), cells(0)(7),cells(1)(7),cells(2)(7), cells(0)(8),cells(1)(8),cells(2)(8) ) )
    cs += Group.fromList( List(cells(3)(6),cells(4)(6),cells(5)(6), cells(3)(7),cells(4)(7),cells(5)(7), cells(3)(8),cells(4)(8),cells(5)(8) ) )
    cs += Group.fromList( List(cells(6)(6),cells(7)(6),cells(8)(6), cells(6)(7),cells(7)(7),cells(8)(7), cells(6)(8),cells(7)(8),cells(8)(8) ) )

    cs.toList
  }

  def placeHints( hints : String ) = {
    if( hints.length == 81 ){
      val lines = hints.toList.grouped(9)
      var x = 0
      for( line <- lines ){
        for( y <- 0 until line.length ){
          if( line(y).isDigit ) {
            line(y).toString.toInt match {
              case 0 => {}
              case i : Int => {
                cells(x)(y).set(i)
                cells(x)(y).hint = true
              }
            }
          }
        }
        x += 1
      }
    }
  }

  def isSolved() : Boolean = {
    val all = cells.flatten
    val found = all.find( (c) => { !c.solved() })
    found.isEmpty
  }

  def findInGroups( cell : Cell, groups : List[Group] ) : Group = {
    groups.find( (g) => { g.cells.contains(cell) }).head
  }

  def findRow( cell : Cell ) : Group = {
    findInGroups( cell, rows )
  }

  def findCol( cell : Cell ) : Group = {
    findInGroups( cell, cols )
  }

  def findSub( cell : Cell ) : Group = {
    findInGroups( cell, subs )
  }

  def coords( cell : Cell ) : (Int,Int) = {
    val row = findRow(cell)
    val col = findCol(cell)
    // ( (rows.indexOf(row) ), (cols.indexOf(col) ))
    ( (cols.indexOf(col) ), (rows.indexOf(row) ))
  }

  def findCell( x : Int, y : Int ) : Cell = {
    cells(y)(x)
  }


  override def clone() : Grid = {

    val copy = new Grid()
    for( i <- 0 until 9 ){
      for( j <- 0 until 9 ){
        copy.cells(i)(j) = cells(i)(j).clone()
      }
    }

    copy.rows = copy.initRows
    copy.cols = copy.initCols
    copy.subs = copy.initSubs

    copy

  }

  def unsolved() : List[Cell] = {

    val unsolvedCells = for( i <- 0 until 9; j <- 0 until 9; if( !cells(i)(j).solved() ) ) yield cells(i)(j)
    unsolvedCells.toList
  }

  def isValid() : Boolean = {

    val groups = rows ++ cols ++ subs

    val exists = for( group <- groups ) yield {

      val counts = buildCountMap( group.solved  )
      counts.exists( ( (t) => { t._2 > 1 }) )

    }

    exists.forall( (b) => { b == false })

  }

  def buildCountMap( cells : List[Cell] ) : Map[Int,Int] = {
    val counts = mutable.HashMap[Int,Int]()
    for( c <- cells ){
      val ps = c.possibles
      for( p <- ps ) {
        counts.get(p) match {
          case Some(i) => { counts.put( p, (i+1))}
          case None => { counts.put( p, (1))}
        }
      }
    }

    counts.toMap
  }
}
