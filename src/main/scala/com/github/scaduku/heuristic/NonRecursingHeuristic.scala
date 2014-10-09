package com.github.scaduku.heuristic

import com.github.scaduku.{Cell, Grid}

abstract class NonRecursingHeuristic extends Heuristic {

  def eliminate(grid : Grid) : Int = {
    reduceGrid(grid)
  }

  def reduceGrid(grid : Grid) : Int = {

    reduceGroups(grid, grid.rows) match {
      case 0 => {}
      case i : Int => {
        return i
      }
    }

    reduceGroups(grid, grid.cols) match {
      case 0 => {}
      case i : Int => {
        return i
      }
    }

    reduceGroups(grid, grid.subs) match {
      case 0 => {}
      case i : Int => {
        return i
      }
    }

    0
  }

  def reduceGroups(grid : Grid, groups : List[List[Cell]]) : Int = {
    for( group <- groups ) {
      reduce(grid, group) match {
        case 0 => {}
        case i : Int => {
          return i
        }
      }
    }

    0
  }

  def reduce(grid : Grid, cells : List[Cell]) : Int

}
