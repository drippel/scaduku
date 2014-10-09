package com.github.scaduku.heuristic

import com.github.scaduku.{Cell, Grid}

abstract class Heuristic {
  def eliminate( grid : Grid ) : Int

  def unsolved( cells : List[Cell] ) : List[Cell] = {
    cells.filter( (c) => { !c.solved() } )
  }

  def solved( cells : List[Cell] ) : List[Cell] = {
    cells.filter( (c) => { c.solved() } )
  }
}
