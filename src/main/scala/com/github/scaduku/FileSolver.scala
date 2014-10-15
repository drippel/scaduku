package com.github.scaduku

import java.io.{FileInputStream, BufferedReader, FileReader, File}

import scala.io.BufferedSource

object FileSolver {

  def main (args : Array[String]) {

    val bs = new BufferedSource( new FileInputStream( new File( "c:/dev/projects/opensource/scaduku/git/scaduku/src/test/norvig.txt")) )

    for( line <- bs.getLines() ){
      Scaduku.solve( line )
    }


  }

}
