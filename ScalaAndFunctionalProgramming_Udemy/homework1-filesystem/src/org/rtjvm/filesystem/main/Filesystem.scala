package org.rtjvm.filesystem.main

import java.util.Scanner

import org.rtjvm.filesystem.commands.Command
import org.rtjvm.filesystem.files.Directory

/**
  * Created by daniel.
  *
  * Make sure to read this homework's instructions and explanations for more
  * information on what everything in here does and means.
  */
object Filesystem extends App {
  // small prep
  val firstRoot = Directory.newRoot
  var state = State(firstRoot,firstRoot)
  val scanner = new Scanner(System.in)

  /*
    The main loop of the application:
      at every point, show the command line, then fetch my command
      then change the state to reflect the effect of my command

      After watching the Functional Programming chapter in the course,
      try to redesign this part in a functional, more elegant way.
   */
  while(true) {
    state.show
    state = Command.from(scanner.nextLine()).apply(state)
  }
}
