package org.rtjvm.filesystem.commands

import org.rtjvm.filesystem.main.State

/**
  * Created by daniel.
  *
  * Make sure to read this homework's instructions and explanations for more
  * information on what everything in here does and means.
  */
trait Command {
  /**
    * The primary function of every Command - to transform a State into another State.
    */
  def apply(state: State): State
}

object Command {
  /**
    * A default implementation of a Command which rejects everything.
    */
  def unknownCommand(name: String) =
    new Command {
      // anonymous class - see the object-oriented skill vault for how this is achieved
      override def apply(state: State): State =
        state.withMessage(name + ": command not found")
    }

  /**
    * A function which will parse a String and turn it into one of your Command implementations.
    */
  def from(input: String): Command = {
    val tokens = input.trim.split(" ")

    // for now it returns our default implementation
    // modify code here
    unknownCommand(tokens(0))
  }
}