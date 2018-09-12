package org.rtjvm.filesystem.main

import org.rtjvm.filesystem.files.Directory

/**
  * Created by daniel.
  *
  * Make sure to read this homework's instructions and explanations for more
  * information on what everything in here does and means.
  */
class State(val root: Directory, val wd: Directory, val output: String) {
  def showShell: Unit =
    print(State.SHELL_TOKEN)
  def show: Unit = {
    println(output)
    showShell
  }
  // builds a NEW State!
  def withMessage(message: String): State =
    new State(root, wd, message)
}
object State {
  val SHELL_TOKEN = "$ "
  def apply(root: Directory, wd: Directory, output: String = "") =
    new State(root, wd, output)
}