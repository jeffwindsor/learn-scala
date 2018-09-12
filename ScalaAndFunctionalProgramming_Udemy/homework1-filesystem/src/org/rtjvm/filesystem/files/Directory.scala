package org.rtjvm.filesystem.files


/**
  * Created by daniel.
  *
  * Make sure to read this homework's instructions and explanations for more
  * information on what everything in here does and means.
  */
class Directory(
                 override val parentPath: String,
                 override val name: String,
                 val contents: List[DirEntry])
  extends DirEntry(parentPath, name)

/**
  * Companion object for the Directory class. Contains utility methods.
  */
object Directory {
  /**
    * Creates a root directory. Do not call this twice... unless you want two different root folders
    * in your filesystem.
    *
    * What an idea for improving the assignment!
    * Create your own alien file system with multiple root folders, analogous to hard drive partitions!
    */
  def newRoot: Directory =
    Directory.empty("", "")

  /**
    * A utility function which creates and empty Directory at a given parent path.
    */
  def empty(parentPath: String, name: String): Directory =
    new Directory(parentPath, name, List())
}
