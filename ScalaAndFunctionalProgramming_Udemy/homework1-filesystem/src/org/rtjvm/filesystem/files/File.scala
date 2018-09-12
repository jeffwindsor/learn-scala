package org.rtjvm.filesystem.files

/**
  * Created by daniel.
  *
  * Make sure to read this homework's instructions and explanations for more
  * information on what everything in here does and means.
  */
class File(
      override val parentPath: String,
      override val name: String,
      val contents: String)
extends DirEntry(parentPath, name)