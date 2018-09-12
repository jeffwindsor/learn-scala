package forcomp


object Anagrams {
  type Word = String
  type Sentence = List[Word]

  /** Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]
  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase().groupBy(identity).mapValues(_.size).toList.sorted

  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.flatten.mkString)

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary
      .map( w => (wordOccurrences(w), w))
      .groupBy(pair => pair._1)
      .map(g => (g._1, g._2.map(p => p._2)))

  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences(wordOccurrences(word))

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def allCounts(char:Char, count:Int) =
      for(i <- 1 to count) yield (char,i)
    def combinationsAcc(o:Occurrences,acc:List[Occurrences]) =
      acc ++ (for(oi <- o; acci <- acc) yield (oi :: acci))

    occurrences
      .map( {case (c,i) => allCounts(c,i).toList} )
      .foldRight(List[Occurrences](List()))(combinationsAcc)
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val (samechar, uniquechar) = x.partition({ case (xc,_) => y.exists( {case (yc, _) => xc == yc }) })
    val samecharsubtracty = for (((ac,ai), (bc, bi)) <- samechar.zip(y) if ai != bi) yield (ac, ai - bi)
    (uniquechar ++ samecharsubtracty).sorted
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def inner(o: Occurrences): List[Sentence] = o match {
      case Nil => List(Nil)
      case _ => for (
        i <- combinations(o) if dictionaryByOccurrences.keySet(i);
        os <- dictionaryByOccurrences(i);
        osub <- inner(subtract(o, i))) yield {
        os :: osub
      }
    }
    inner(sentenceOccurrences(sentence))
  }
}
