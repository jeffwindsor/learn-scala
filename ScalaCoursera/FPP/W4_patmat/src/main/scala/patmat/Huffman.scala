package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {
  val left_code = 0
  val right_code = 1

  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = tree match {
    case f: Fork => f.weight
    case l: Leaf => l.weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case f: Fork => f.chars
    case l: Leaf => List(l.char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    new Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees
  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] =
    chars.groupBy(identity).mapValues(_.size).toList

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    freqs.sortBy({ case (c, i) => i }).map(c => Leaf.tupled(c))

  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case x :: Nil => true
    case _ => false
  }

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case a :: b :: tail => (makeCodeTree(a, b) :: tail).sortWith(weight(_) < weight(_))
    case _ => trees
  }

  def until(p: List[CodeTree] => Boolean, combine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): CodeTree =
    p(trees) match {
      case true => trees.head
      case false => until(p, combine)(combine(trees))
    }

  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars)))

  // Part 3: Decoding
  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decodeAcc(subtree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = subtree match {
      case l: Leaf => decodeAcc(tree, bits, l.char :: acc)
      case f: Fork => bits match {
        case Nil => acc
        case head :: tail => decodeAcc(if (head == left_code) f.left else f.right, tail, acc)
      }
    }
    decodeAcc(tree, bits, Nil).reverse
  }

  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeChar(subtree: CodeTree, char: Char, acc: List[Bit]): List[Bit] = subtree match {
      case l: Leaf => acc
      case f: Fork =>
        if (chars(f.left).contains(char))
          encodeChar(f.left, char, left_code :: acc)
        else
          encodeChar(f.right, char, right_code :: acc)
    }
    def encodeAcc(text: List[Char], acc: List[Bit]): List[Bit] = text match {
      case Nil => acc
      case char :: tail => encodeAcc(tail, encodeChar(tree, char, acc))
    }
    encodeAcc(text, Nil).reverse
  }

  // Part 4b: Encoding using code table
  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.filter(x => x._1 == char) match {
    case Nil => Nil
    case x :: _ => x._2
  }

  def convert(tree: CodeTree): CodeTable = {
    def convertWithLineage(subtree: CodeTree, lineage: List[Bit]): CodeTable = subtree match {
      case l: Leaf => List((l.char, lineage.reverse))
      case f: Fork => mergeCodeTables(convertWithLineage(f.left, left_code :: lineage), convertWithLineage(f.right, right_code :: lineage))
    }
    return tree match {
      case l: Leaf => List((l.char, Nil))
      case f: Fork => mergeCodeTables(convertWithLineage(f.left, List(left_code)), convertWithLineage(f.right, List(right_code)))
    }
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val table = convert(tree)
    text.foldLeft(List[Bit]())({ case (acc, c) => acc ::: (codeBits(table)(c)) })
  }

}
