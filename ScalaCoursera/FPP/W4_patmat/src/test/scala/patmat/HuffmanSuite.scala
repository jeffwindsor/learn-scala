package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(
      Fork(
        Leaf('a',2),
        Leaf('b',3),
        List('a','b'), 5),
      Leaf('d',4),
      List('a','b','d'), 9)
    var te = Fork(
      Leaf('a',8),
      Fork(
        Fork(
          Leaf('b',3),
          Fork(Leaf('c',1), Leaf('d',1), List('c','d'),2),
          List('b','c','d'),5),
        Fork(
          Fork(Leaf('e',1),Leaf('f',1), List('e','f'),2),
          Fork(Leaf('g',1),Leaf('h',1), List('g','h'),2),
          List('e','f','g','h'),4),
        List('b','c','d','e','f','g','h'),9),
      List('a', 'b','c','d','e','f','g','h'),17)
	}

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  /*
  test("createCodeTree(someText)") {
    assert(createCodeTree(string2Chars("someText")) === new Leaf('a',1))
  }
    //[Observed Error] 16692 did not equal 1919
  //Fork(Fork(Fork(Fork(Fork(Fork(Leaf(s,1),Leaf(x,1),List(s, x),2),Leaf(T,1),List(s, x, T),3),Leaf(t,1),List(s, x, T, t),4),Leaf(m,1),List(s, x, T, t, m),5),Leaf(o,1),List(s, x, T, t, m, o),6),Leaf(e,2),List(s, x, T, t, m, o, e),8)
*/

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode secret") {
    new TestTrees {
      assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l') )
    }
  }



  test("convert t1") {
    new TestTrees {
      assert(convert(t1) === List(('a',List(0)),('b', List(1))))
    }
  }

  test("convert t2") {
    new TestTrees {
      assert(convert(t2) === List(('a',List(0,0)),('b', List(0,1)), ('d', List(1))))
    }
  }

  test("convert te") {
    new TestTrees {
      assert(convert(te) === List(
        ('a', List(0)),
        ('b', List(1,0,0)),
        ('c', List(1,0,1,0)),
        ('d', List(1,0,1,1)),
        ('e', List(1,1,0,0)),
        ('f', List(1,1,0,1)),
        ('g', List(1,1,1,0)),
        ('h', List(1,1,1,1))
      ))
    }
  }

  test("encode a t2") { new TestTrees { assert(encode(t2)(List('a')) === List(0,0)) } }
  test("encode b t2") { new TestTrees { assert(encode(t2)(List('b')) === List(0,1)) } }
  test("encode d t2") { new TestTrees { assert(encode(t2)(List('d')) === List(1)) } }
  test("encode b te") { new TestTrees { assert(encode(te)(List('b')) === List(1,0,0))}}
  test("encode example te") {new TestTrees { assert(encode(te)(List('b', 'a', 'c')) === List(1,0,0,0,1,0,1,0)) }}
  test("decode example te") {new TestTrees { assert(decode(te,List(1,0,0,0,1,0,1,0)) === List('b', 'a', 'c') )}}
  test("decode and encode te") {
    new TestTrees {
      val l = List('a', 'b', 'c','e','f','d','g','h')
      assert(decode(te, encode(te)(l)) === l)
    }
  }

  test("quickEncode a t2") { new TestTrees { assert(quickEncode(t2)(List('a')) === List(0,0)) } }
  test("quickEncode b t2") { new TestTrees { assert(quickEncode(t2)(List('b')) === List(0,1)) } }
  test("quickEncode d t2") { new TestTrees { assert(quickEncode(t2)(List('d')) === List(1)) } }
  test("quickEncode b te") { new TestTrees { assert(quickEncode(te)(List('b')) === List(1,0,0))}}
  test("quickEncode ba t2") {new TestTrees { assert(quickEncode(t2)(List('b', 'a')) === List(0,1,0,0)) }}

  test("quickEncode example te") {new TestTrees { assert(quickEncode(te)(List('b', 'a', 'c')) === List(1,0,0,0,1,0,1,0)) }}

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      val l = List('a', 'b', 'c')
      assert(decode(te, quickEncode(te)(l)) === l)
    }
  }
}
