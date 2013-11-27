package patmat

import patmat.Huffman._


object Main extends App {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    println(encode(t1)("abaaab".toList))
    println(decode(t1, List(0, 1, 0, 0, 0, 1)))
    println(decodedSecret)
    println(quickEncode(t1)("abaaab".toList))
    println(quickEncode(frenchCode)(decodedSecret) == secret)
    println(times("asdskfhkdjkasdkaasdasdasaasdasd".toList))
    println(createCodeTree("asdskfhkdjkasdkaasdasdasaasdasd".toList))
}