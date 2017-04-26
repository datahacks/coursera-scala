package patmat

import patmat.Huffman._

object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val l1 = List('a')                              //> l1  : List[Char] = List(a)
  
  times(List('a', 'b', 'a'))                      //> res0: List[(Char, Int)] = List((a,2), (b,1))
  
  decodedSecret                                   //> res1: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
}