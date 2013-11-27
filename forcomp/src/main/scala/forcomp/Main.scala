package forcomp
import Anagrams._

object Main extends App {
	val w = "Banana"
	println(wordOccurrences(w))
	val s = "apple" :: "banana" :: "orange" :: "kiwi" :: "pomegranate" :: Nil;
	println(sentenceOccurrences(s))
	
	println
	val res0 =combinations(List(('a', 2))); 
	res0.foreach(println)
	
	println
	val res = combinations(List(('a', 2), ('b', 2)))
	res.foreach(println)

	println
	val res2 = combinations(List(('a', 2), ('b', 2), ('c', 3)))
	res2.foreach(println)

	println
	subtract(List(('a', 3), ('b', 2), ('c', 5)), List(('a', 1), ('b',2)))
	
	println
	sentenceAnagrams(List("Linux", "rulez")).foreach(println);
}