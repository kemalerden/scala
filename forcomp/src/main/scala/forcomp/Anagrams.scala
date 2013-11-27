package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    //    def charEquals(x : Char, y: Char) : Boolean = {
    //      x.toLower == y.toLower
    //    }
    //    if (w.isEmpty()) Nil
    //    else if (w.head.isSpaceChar) wordOccurrences(w.tail)
    //    else (w.head.toLower, w.filter(charEquals(_, w.head)).size) :: wordOccurrences(w.filterNot(charEquals(_, w.head)))
    w.groupBy((c: Char) => c.toLower).map((t: (Char, String)) => (t._1, t._2.size)).toList.sortWith(_._1 < _._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.foldLeft("")(_ + _));

  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(y => wordOccurrences(y))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def getSubsets(occurrence: (Char, Int)): List[Occurrences] = {
      occurrence match {
        case (char, size) => {
          val x = for (i: Int <- 1 until size + 1) yield {
            List((char, i))
          }
          List() :: x.toList
        }
      }
    }
    def cartesian(subsets: List[Occurrences], list: List[Occurrences]): List[Occurrences] = {
      if (list.isEmpty)
        subsets;
      else {
        val y = for (subset <- subsets; occurrence <- list) yield {
          subset match {
            case List() => occurrence
            case List((x, y)) => (x, y) :: occurrence
          }
        }
        y.toList
      }
    }
    if (occurrences.isEmpty)
      List(Nil)
    else
      cartesian(getSubsets(occurrences.head), combinations(occurrences.tail));
  }

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xm: Map[Char, Int] = x.toMap
    val ym: Map[Char, Int] = y.toMap
    val t: Map[Char, Int] = ym.foldLeft(xm)((r, c) => if (r(c._1) == c._2) r - c._1 else r.updated(c._1, r(c._1) - c._2));
    t.toList.sortWith(_._1 < _._1)
    //    val k = x.foldLeft(List[(Char,Int)]())(
    //        (r,current)=>
    //          (if (c(current._1)!= null) 
    //            current 
    //          else c(current._1)) :: r)
    //    Nil;
  }

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def dictionaryWords(currentOccurrences: forcomp.Anagrams.Occurrences): Set[Word] = {
      val combinationsList: List[Occurrences] = combinations(currentOccurrences);
      val wordsList: List[List[Word]] = for (c: Occurrences <- combinationsList) yield {
        if (dictionaryByOccurrences.contains(c))
          dictionaryByOccurrences(c)
        else Nil;
      }
      val words = wordsList.flatten.toSet
      words
    }
    def createSentences(sentence: Occurrences, current: Sentence, level: Int, acc: List[Sentence]): List[Sentence] = {
      if (sentence.isEmpty) {
        current :: acc
      } else {
        val words = dictionaryWords(sentence)
        if (words.isEmpty)
          Nil
        else {
          val t = for (word <- words) yield {
            val wordoccurrences = wordOccurrences(word)
            val newSentence = subtract(sentence, wordoccurrences);
            createSentences(newSentence, word :: current, level + 1, acc)
          }
          t.flatten.toList
        }
      }
    }
    createSentences(sentenceOccurrences(sentence), List(), 0, List())
  }

}
