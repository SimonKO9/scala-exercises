package com.github.simonthecat.markovchain

import org.specs2.mutable._

class MarkovChainSpec extends Specification {
  "MarkovChain" should {
    "have correctly generated chain map of strings for windowSize=1" in {
      val textSeq = "Simon the cat".split(' ').toSeq

      val markov1 = MarkovChain(textSeq, 1)
      markov1.chains mustEqual Map(Seq("Simon") -> Seq("the"), Seq("the") -> Seq("cat"))
    }

    "have correctly generated chain map of strings for windowSize=2" in {
      val textSeq = "Simon the cat".split(' ').toSeq

      val markov2 = MarkovChain(textSeq, 2)
      markov2.chains mustEqual Map(Seq("Simon", "the") -> Seq("cat"))
    }

    "have correctly generated chain map of strings for repeated keys" in {
      val textSeq = "a b a b a".split(' ').toSeq

      val markov2 = MarkovChain(textSeq, 2)
      markov2.chains mustEqual Map(
        Seq("a", "b") -> Seq("a", "a"),
        Seq("b", "a") -> Seq("b")
      )
    }

    "choose successor for simple text" in {
      val textSeq = "Simon the cat".split(' ').toSeq

      val markov2 = MarkovChain(textSeq, 2)
      val successor = markov2.next(Seq("Simon", "the"))
      successor mustEqual Some("cat")
    }

    "return None as successor for simple text and invalid input" in {
      val textSeq = "Simon the cat".split(' ').toSeq

      val markov2 = MarkovChain(textSeq, 2)
      markov2.next(Seq("the", "cat")) mustEqual None
      markov2.next(Seq("the")) mustEqual None
      markov2.next(Seq("cat")) mustEqual None
      markov2.next(Seq()) mustEqual None
    }
  }
}
