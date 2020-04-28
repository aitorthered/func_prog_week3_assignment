package objsets

import org.junit._
import org.junit.Assert.assertEquals

class TweetSetSuite {

  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  @Test def `filter: on empty set`: Unit =
    new TestSets {
      assertEquals(0, size(set1.filter(tw => tw.user == "a")))
    }

  @Test def `filter: a on set5`: Unit =
    new TestSets {
      assertEquals(1, size(set5.filter(tw => tw.user == "a")))
    }

  @Test def `filter: twenty on set5`: Unit =
    new TestSets {
      assertEquals(2, size(set5.filter(tw => tw.retweets == 20)))
    }

  @Test def `union: set4c and set4d`: Unit =
    new TestSets {
      assertEquals(4, size(set4c.union(set4d)))
    }

  @Test def `union: with empty set1`: Unit =
    new TestSets {
      assertEquals(4, size(set5.union(set1)))
    }

  @Test def `union: with empty set1 an set2`: Unit =
    new TestSets {
      assertEquals(0, size(set1.union(set1)))
    }

  @Test def `union: with empty set2`: Unit =
    new TestSets {
      assertEquals(4, size(set1.union(set5)))
    }

  @Test def `union: with set5 and set5`: Unit =
    new TestSets {
      assertEquals(4, size(set5.union(set5)))
    }


  @Test def `descending: set5`: Unit =
    new TestSets {
      val trends = set5.descendingByRetweet
      Assert.assertEquals(true, !trends.isEmpty)
      Assert.assertEquals(true, trends.head.user == "a" || trends.head.user == "b")
    }

  @Test def `mostRetweeted: set1 empty`: Unit =
    new TestSets {
      try {
        val trends = set1.mostRetweeted
        Assert.fail("Should not reach this")
      }
      catch {
        case _: java.util.NoSuchElementException => () // Expected, so continue
      }
    }

  @Test def `mostRetweeted: set5`: Unit =
    new TestSets {
      val trends = set5.mostRetweeted
      Assert.assertEquals(20, trends.retweets)
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
