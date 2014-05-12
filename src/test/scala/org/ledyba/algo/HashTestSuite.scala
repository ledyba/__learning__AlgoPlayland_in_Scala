package org.ledyba.algo

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import Util.shuffle

class HashTestSuite extends AssertionsForJUnit {
	@Before def initialize() {
	}

	@Test def verifyEasy() { // Uses JUnit-style assertions
		val hash = new HashMap();
		for( i <- shuffle(Range(0, 1000).toArray) ) {
			assertTrue( hash.add("%d".format(i), "item%d".format(i)) )
		}
		for( i <- shuffle(Range(0, 1000).toArray) ) {
			assertEquals(Some("item%d".format(i)), hash.search("%d".format(i)));
		}
		for( i <- shuffle(Range(0, 1000).toArray) ) {
			assertTrue( hash.remove("%d".format(i)) )
		}
		for( i <- shuffle(Range(0, 1000).toArray) ) {
			assertEquals(None, hash.search("%d".format(i)));
		}
	}
}
