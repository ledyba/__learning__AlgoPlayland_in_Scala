package org.ledyba.algo

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import Util.shuffle

class BinaryTreeSuite extends AssertionsForJUnit {
	@Before def initialize() {
	}

	@Test def verifyEasy() { // Uses JUnit-style assertions
		val vt = new BinaryTree();
		for( i <- shuffle(Range(0, 1000).toArray) ) {
			vt.add(i);
			if( !vt.isValid() ) {
				fail("not valid");
			}
		}
		assertArrayEquals(Range(0,1000).toArray, vt.toList.toArray)
		for( i <- shuffle(Range(0, 1000).toArray) ) {
			vt.remove(i);
			if( !vt.isValid() ) {
				fail("not valid");
			}
		}
		val end = vt.toList
		assertTrue(end.isEmpty)
	}
}
