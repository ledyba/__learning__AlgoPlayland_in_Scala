package org.ledyba.algo

import scala.util.Random
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import scala.util.matching.Regex
import scala.util.matching.Regex
import org.junit.Assert
import Function.tupled;

object Util {
	def shuffle(array:Array[Int]):Array[Int] = {
		val r = new Random();
		def swap( i:Int, j:Int ) = {
			val t = array(i);
			array(i) = array(j);
			array(j) = t;
		}
		for(i <- Range(0, array.length-1)){
			swap(i, r.nextInt(array.length-i)+i);
		}
		array
	}
	def sorted(array:Array[Int]):Boolean = {
		array.zip(array.tail).foldLeft(true)((b,i) => b && i._1 <= i._2)
	}
}
