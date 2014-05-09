package org.ledyba.algo

import scala.util.Random

class Heap(val size:Int) {
	val heap = Range(0, size).toArray;
	def swap(i:Int,j:Int):Unit = {
		val t = heap(i);
		heap(i) = heap(j);
		heap(j) = t;
	}
	def randomize():Unit = {
		val r = new Random();
		for(i <- Range(0, size-1)){
			swap(i, r.nextInt(size-i)+i);
		}
	}
	randomize()
	def leftIdx(j:Int)  = (((j+1) << 1) | 0)-1;
	def rightIdx(j:Int) = (((j+1) << 1) | 1)-1;
	def isMaxHeap:Boolean = {
		def isMaxHeapImpl(i:Int):Boolean = {
			val l = leftIdx(i);
			val r = rightIdx(i);
			(size <= l || (heap(i) > heap(l) && isMaxHeapImpl(l) )) &&
			(size <= r || (heap(i) > heap(r) && isMaxHeapImpl(r) ))
		}
		isMaxHeapImpl(0);
	}
	def maxHeapify(i:Int):Unit = {
		val l = leftIdx(i);
		val r = rightIdx(i);
		if( l < size && heap(l) > heap(i) && (r >= size || heap(l) > heap(r)) ) {
			swap(l, i);
			maxHeapify(l);
		}else if( r < size && heap(r) > heap(i) && (l >= size || heap(r) > heap(l)) ) {
			swap(r, i);
			maxHeapify(r);
		}
	}
	def build():Unit={
		for( i <- (0 to size/2).reverse ) {
			maxHeapify(i)
		}
	}
}