package org.ledyba.algo

import scala.util.Random

class Heap(val heap:Array[Int]) {
	def this(size_ : Int) = { this(Range(0,size_).toArray); randomize(); }
	var size = heap.size;
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
	def leftIdx(j:Int)  = (((j+1) << 1) | 0)-1;
	def rightIdx(j:Int) = (((j+1) << 1) | 1)-1;
	def parentIdx(j:Int) = ((j+1)/2) - 1;
	def isMaxHeap:Boolean = {
		def isMaxHeapImpl(i:Int):Boolean = {
			val l = leftIdx(i);
			val r = rightIdx(i);
			(size <= l || (heap(i) >= heap(l) && isMaxHeapImpl(l) )) &&
			(size <= r || (heap(i) >= heap(r) && isMaxHeapImpl(r) ))
		}
		isMaxHeapImpl(0);
	}
	def maxHeapify(i:Int):Unit = {
		val l = leftIdx(i);
		val r = rightIdx(i);
		if( l < size && heap(l) > heap(i) && (r >= size || heap(l) >= heap(r)) ) {
			swap(l, i);
			maxHeapify(l);
		}else if( r < size && heap(r) > heap(i) && (l >= size || heap(r) >= heap(l)) ) {
			swap(r, i);
			maxHeapify(r);
		}
	}
	def build():Unit={
		for( i <- (0 to size/2).reverse ) {
			maxHeapify(i)
		}
	}
	def pop() = {
		val it = heap.head;
		heap(0) = heap(size-1)
		size = size - 1;
		maxHeapify(0)
		it
	}
	def max() = heap.head;
	def insert(v:Int) = {
		if( size >= heap.size ) {
			throw new Exception();
		}
		heap(size) = v;
		var j = size;
		size = size + 1;
		while( j > 0 && heap(j) > heap(parentIdx(j)) ){
			swap(j, parentIdx(j));
			j = parentIdx(j);
		}
	}
	def increase(i:Int, v:Int) = {
		heap(i) = v;
		var j = i;
		while( j > 0 && heap(j) > heap(parentIdx(j)) ){
			swap(j, parentIdx(j));
			j = parentIdx(j);
		}
	}
}