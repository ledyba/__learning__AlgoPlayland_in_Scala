package org.ledyba.algo

import scala.util.Random

object Main {
	def main(args:Array[String]):Unit = {
		val r = new Random();
		val dat = (Array.fill(10)(0)).map(x => r.nextInt(10));
		println( bubbleSort(dat).mkString(",") )
		println( insertionSort(dat).mkString(",") )
		println( mergeSort(dat).mkString(",") )
		println( quickSort(dat).mkString(",") )
		
		val heap = new Heap(1000);
		println( heap.isMaxHeap + "->" + { heap.build(); heap.isMaxHeap } )
		
	}
	def swap(dat:Array[Int], i:Int, j:Int):Unit = {
		val t = dat(i);
		dat(i) = dat(j);
		dat(j) = t;
	}
	def bubbleSort(dat_ : Array[Int]):Array[Int] = {
		val dat = dat_.clone().asInstanceOf[Array[Int]];
		for( i <- Range(0,dat.length) ) {
			for( j <- Range(0,dat.length-i-1) ) {
				if( dat(j) > dat(j+1) ) {
					swap(dat, j, j+1)
				}
			}
		}
		dat
	}
	def insertionSort(dat_ : Array[Int]):Array[Int] = {
		val dat = dat_.clone().asInstanceOf[Array[Int]];
		for( i <- Range(0,dat.length-1) ) {
			val t = dat(i+1)
			if( dat(i) > t ) {
				var j = i+1;
				do{
					dat(j) = dat(j-1);
					j = j - 1
				}while( j > 0 && t < dat(j-1) );
				dat(j) = t;
			}
		}
		dat
	}
	def mergeSort(dat_ : Array[Int]):Array[Int] = {
		val dat = dat_.clone().asInstanceOf[Array[Int]];
		def merge(l1 : List[Int], l2 : List[Int], acc : List[Int]):Array[Int] = {
			if (l1.isEmpty && l2.isEmpty)  acc.reverse.toArray[Int]
			else if (l1.isEmpty)           merge(l1, l2.tail, l2.head :: acc)
			else if (l2.isEmpty)           merge(l1.tail, l2, l1.head :: acc)
			else if(l1(0) > l2(0))         merge(l1, l2.tail, l2.head :: acc)
			else                           merge(l1.tail, l2, l1.head :: acc)
		}
		if( dat.size == 1 ) {
			dat
		}else if(dat.size == 2){
			if( dat(0) < dat(1) ) {
				dat
			}else{
				Array[Int](dat(1), dat(0))
			}
		}else{
			val (l1,l2) = dat.splitAt(dat.length/2);
			val l1_ = mergeSort(l1)
			val l2_ = mergeSort(l2)
			merge(l1_.toList, l2_.toList, List[Int]())
		}
	}
	def quickSort(dat_ : Array[Int]):Array[Int] = {
		val dat = dat_.clone().asInstanceOf[Array[Int]];
		def quickSortI(dat : Array[Int], beg:Int, end:Int):Unit={
			if(beg >= end){
				return
			}
			var i=beg;
			var j=end;
			val p = dat((beg+end) / 2);
			var run = true;
			while(run){
				while(dat(i) < p && i < end) i=i+1;
				while(dat(j) > p && j > 0) j=j-1;
				if( i < j ) {
					swap(dat, i, j)
					i=i+1;
					j=j-1;
				}else{
					run = false;
				}
			}
			quickSortI(dat, beg, i-1)
			quickSortI(dat, j+1,end)
		}
		quickSortI(dat, 0, dat.length-1)
		dat
	}
}
