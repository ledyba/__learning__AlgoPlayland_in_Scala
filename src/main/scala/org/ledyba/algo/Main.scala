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
import Util.shuffle;

object Main {
	def main(args:Array[String]):Unit = {
		val r = new Random();
		val dat = shuffle(Range(0, 10).toArray)
		println( "bubble  : " + bubbleSort(dat).mkString(",") )
		println( "insert  : " + insertionSort(dat).mkString(",") )
		println( "merge   : " + mergeSort(dat).mkString(",") )
		println( "quick   : " + quickSort(dat).mkString(",") )
		println( "heap    : " + heapSort(dat).mkString(",") )
		println( "counting: " + countingSort(dat, dat.length-1).mkString(",") )
		println( "bucket  : " + bucketSort(dat, dat.length-1).mkString(",") )
		//codeJam();
		
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
	def heapSort(dat_ : Array[Int]):Array[Int] = {
		val dat = dat_.clone().asInstanceOf[Array[Int]];
		val heap = new Heap(dat);
		heap.build();
		for( i <- Range(0, dat.size).reverse ) {
			val t = heap.pop();
			dat(i) = t;
		}
		dat
	}
	def countingSort(dat_ : Array[Int], max:Int):Array[Int] = {
		val dat = new Array[Int](dat_.size);
		val cnt = (new Array[Int](max+1)).map(x => 0);
		dat_.foreach(i => cnt(i) = cnt(i) + 1)
		Stream.from(1).zip(cnt.tail).foreach( tupled { (i,d) => cnt(i) = cnt(i-1)+d } )
		dat_.foreach(x => {cnt(x) = cnt(x)-1;dat(cnt(x)) = x;} )
		dat
	}
	def bucketSort(dat_ : Array[Int], max:Int):Array[Int] = {
		val dat = new Array[Int](dat_.size);
		val n = Math.log(dat_.length).toInt;
		val bucket = new Array[List[Int]](n);
		def insertTo(lst : List[Int], v:Int):List[Int] = {
			if(lst == null){
				v :: List[Int]();
			}else if(lst.isEmpty) {
				v :: lst
			}else if( lst.head > v ){
				v :: lst
			}else{
				lst.head :: insertTo(lst.tail, v)
			}
		}
		dat_.foreach(i =>{
			val idx = n * i / (max+1);
			bucket(idx) = insertTo( bucket(idx), i )
		})
		def allEmpty = bucket.foldLeft(true)( (b,x) => b && (x == null || x.isEmpty) )
		var c = 0;
		while( !allEmpty ) {
			val (i, lst) = Stream.from(0).zip(bucket).minBy(tupled { (_, l) => if(l == null || l.isEmpty) max+1 else l.head } )
			bucket(i) = lst.tail
			dat(c) = lst.head
			c = c+1;
		}
		dat
	}
	def codeJam():Unit={
		val f = new BufferedReader(new InputStreamReader(new FileInputStream("A-large-practice.in.txt")))
		val n = Integer.valueOf(f.readLine());
		val dat = new Array[Array[String]](n);
		for( i <- Range(0,n) ){
			val d = new Array[String](Integer.valueOf(f.readLine()));
			for( j <- Range(0,d.size) ){
				d(j) = f.readLine();
			}
			dat(i) = d;
		}
		f.close();
		def reduce(str:String)={
			var last = str.head;
			var cont = 1;
			var lst = List[(Int, Char)]();
			val b = new StringBuilder();
			b.append(str.head);
			for( c <- str.tail ) {
				if( last != c ){
					lst = (cont, last) :: lst;
					b.append(last);
					last = c;
					cont = 1;
				}else{
					cont = cont+1;
				}
			}
			lst = (cont, last) :: lst;
			b.append(last);
			(b.toString(), lst.reverse.toArray)
		}
		for( (i, pr) <- Stream.from(0).zip(dat) ){
			val reduced = new Array[(String, Array[(Int, Char)])](pr.size)
			for ((i,it) <-Stream.from(0).zip(pr)) {
				reduced(i) = reduce(it)
			}
			if( reduced.tail.foldLeft(true)((b,s) => b && s._1 == reduced(0)._1 ) ){
				var total = 0;
				for( i <- Range(0, reduced.head._2.size) ) {
					val max = reduced.maxBy(t => t._2(i)._1)._2(i)._1
					val min = reduced.minBy(t => t._2(i)._1)._2(i)._1
					var m = 10000;
					for( c <- (min to max) ) {
						var s = 0;
						for( (_, cmp) <- reduced ){
							s = s + Math.abs(cmp(i)._1 - c)
						}
						if( s < m ) {
							m = s;
						}
					}
					total = total + m;
				}
				println("Case #%d: %d".format(i+1, total));
			}else{
				println("Case #%d: Fegla Won".format(i+1));
			}
		}
	}
}
