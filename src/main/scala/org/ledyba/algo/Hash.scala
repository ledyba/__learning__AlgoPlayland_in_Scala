package org.ledyba.algo;

class HashMap(size:Int = 1024) {
	val slots = new Array[(String,String)](size);
	val deleted:(String,String) = (null, null);
	def hash(str:String) = {
		var s = 0;
		for( c <- str ){
			s = s*31+c
		}
		s
	}
	def rangedHash(i:Int) = i % size
	def add(str:String, item:String):Boolean = {
		val idx = rangedHash(hash(str))
		for( i <- Range(0, size) ){
			val ind = (idx + i) % size
			if( slots(ind) == null || slots(ind).eq(deleted) ){
				slots(ind) = (str, item);
				return true;
			}
		}
		return false;
	}
	def search(str:String):Option[String] = {
		val idx = rangedHash(hash(str))
		for( i <- Range(0, size) ){
			val ind = (idx + i) % size
			if( slots(ind) != null && slots(ind)._1 == str ){
				return Some(slots(ind)._2);
			}
		}
		return None;
	}
	def remove(str:String):Boolean = {
		val idx = rangedHash(hash(str))
		for( i <- Range(0, size) ){
			val ind = (idx + i) % size
			if( slots(ind) != null && slots(ind)._1 == str ){
				slots(ind) = deleted;
				return true;
			}
		}
		return false;
	}
}
