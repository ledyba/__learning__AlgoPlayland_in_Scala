package org.ledyba.algo;


class BinaryTree() {
	case class Item(var parent:Item, val value:Int, var left:Item = null, var right:Item = null) {
	}
	var root:Item = null;
	def add(v:Int) = {
		if( root == null ){
			root = Item(null, v);
		}else{
			def addI(v:Int, node:Item):Unit = {
				if( v < node.value ) {
					if( node.left == null ) {
						node.left = Item(node, v);
					}else{
						addI(v, node.left);
					}
				}else{
					if( node.right == null ) {
						node.right = Item(node, v);
					}else{
						addI(v, node.right);
					}
				}
			}
			addI(v, root)
		}
	}
	def toList:List[Int] = {
		def toListI( node:Item ):List[Int] = {
			if (node == null) List.empty
			else toListI(node.left) ++ List[Int](node.value) ++ toListI(node.right);
		}
		toListI(root)
	}
	def isValid() = {
		def isValidI(node:Item):Boolean = {
			node == null || (
				(node.left == null ||
						(node.left.value <= node.value && isValidI(node.left))) &&
				(node.right == null ||
						(node.right.value >= node.value && isValidI(node.right)))
			)
		}
		isValidI(root);
	}
	def minimum(node:Item) : Item = {
		if(node == null) node
		else if(node.left == null) node
		else minimum(node.left)
	}
	def remove(v:Int):Boolean = {
		def removeI(node:Item, v:Int):Boolean = {
			if( node.value == v ) {
				if( node.right == null && node.left == null ) {
					if(node.parent == null) root = null;
					else if( node.parent.left == node ) {
						node.parent.left = null;
					}else{
						node.parent.right = null;
					}
				}else if( node.right == null ) {
					if(node.parent == null) root = node.left;
					else if( node.parent.left == node ) {
						node.parent.left = node.left;
					}else{
						node.parent.right = node.left;
					}
					node.left.parent = node.parent;
				}else if( node.left == null ) {
					if(node.parent == null) root = node.right;
					else if( node.parent.left == node ) {
						node.parent.left = node.right;
					}else{
						node.parent.right = node.right;
					}
					node.right.parent = node.parent;
				}else{
					val mini = minimum(node.right)
					if( mini.parent != node ) {
						mini.parent.left = mini.right;
						if( mini.right != null ) mini.right.parent = mini.parent;
						mini.right = node.right;
						mini.right.parent = mini;
					}
					if(node.parent == null) root = mini;
					else if( node.parent.left == node ) {
						node.parent.left = mini;
					}else{
						node.parent.right = mini;
					}
					mini.parent = node.parent;
					mini.left = node.left;
					mini.left.parent = mini;
				}
				true
			}else if( v < node.value ){
				if( node.left != null ){
					removeI(node.left, v);
				}else{
					false;
				}
			}else{
				if( node.right != null ){
					removeI(node.right, v);
				}else{
					false;
				}
			}
		}
		root != null && removeI(root, v)
	}
}