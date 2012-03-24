package redblack

abstract class rbColor {
	def show():Unit
}
case class rbRed() extends rbColor {
	def show():Unit = {print("R")}
}
case class rbBlack() extends rbColor{
	def show():Unit = {print("B")}
}

abstract class RedBlackTree {

////////////////////
// Public interface

def insert (value:Int) : RedBlackTree = {
	var sn = ins(value,this)
	sn match {
		case Node(_, l, v, r) => Node(rbBlack(),l,v,r)
	}
}

def member (value:Int) : Boolean = this match {
	case Empty() => false
	case Node(_, l, v, r) => if (value<v) {l member value} else if (value>v) {r member value} else true
}

def show () : Unit = this match {
	case Empty() =>  println("Empty")
	case _       => {this showP; println("")}
}


def delete (value:Int) : RedBlackTree = {
	var dt = del(value,this)
	dt match {
		case Node(_, l,v,r) => Node (rbBlack(), l, v, r)
		case _ => Empty()
	}
}
// delete x t =
	// case del t of {T _ a y b -> T B a y b; _ -> E}

//def insert(value:Int, s:RedBlackTree):RedBlackTree =  {
//	var sn = ins(value,s)
//	sn match {
//		case Node(_, l, v, r) => Node(rbBlack(),l,v,r)
//	}
//}
//def member(value:Int, s:RedBlackTree): Boolean = s match {
//	case Empty() => false
//	case Node(_, l, v, r) => if (value<v) {member(value,l)} else if (value>v) {member(value,r)} else true
//}
//def show(t:RedBlackTree):Unit = t match {
//	case Empty() => Unit
//	case Node(c , l, v, r) => print("("); show(l); c show; print(v) ; show(r); print(")")
//	}

////////////////////
// Private Methods

private def showP () : Unit = this match {
	case Empty() => Unit
	case Node(c , l, v, r) => print("("); l showP; c show; print (":{"+ v + "}") ; r showP; print(")")
}

private def ins (value:Int, s:RedBlackTree) : RedBlackTree = s match {
	case Empty()                   => Node(rbRed(), Empty(), value, Empty())
	case Node(rbBlack(), l, v, r ) => if (value<v) {balance(ins(value, l),v,r) } else if (value>v) { balance(l, v, ins(value,r)) } else s
	case Node(rbRed(), l, v, r )   => if (value<v) {Node(rbRed(),ins(value, l),v,r) } else if (value>v) { Node(rbRed(),l, v, ins(value,r)) } else s
}
	// ins E = T R E x E
	// ins s@(T B a y b)
		// | x<y = balance (ins a) y b
		// | x>y = balance a y (ins b)
		// | otherwise = s
	// ins s@(T R a y b)
		// | x<y = T R (ins a) y b
		// | x>y = T R a y (ins b)
		// | otherwise = s

private def balance (l:RedBlackTree, value:Int, r:RedBlackTree) : RedBlackTree = (l,value,r) match {
	case ( Node(rbRed(), l1, v1, r1), v, Node(rbRed(), l2, v2, r2)) => Node(rbRed(), Node(rbBlack(),l1,v1,r1), v, Node(rbBlack(),l2,v2,r2))
	case ( Node(rbRed(), Node(rbRed(), l1, v1, r1), v, l2), v2, r2) => Node(rbRed(), Node(rbBlack(),l1,v1,r1), v, Node(rbBlack(),l2,v2,r2))
	case ( Node(rbRed(), l1, v1, Node(rbRed(), r1, v, l2)), v2, r2) => Node(rbRed(), Node(rbBlack(),l1,v1,r1), v, Node(rbBlack(),l2,v2,r2))
	case ( l1, v1, Node(rbRed(), r1, v, Node(rbRed(), l2, v2, r2))) => Node(rbRed(), Node(rbBlack(),l1,v1,r1), v, Node(rbBlack(),l2,v2,r2))
	case ( l1, v1, Node(rbRed(), Node(rbRed(), r1, v, l2), v2, r2)) => Node(rbRed(), Node(rbBlack(),l1,v1,r1), v, Node(rbBlack(),l2,v2,r2))
	case ( l1, v1, r1)                                              => Node(rbBlack(), l1,v1,r1)
	}
// balance :: RB a -> a -> RB a -> RB a
// balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
// balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
// balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
// balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
// balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
// balance a x b = T B a x b

private def balleft (l:RedBlackTree, value:Int, r:RedBlackTree) : RedBlackTree = (l,value,r) match {
	case (Node(rbRed(),l1,v1,r1), v, r2) => Node(rbRed(), Node(rbBlack(), l1, v1, r1 ), v, r2)
	case (l1, v, Node(rbBlack(),l2,v2,r2)) => balance(l1, v, Node(rbRed(), l2, v2, r2))
    case (l1, v, Node(rbRed(), Node(rbBlack(), l2, v1, r1) ,v2, r2)) => Node(rbRed(), Node(rbBlack(),l1,v,l2),v1, balance( r1, v2, sub1(r2))) 
}
// balleft :: RB a -> a -> RB a -> RB a
// balleft (T R a x b) y c = T R (T B a x b) y c
// balleft bl x (T B a y b) = balance bl x (T R a y b)
// balleft bl x (T R (T B a y b) z c) = T R (T B bl x a) y (balance b z (sub1 c))

private def balright (l:RedBlackTree, value:Int, r:RedBlackTree) : RedBlackTree = (l,value,r) match {
	case (l1, v, Node(rbRed(),l2,v2,r2))                             => Node(rbRed(), l1, v, Node(rbBlack(),l2, v2, r2))
	case (Node(rbBlack(),l1, v, r1),v2,r2)                           => balance(Node(rbRed(), l1, v, r1), v2, r2)
    case (Node(rbRed(), l1, v, Node(rbBlack(), l2, v1, r1)) ,v2, r2) => Node(rbRed(), balance(sub1(l1),v,l2), v1, Node(rbBlack(), r1, v2, r2)) 
}
// balright :: RB a -> a -> RB a -> RB a
// balright a x (T R b y c) = T R a x (T B b y c)
// balright (T B a x b) y bl = balance (T R a x b) y bl
// balright (T R a x (T B b y c)) z bl = T R (balance (sub1 a) x b) y (T B c z bl)

private def del(value:Int, t:RedBlackTree) = t match {
	case Empty() => Empty()
	case Node(_, l, v, r)      => if (value<v) {delfromLeft (value, l, v, r ) }
	                         else if (value>v) {delfromRight (value, l, v, r ) }  
	                         else app(l, r)
}
	// del E = E
	// del (T _ a y b)
	    // | x<y = delformLeft a y b
	    // | x>y = delformRight a y b
            // | otherwise = app a b


private def delfromLeft(value:Int, l:RedBlackTree, v:Int , r:RedBlackTree):RedBlackTree = l match {
	case Node(rbBlack(),_,_,_) => balleft (del(value,l), v ,r)
	case _                     => Node(rbRed(), del(value,l), v, r)
}
// delformLeft a@(T B _ _ _) y b = balleft (del a) y b
// delformLeft a y b = T R (del a) y b

private def delfromRight(value: Int, l:RedBlackTree, v:Int , r:RedBlackTree):RedBlackTree = r match {
	case Node(rbBlack(),_,_,_) => balright (l, v, del(value, r))
	case _                     => Node(rbRed(), l, v, del(value,r))
}
// delformRight a y b@(T B _ _ _) = balright a y (del b)
// delformRight a y b = T R a y (del b)



private def sub1(t:RedBlackTree): RedBlackTree = t match {
	case Node(rbBlack, l, v, r) => Node(rbRed(), l, v, r)
	case _ => error("invariance violation")
}
// sub1 :: RB a -> RB a
// sub1 (T B a x b) = T R a x b
// sub1 _ = error "invariance violation"


private def app(tl:RedBlackTree, tr:RedBlackTree):RedBlackTree = (tl,tr) match {
	case (Empty(), r) => r
	case (l, Empty()) => l
	case (Node(rbRed(),l1,v1,r1 ), Node(rbRed(),l2,v2,r2 ) ) => {
		var ap = app(r1,l2)
		ap match {
			case Node(rbRed(), l3, v3,r3 ) => Node(rbRed(), Node(rbRed(), l1, v1, l3), v3, Node(rbRed(), r3, v2, r2))
			case _ => Node(rbRed(),l1,v1, Node(rbRed(), ap, v2, r2))
		}
	}
	case (Node(rbBlack(),l1,v1,r1 ), Node(rbBlack(),l2,v2,r2 ) ) => {
	var ap = app(r1,l2)
		ap match {
			case Node(rbRed(), l3, v3,r3 ) => Node(rbRed(), Node(rbBlack(), l1, v1, l3), v3, Node(rbBlack(), r3, v2, r2))
			case _ => balleft(l1,v1, Node(rbBlack(), ap, v2, r2))
		}
	}
	case (l, Node(rbRed(),l1,v1,r1 )) => Node(rbRed(), app (l,l1 ),v1,r1)
	case (Node(rbRed(),l1,v1,r1 ), r) => Node(rbRed(), l1, v1, app (r,r1))
	
}
// app :: RB a -> RB a -> RB a
// app E x = x
// app x E = x
// app (T R a x b) (T R c y d) =
	// case app b c of
	    // T R b' z c' -> T R(T R a x b') z (T R c' y d)
	    // bc -> T R a x (T R bc y d)
// app (T B a x b) (T B c y d) = 
	// case app b c of
	    // T R b' z c' -> T R(T B a x b') z (T B c' y d)
	    // bc -> balleft a x (T B bc y d)
// app a (T R b x c) = T R (app a b) x c
// app (T R a x b) c = T R a x (app b c)

}
case class Empty() extends RedBlackTree 
case class Node(color:rbColor, left:RedBlackTree, value:Int, right:RedBlackTree) extends RedBlackTree


object rbt extends Application {

	override def main(args : Array[String]): Unit = {
		var rb:RedBlackTree = Empty()
//		rb = rb insert 5
//		println ("2 está en el árbol?: " + (rb member 2))
//		println ("5 está en el árbol?: " + (rb member 5))

		(Array(1,2,3,4,5,6,7)).foreach((a:Int)=> rb = rb insert a)

		rb show
		
		(Array(1,2,3,4,5,6,7)).foreach{(a:Int)=> 
			rb = rb delete a
			rb show
		};
	
	}
}

// {- Version 1, 'untyped' -}
// data Color = R | B deriving Show
// data RB a = E | T Color (RB a) a (RB a) deriving Show

// {- Insertion and membership test as by Okasaki -}
// insert :: Ord a => a -> RB a -> RB a
// insert x s =
	// T B a z b
	// where
	// T _ a z b = ins s
	// ins E = T R E x E
	// ins s@(T B a y b)
		// | x<y = balance (ins a) y b
		// | x>y = balance a y (ins b)
		// | otherwise = s
	// ins s@(T R a y b)
		// | x<y = T R (ins a) y b
		// | x>y = T R a y (ins b)
		// | otherwise = s

// member :: Ord a => a -> RB a -> Bool
// member x E = False
// member x (T _ a y b)
	// | x<y = member x a
	// | x>y = member x b
	// | otherwise = True

// {- balance: first equation is new,
   // to make it work with a weaker invariant -}
// balance :: RB a -> a -> RB a -> RB a
// balance (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
// balance (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
// balance (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
// balance a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
// balance a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
// balance a x b = T B a x b

// {- deletion a la SMK -}
// delete :: Ord a => a -> RB a -> RB a
// delete x t =
	// case del t of {T _ a y b -> T B a y b; _ -> E}
	// where
	// del E = E
	// del (T _ a y b)
	    // | x<y = delformLeft a y b
	    // | x>y = delformRight a y b
            // | otherwise = app a b
	// delformLeft a@(T B _ _ _) y b = balleft (del a) y b
	// delformLeft a y b = T R (del a) y b
	// delformRight a y b@(T B _ _ _) = balright a y (del b)
	// delformRight a y b = T R a y (del b)

// balleft :: RB a -> a -> RB a -> RB a
// balleft (T R a x b) y c = T R (T B a x b) y c
// balleft bl x (T B a y b) = balance bl x (T R a y b)
// balleft bl x (T R (T B a y b) z c) = T R (T B bl x a) y (balance b z (sub1 c))

// balright :: RB a -> a -> RB a -> RB a
// balright a x (T R b y c) = T R a x (T B b y c)
// balright (T B a x b) y bl = balance (T R a x b) y bl
// balright (T R a x (T B b y c)) z bl = T R (balance (sub1 a) x b) y (T B c z bl)

// sub1 :: RB a -> RB a
// sub1 (T B a x b) = T R a x b
// sub1 _ = error "invariance violation"

// app :: RB a -> RB a -> RB a
// app E x = x
// app x E = x
// app (T R a x b) (T R c y d) =
	// case app b c of
	    // T R b' z c' -> T R(T R a x b') z (T R c' y d)
	    // bc -> T R a x (T R bc y d)
// app (T B a x b) (T B c y d) = 
	// case app b c of
	    // T R b' z c' -> T R(T B a x b') z (T B c' y d)
	    // bc -> balleft a x (T B bc y d)
// app a (T R b x c) = T R (app a b) x c
// app (T R a x b) c = T R a x (app b c)
