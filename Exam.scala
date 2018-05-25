// Your name: Luca Irina Alina Gabriela
// Your ITU email: irlu@itu.dk

package adpro.exam2016
object Q1 {
   def checksumImp (in: String) :Int = {
    var result = 0
    for (c <- in.toList)
      result = (result + c.toInt) % 0xffff
    return result
  }

  def checksumFun (in :String) :Int = {
    @annotation.tailrec
    def loop (acc: Int, stringList: List[Char]) : Int = stringList match {
      case Nil => acc
      case h::t => loop((acc + h.toInt) % 0xffff, t)
    }
    loop(0, in.toList) // in tail position
  }


  // Write your answer for Task 2 here.
  // My solution is tail-recursive, because of the use of the inner function (loop()), which 
  // is called recursively in the second pattern matching case 
  // and the last call that gets executed in the entire computation is basically the loop() (the function itself),
  // which makes it be in tail position. In other words, the statement that produces the return value for 
  // checksumFun() is exactly the call to loop().
  // The same fact is checked by adding the annotation '@annotation.tailrec'.
}


object Q2 {
  import fpinscala.monads.Functor
  import scala.language.higherKinds
  // Task 3.
  def onList[A] (f: A => A) :List[A] => List[A] = 
    (l:List[A]) => l.map(a => f(a))

  def onList1[A] (f: A => A) :List[A] => List[A] = 
    (l:List[A]) => {
      def applyF (l :List[A]) :List[A] = l match {
        case Nil => Nil
        case h::t => f(h)::applyF(t)
      }
      applyF(l)
    }
    
  // Task 4.
  def onCollection[C[_],A] (f: A => A)
    (implicit functorC: Functor[C]) :C[A] => C[A] = 
    (ca: C[A]) => functorC.map[A, A] (ca) (a => f(a)) 

}

object Q3 {

  import fpinscala.monoids.Monoid
  import scala.language.higherKinds

  // Task 5.
  def foldBack[A] (l :List[A]) (implicit M :Monoid[A]) :A = 
    (l ++ l.reverse).foldRight(M.zero)(M.op)

}

object Q4 {

  type Computation[A] = A => Either[A,String]

  // Task 6.
  def run[A] (init: A) (progs: List[Computation[A]])
    : (A,List[String]) = {
      @annotation.tailrec
      def loop (accA: A, accListString: List[String], progs: List[Computation[A]]) : (A,List[String]) = progs match {
        case _ if(progs.length == 0) => (accA, accListString)
        case _ => progs(1)(accA) match {
          case Left(a) => loop(a, accListString, progs.drop(1))
          case Right(s) => loop(accA, accListString :+ s, progs.drop(1))
        }        
      }
      loop(init, List[String](), progs)
    }

}


object Q5 {

  sealed trait Tree[A]
  case class Branch[A] (l: () => Tree[A], a: A, r:() => Tree[A]) extends Tree[A]
  case class Leaf[A] (a: A) extends Tree[A]

  // Task 7.
  def multiply (t: Tree[Int]) :Int = t match {
    case Leaf(v) => v
    case Branch(l, a, r) => if(a == 0) 0 else multiply(l()) * a * multiply(r())
  }

  // Task 8. (answer below in a comment)
  // In order to test that the function is not overly eager, I would create a generator of trees Gen[Tree[A]],
  // where the 0 value would be present close to the root and afterwards I would use property-testing 
  // and make use of a good property, to check whether it is applied or not. For instance, I could test whether 
  // the left/right most element of the tree is also forced, in case 0 values are to be met before that element.
  // In case it is forced, the computation is overly eager, because the computation must stop
  // as soon as encountering the 0 value, and not evaluate the whole tree.
  // Another idea would be to create a monadic evaluator which gives me a trace of the computation, 
  // so I can follow if too many values were forced.

}

object Q6 {

  sealed trait Nat[+A]
  case object Zero extends Nat[Unit]
  case class Succ[A] (pred: A) extends Nat[A]

  val zero : Nat[Unit] = Zero            // Task 9.
  val one  : Nat[Nat[Unit]] = Succ (zero)     // Task 9.
  val two  : Nat[Nat[Nat[Unit]]] = Succ (one)      // Task 9.

  // Type annotations for the above values are, as follows:
  // - for zero: Nat[Unit]
  // - for one: Nat[Nat[Unit]]
  // - for two: Nat[Nat[Nat[Unit]]]

  // Task 10.
  def plus2  (x : Nat[Unit] ) : Nat[Nat[Nat[Unit]]] = x match {
    case Zero => two
    case Succ(a) => Succ(Succ(x))
  }   

  // Tests:
  // plus2(Zero)   
  // plus2(Succ(Zero))   
  // plus2(Succ (Zero))   
  // plus2(Succ(Succ (Zero)))   
  

}