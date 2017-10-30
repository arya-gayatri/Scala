import com.sun.org.apache.xpath.internal.functions.FuncFalse
import sun.font.TrueTypeFont

object inheritance_bst{

  val t1 = new NonEmptyTree(3, new EmptyTree, new EmptyTree)
  val t2 = t1.incl(4)

  abstract class IntSet{

    def contains(x:Int): Boolean
    def incl(x:Int): IntSet
    def union(other:IntSet): IntSet

  }

  class EmptyTree extends IntSet{

    def contains(x: Int): Boolean = false
    def incl(x:Int): IntSet =  new NonEmptyTree(x, new EmptyTree, new EmptyTree)
    override def toString() = "."
    def union(other:IntSet): IntSet = other

  }

  class NonEmptyTree(elem: Int, left:IntSet, right:IntSet) extends IntSet{

    def contains(x:Int): Boolean = {

      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true
    }

    def incl(x:Int): IntSet = {

      if(x<elem) new NonEmptyTree(elem, left incl x, right)
      else if(x>elem) new NonEmptyTree(elem, left, right incl x)
      else this
    }

    def union(other:IntSet): IntSet = {
      ((left union right)union other) incl elem
    }

    override def toString = "{"+left+elem+right+"}"

    }

  }

