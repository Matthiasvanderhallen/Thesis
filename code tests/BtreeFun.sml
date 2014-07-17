signature TOTALORDER = sig
  type element
  val lt : element * element -> bool
end;

signature BST =
  sig
    type element
    type 'a btree
    val create : element btree
    val lookup : element * element btree -> bool
    val insert : element * element btree -> element btree
  end

functor MakeBST(Lt:TOTALORDER):>
  BST where type element = Lt.element
  =
  struct
    open Lt

    datatype 'a btree =
             Empty
           | Node of 'a * 'a btree * 'a btree;

    val create = Empty

    fun lookup(x, Empty) = false
      | lookup(x, Node(v, left, right)) =
        if Lt.lt(x, v)      then lookup(x, left)
        else if Lt.lt(v, x) then lookup(x, right)
        else true

    fun insert(x, Empty) = Node(x, Empty, Empty)
      | insert(x, n as Node(v, left, right)) =
        if Lt.lt(x, v)    then Node(v, insert(x, left), right)
        else if Lt.lt(v, x) then Node(v, right, insert(x, right))
        else n
  end;

structure StringLt : TOTALORDER =
struct
  type element = string
  val lt:(string * string -> bool) = op <;
end;

structure StringBST = MakeBST(StringLt);
