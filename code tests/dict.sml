signature DICTIONARY = 
  sig
  type key				(*type of keys*)
  type 'a t				(*type of tables*)
  exception E of key			(*errors in lookup, insert*)
  val empty: 'a t			(*the empty dictionary*)
  val lookup: 'a t * key -> 'a
  val insert: 'a t * key * 'a -> 'a t
  val update: 'a t * key * 'a -> 'a t
  end;

(*** Dictionaries -- as a functor ***)

(** Linearly ordered types **)

signature ORDER = 
  sig
  type t
  val compare: t*t -> order
  end;

structure StringOrder: ORDER =
  struct
  type t = string;
  val compare = String.compare
  end;


functor Dictionary (Key: ORDER) : DICTIONARY = 
  struct

  type key = Key.t;

  abstype 'a t = Leaf
               | Bran of key * 'a * 'a t * 'a t
    with

    exception E of key;

    val empty = Leaf;

    fun lookup (Bran(a,x,t1,t2), b) =
          (case Key.compare(a,b) of
               GREATER => lookup(t1, b)
             | EQUAL   => x
             | LESS    => lookup(t2, b))
      | lookup (Leaf, b) = raise E b;

    fun insert (Leaf, b, y) = Bran(b, y, Leaf, Leaf)
      | insert (Bran(a,x,t1,t2), b, y) =
          (case Key.compare(a,b) of
               GREATER => Bran(a, x, insert(t1,b,y), t2)
             | EQUAL   => raise E b
             | LESS    => Bran(a, x, t1, insert(t2,b,y)));

    fun update (Leaf, b, y) = Bran(b, y, Leaf, Leaf)
      | update (Bran(a,x,t1,t2), b, y) =
          (case Key.compare(a,b) of
               GREATER => Bran(a, x, update(t1,b,y), t2)
             | EQUAL   => Bran(a, y, t1, t2)
             | LESS    => Bran(a, x, t1, update(t2,b,y)));
    end
  end;


(*This instance is required by sample9.sml and sample10.sml*)
structure StringDict = Dictionary (StringOrder);