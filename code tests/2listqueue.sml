signature QUEUE = 
sig
   type 'a queue
   exception Queue
   val empty     : 'a queue
   val isEmpty   : 'a queue -> bool
   val singleton : 'a -> 'a queue
   val insert    : 'a * 'a queue -> 'a queue
   val peek      : 'a queue -> 'a
   val remove    : 'a queue -> 'a * 'a queue
end

structure TwoListQueue    :> QUEUE = 
struct
  type 'a queue = 'a list * 'a list
  exception Queue

  val empty = ([],[])

  fun isEmpty ([],[]) = true
    | isEmpty _ = false
  
  fun singleton a = ([], [a])

  fun insert (a, ([], [])) = ([], [a])
    | insert (a, (ins, outs)) = (a::ins, outs)
  
  fun peek (_,[]) = raise Queue
    | peek (ins, a::outs) = a
  
  fun remove (_,[]) = raise Queue
    | remove (ins, [a]) = (a, ([], rev ins))
    | remove (ins, a::outs) = (a, (ins,outs))
   
end