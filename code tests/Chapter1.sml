signature DICTIONARYSIGNATURE =
	sig
		type 'a dictionary
		val emptyDictionary : 'a dictionary
		val insert: 'a dictionary * string * 'a -> 'a dictionary
end

structure Dictionary:>DICTIONARYSIGNATURE =
	struct
		type 'a dictionary = (string list * 'a list)
		val emptyDictionary = ([],[])
		fun insert((fst,snd), x, y) = (x::fst, y::snd)
	end

structure Dictionary2:>DICTIONARYSIGNATURE = 
	struct
		type 'a dictionary = (string * 'a) list
		val emptyDictionary = []
		fun insert(d,x,y) = (x,y)::d
	end

(*
val t = fn (x,y) => x;
Dictionary.emptyDictionary;
Dictionary.insert(it,"1",1);  
Dictionary.insert(it,"2",2);
t(it);

Dictionary2.emptyDictionary;
Dictionary2.insert(it,"1","2");  
Dictionary2.insert(it,"2","3");
t(it);
*)