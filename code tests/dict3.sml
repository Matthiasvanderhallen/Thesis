signature EQUAL =
    sig
        type t
        val equal : t * t -> bool
    end

signature DICTIONARY =
    sig
		structure Key:EQUAL
	    type 'a dictionary
        
        val emptyDictionary : 'a dictionary
        val insert : 'a dictionary -> Key.t -> 'a -> 'a dictionary
    end

functor DictionaryFn (KeyStruct:EQUAL) : DICTIONARY = 
    struct
        structure Key : EQUAL = KeyStruct
        type 'a dictionary = (Key.t *'a) list
        val emptyDictionary = []
        fun insert d x y = (x,y)::d : (KeyStruct.t * 'a) list
        (* val lookup = emptyDictionary *)
    end

structure StringEqual:EQUAL = 
    struct
        type t = string;
        fun equal (t1,t2) = case String.compare(t1,t2)
                                of EQUAL => true
                                 | _ => false
    end;

structure StringDict = DictionaryFn(StringEqual);
(*
StringDict.emptyDictionary;
StringDict.insert it "a" "b";
StringDict.insert it "c" "d";
*)