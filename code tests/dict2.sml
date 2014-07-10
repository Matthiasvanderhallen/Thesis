signature DICTIONARY =
    sig
        type key
        type 'a dictionary
        val emptyDictionary : 'a dictionary
        val insert : 'a dictionary -> key -> 'a -> 'a dictionary
    end

signature EQUAL =
    sig
        type t
        val equal : t -> t -> bool
    end
(*functor DictionaryFn (KeyStruct:EQUAL) : DICTIONARY = *)
functor DictionaryFn (KeyStruct:EQUAL) :> DICTIONARY where type key = KeyStruct.t =
    struct
        type key = KeyStruct.t
        type 'a dictionary = (key * 'a) list
        val emptyDictionary = []
        fun insert d x y = (x,y)::d
    end

(*functor DictionaryFn2 (KeyStruct:EQUAL) : DICTIONARY =*)
functor DictionaryFn2 (KeyStruct:EQUAL) :> DICTIONARY where type key = KeyStruct.t =
    struct
        type key = KeyStruct.t
        type 'a dictionary = (key list * 'a list)
        val emptyDictionary = ([],[])
        fun insert (a,b) x y = (x::a,y::b)
    end

structure StringEqual:EQUAL = 
    struct
        type t = string;
        fun equal t1 t2  = case String.compare(t1,t2)
                                of EQUAL => true
                                 | _ => false
    end;

structure StringDict = DictionaryFn(StringEqual);
StringDict.emptyDictionary;
StringDict.insert it "a" "b";
StringDict.insert it "c" "d";


val t = fn (x,y) => x;

structure StringDict2 = DictionaryFn2(StringEqual);
StringDict2.emptyDictionary;
StringDict2.insert it "a" "b";
StringDict2.insert it "c" "d";