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

structure StringDict = DictionaryFn(StringEqual);

val t = fn (x,y) => x;

structure StringDict2 = DictionaryFn2(StringEqual);