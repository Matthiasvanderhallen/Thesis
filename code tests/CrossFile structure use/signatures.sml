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