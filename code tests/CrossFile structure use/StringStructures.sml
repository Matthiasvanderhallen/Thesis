structure StringEqual:EQUALB = 
    struct
        type t = string;
        fun equal t1 t2  = case String.compare(t1,t2)
                                of EQUAL => true
                                 | _ => false
    end;