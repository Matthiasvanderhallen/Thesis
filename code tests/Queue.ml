module type ORDERED_TYPE =
    sig
      type t
      val compare: t -> t -> bool
    end;;