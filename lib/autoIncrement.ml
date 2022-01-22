type t = {current: int ref}

let create () = {current= ref 0}

let get {current} =
  let ret = !current in
  current := ret + 1 ;
  ret
