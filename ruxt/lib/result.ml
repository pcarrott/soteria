include Stdlib.Result

let[@inline] fold (module M : Sigs.Foldable) xs ~init ~f =
  Monad.foldM (module M) ~return:ok ~bind:(fun f x -> bind x f) ~init ~f xs

let fold_list xs ~init ~f = fold (module List) xs ~init ~f
let fold_iter xs ~init ~f = fold (module Iter) xs ~init ~f
let fold_seq xs ~init ~f = fold (module Seq) xs ~init ~f

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) x f = map f x
end
