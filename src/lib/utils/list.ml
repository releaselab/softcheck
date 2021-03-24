include Stdlib.List

let to_string ~fst ~lst ~sep f l = fst ^ String.concat sep (map f l) ^ lst
