type 'a btree = Nil | Node of 'a btree * 'a * 'a btree;;

(* construction d'arbres binaires optimisés*)

let construit_ABRo p a = let n=Array.length p in
  let c=Array.make_matrix (n+1) (n+1) 0 in
  let w=Array.make_matrix (n+1) (n+1) 0 in
  let b=Array.make_matrix (n+1) (n+1) 0 in
  let j=ref 0 and m=ref 0 in
  for l=1 to n do
    for i=0 to (n-l) do
      j:=i+l;
      w.(i).(!j)<- w.(i).(!j-1) + p.(!j-1);
      m:=c.(i).(i)+c.(i+1).(!j);
      b.(i).(!j)<- (i+1);
      for k=(i+2) to !j do
        let s=c.(i).(k-1)+c.(k).(!j) in if s<(!m) then
          (m:=s; b.(i).(!j)<- k)
      done;
      c.(i).(!j)<- !m + w.(i).(!j)
    done;
  done;
  let rec build_tree i j = match (b.(i).(j)) with
    | 0 -> Node(Nil, a.(j-1), Nil)
    | k -> let left = if i=(k-1) then Nil else build_tree i (k-1) in
           let right = if j=k then Nil else build_tree k j in
            Node(left, a.(k-1), right)
  in build_tree 0 n;;