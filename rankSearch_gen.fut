-- Test
-- ==
-- input {[2i32, 3i32] [6i32, 4i32] [0i32,0i32,0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32] [1.0f32, 3.0f32, 2.0f32, 2.0f32, 5.0f32, 4.0f32, 7.0f32, -1.0f32, 0.0f32, 7.0f32]}
-- output {[2.0f32, 7.0f32]}
-- input {[1i32,1i32,2i32] [3i32,0i32,2i32] [0i32,0i32,0i32, 2i32,2i32] [9.0f32, 8.0f32, 8.0f32, -2.0f32, -2.0f32]}
-- output {[8.0f32, 0.0f32, -2.0f32]}
-- input {[1i32] [5i32] [0i32,0i32,0i32,0i32,0i32] [4.5f32,-2.0f32,-2.0f32,0.0f32,10.0f32]}
-- output {[-2.0f32]}
-- input {[5i32] [5i32] [0i32,0i32,0i32,0i32,0i32] [3.0f32,1.0f32,2.0f32,5.0f32,4.0f32]}
-- output {[5.0f32]}
-- input {[1i32,1i32] [3i32,3i32] [0i32,0i32,0i32, 1i32,1i32,1i32] [5.0f32,5.0f32,5.0f32,-1.0f32,-1.0f32,-1.0f32]}
-- output {[5.0f32, -1.0f32]}
-- input {[2i32,1i32,2i32] [3i32,0i32,4i32] [0i32,0i32,0i32, 2i32,2i32,2i32,2i32] [7.0f32,3.0f32,9.0f32, 0.0f32,-5.0f32,0.0f32,10.0f32]}
-- output {[7.0f32, 0.0f32, 0.0f32]}
-- input {[2i32,3i32] [4i32,5i32] [0i32,1i32,0i32,1i32,0i32,1i32,0i32,1i32,1i32] [8.0f32,0.0f32,3.0f32,-1.0f32,7.0f32,10.0f32,5.0f32,10.0f32,2.0f32]}
-- output {[5.0f32, 2.0f32]}
-- input {[3i32] [6i32] [0i32,0i32,0i32,0i32,0i32,0i32] [2.0f32,2.0f32,2.0f32,2.0f32,2.0f32,2.0f32]}
-- output {[2.0f32]}
-- input {[4i32] [7i32] [0i32,0i32,0i32,0i32,0i32,0i32,0i32] [-3.0f32,-3.0f32,9.0f32,0.0f32,0.0f32,5.0f32,1.0f32]}
-- output {[0.0f32]}
-- input {[3i32,2i32] [6i32,4i32] [0i32,0i32,0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32] [100.0f32,-100.0f32,0.0f32,50.0f32,-50.0f32,1.0f32, 3.0f32,3.0f32,3.0f32,3.0f32]}
-- output {[0.0f32, 3.0f32]}

let rankSearchBatch [m][n] 't
  (ks: [m]i32) (shp: [m]i32) (II1: [n]i32) (A: [n]t)
  (less:   t -> t -> bool)
  (equal:  t -> t -> bool)
  (neutral: t) : [m]t =
  let result = replicate m neutral
  let (_,_,_,_,result) =
    loop (ks: [m]i32, shp: [m]i32, II1, A, result: [m]t)
    while (length A > 0) do

    let seg_ends   = scan (+) 0 shp
    let pivot_inds = map2 (\e s -> if s > 0 then e-1 else 0) seg_ends shp
    let pivots     = map2 (\i s -> if s > 0 then A[i] else neutral) pivot_inds shp

    let owners64   = map i64.i32 II1
    let less_flags = map2 (\a j -> if less  a pivots[j] then 1 else 0) A II1
    let equal_flags= map2 (\a j -> if equal a pivots[j] then 1 else 0) A II1
    let L = hist (+) 0 m owners64 less_flags
    let E = hist (+) 0 m owners64 equal_flags

    let kinds =
      map4 (\s k l e ->
              if s == 0 then -1
              else if k <= l then 0
              else if k <= l+e then 1
              else 2)
           shp ks L E

    let ks' =
      map4 (\kind k l e ->
              if kind == 0 then k
              else if kind == 2 then k - l - e
              else -1)
           kinds ks L E

    let shp' =
      map4 (\s kind l e ->
              if kind == 1 || kind == -1 then 0
              else if kind == 0 then l
              else s - l - e)
           shp kinds L E

    let result' =
      map (\i -> if kinds[i] == 1 then pivots[i] else result[i]) (iota m)

    let n = length A
    let piv_per_elem  = map (\j -> pivots[j]) II1
    let kind_per_elem = map (\j -> kinds[j])  II1
    let greater = (\x y -> not (less x y || equal x y))

    let keep_idx =
      filter (!= -1)
        (map (\i ->
                let a = A[i]
                let p = piv_per_elem[i]
                let k = kind_per_elem[i]
                in if (k == 0 && less a p) || (k == 2 && greater a p)
                   then i else -1)
             (iota n))

    let A'   = map (\i -> A[i])   keep_idx
    let II1' = map (\i -> II1[i]) keep_idx
    in (ks', shp', II1', A', result')

  in result

-- calls the generic function with (<), (==), and a neutral value
entry main [m][n]
  (ks: [m]i32) (shp: [m]i32) (II1: [n]i32) (A: [n]f32) =
  rankSearchBatch ks shp II1 A (<) (==) 0f32