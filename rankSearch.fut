-- Test
-- ==
-- input {[2i32, 3i32] [6i32, 4i32] [0i32,0i32,0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32] [1.0f32, 3.0f32, 2.0f32, 2.0f32, 5.0f32, 4.0f32, 7.0f32, -1.0f32, 0.0f32, 7.0f32]}
-- output {[2.0f32, 7.0f32]}
-- input {[1i32,1i32,2i32] [3i32,0i32,2i32] [0i32,0i32,0i32, 2i32,2i32] [9.0f32, 8.0f32, 8.0f32, -2.0f32, -2.0f32]}
-- output {[8.0f32, 0.0f32, -2.0f32]}
-- input {[1i32] [5i32] [0i32,0i32,0i32,0i32,0i32]
--         [4.5f32, -2.0f32, -2.0f32, 0.0f32, 10.0f32]}
-- output {[-2.0f32]}
-- input {[5i32] [5i32] [0i32,0i32,0i32,0i32,0i32]
--         [3.0f32, 1.0f32, 2.0f32, 5.0f32, 4.0f32]}
-- output {[5.0f32]}
-- input {[1i32, 1i32] [3i32, 3i32] [0i32,0i32,0i32, 1i32,1i32,1i32]
--         [5.0f32, 5.0f32, 5.0f32,  -1.0f32, -1.0f32, -1.0f32]}
-- output {[5.0f32, -1.0f32]}
-- input {[2i32, 1i32, 2i32] [3i32, 0i32, 4i32] [0i32,0i32,0i32,  2i32,2i32,2i32,2i32]
--         [7.0f32, 3.0f32, 9.0f32,   0.0f32, -5.0f32, 0.0f32, 10.0f32]}
-- output {[7.0f32, 0.0f32, 0.0f32]}
-- input {[2i32, 3i32] [4i32, 5i32] [0i32,1i32,0i32,1i32,0i32,1i32,0i32,1i32,1i32]
--         [8.0f32, 0.0f32, 3.0f32, -1.0f32, 7.0f32, 10.0f32, 5.0f32, 10.0f32, 2.0f32]}
-- output {[5.0f32, 2.0f32]}
-- input {[3i32] [6i32] [0i32,0i32,0i32,0i32,0i32,0i32]
--         [2.0f32, 2.0f32, 2.0f32, 2.0f32, 2.0f32, 2.0f32]}
-- output {[2.0f32]}
-- input {[4i32] [7i32] [0i32,0i32,0i32,0i32,0i32,0i32,0i32]
--         [-3.0f32, -3.0f32, 9.0f32, 0.0f32, 0.0f32, 5.0f32, 1.0f32]}
-- output {[0.0f32]}
-- input {[3i32, 2i32] [6i32, 4i32] [0i32,0i32,0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32]
--         [100.0f32, -100.0f32, 0.0f32, 50.0f32, -50.0f32, 1.0f32,   3.0f32, 3.0f32, 3.0f32, 3.0f32]}
-- output {[0.0f32, 3.0f32]}
-- compiled input @ 128_datasets/m128_s1000000.in
-- output @ 128_datasets/m128_s1000000.out
-- compiled input @ 128_datasets/m1280_s100000.in
-- output @ 128_datasets/m1280_s100000.out
-- compiled input @ 128_datasets/m12800_s10000.in
-- output @ 128_datasets/m12800_s10000.out
-- compiled input @ 128_datasets/m128000_s1000.in
-- output @ 128_datasets/m128000_s1000.out
-- compiled input @ 128_datasets/m1280000_s100.in
-- output @ 128_datasets/m1280000_s100.out
-- compiled input @ 128_datasets/m12800000_s10.in
-- output @ 128_datasets/m12800000_s10.out

let rankSearchBatch [m][n]
  (ks: [m]i32) (shp: [m]i32) (II1: [n]i32) (A: [n]f32) : [m]f32 =
  let result = replicate m 0f32
  let (_,_,_,_,result) =
    loop (ks: [m]i32, shp: [m]i32, II1, A, result: [m]f32)
    while (length A > 0) do

    -- 1.
    let seg_ends = scan (+) 0 shp
    let pivot_inds = map2 (\e s -> if s > 0 then e-1 else 0) seg_ends shp
    let pivots = map2 (\i s -> if s > 0 then A[i] else 0f32) pivot_inds shp

    -- 2.
    let owners64 = map i64.i32 II1

    let less_flags = map2 (\a j -> if a <  pivots[j] then 1 else 0) A II1
    let equal_flags = map2 (\a j -> if a == pivots[j] then 1 else 0) A II1
    
    let less_counts = hist (+) 0 m owners64 less_flags
    let equal_counts= hist (+) 0 m owners64 equal_flags

    -- 3.
    let kinds =
      map4 (\s k L E ->
              if s == 0 then -1
              else if k <= L then 0
              else if k <= L+E then 1
              else 2)
           shp ks less_counts equal_counts

    let ks' =
      map4 (\kind k L E ->
              if kind == 0 then k
              else if kind == 2 then k - L - E
              else -1)
           kinds ks less_counts equal_counts

    let shp' =
      map4 (\s kind L E ->
              if kind == 1 || kind == -1 then 0
              else if kind == 0 then L
              else s - L - E)
           shp kinds less_counts equal_counts

    -- 4.
    let result' =
      map (\i -> if kinds[i] == 1 then pivots[i] else result[i]) (iota m)

    -- 5.
    --let n = length A
    --let piv_per_elem  = map (\j -> pivots[j]) II1
    --let kind_per_elem = map (\j -> kinds[j])  II1

    let keep_idx =
      filter (\i ->
              let j = II1[i]
              let a = A[i]
              let p = pivots[j]
              let k = kinds[j]
              let go_left  = k == 0 && a < p
              let go_right = k == 2 && a > p
              in  go_left || go_right)
            (indices A)

    let A'   = map (\i -> A[i]) keep_idx
    let II1' = map (\i -> II1[i]) keep_idx


    in (ks', shp', II1', A', result')

  in result

let main [m][n] (ks:  [m]i32) (shp: [m]i32) (II1: [n]i32) (A:   [n]f32) =
  rankSearchBatch ks shp II1 A
