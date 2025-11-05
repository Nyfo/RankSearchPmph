-- Test
-- ==
-- input { empty([0]i32) empty([0]i32) empty([0]i32) empty([0]f32) }
-- output        { empty([0]f32) }
-- input { [1i32] [0i32] empty([0]i32) empty([0]f32) }
-- output        { [0.0f32] }
-- input { [1i32,3i32] [4i32,4i32]
--   [0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32]
--   [4.0f32,1.0f32,9.0f32,2.0f32, 5.0f32,5.0f32,1.0f32,7.0f32] }
-- output { [1.0f32, 5.0f32] }
-- input { [3i32] [4i32]
--   [0i32,0i32,0i32,0i32]
--   [2.0f32,2.0f32,2.0f32,5.0f32] }
-- output { [2.0f32] }
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

    -- 1. Find pivots by segment ends
    let seg_ends = scan (+) 0 shp
    let pivot_inds = map2 (\e s -> if s > 0 then e-1 else 0) seg_ends shp
    let pivots = map2 (\i s -> if s > 0 then A[i] else 0f32) pivot_inds shp

    -- 2. Count less-than and equal-to pivot per segment
    let owners64 = map i64.i32 II1

    let less_flags = map2 (\a j -> if a <  pivots[j] then 1 else 0) A II1
    let equal_flags = map2 (\a j -> if a == pivots[j] then 1 else 0) A II1
    
    let less_counts = hist (+) 0 m owners64 less_flags
    let equal_counts= hist (+) 0 m owners64 equal_flags

    -- 3. Determine kind per segment and update ks and shp
    let kinds =
      map (\(k, s, L, E) ->
              if s == 0 then -1
              else if k <= L then 0
              else if k <= L+E then 1
              else 2)
           (zip4 ks shp less_counts equal_counts)

    let shp' =
      map (\(kind, s, L, E) ->
              if kind == 0 then L
              else if kind == -1 then 0
              else if kind == 1 then 0
              else s - L - E)
          (zip4 kinds shp less_counts equal_counts)

    let ks' =
      map (\(k, kind, L, E) ->
              if kind == 0 then k
              else if kind == 2 then k - L - E
              else -1)
           (zip4 ks kinds less_counts equal_counts)

    -- 4. Update result for segments where we found the k-th element
    let result' =
      map (\i -> if kinds[i] == 1 then pivots[i] else result[i]) (iota m)

    -- 5. Filter A and II1 to keep only elements in segments with kind 0 or 2
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
