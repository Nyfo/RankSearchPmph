let rankSearchBatch [m][n] 't
  (ks: [m]i32) (shp: [m]i32) (II1: [n]i32) (A: [n]t)
  (less:   t -> t -> bool)
  (equal:  t -> t -> bool)
  (neutral: t) : [m]t =
  let result = replicate m neutral
  let (_,_,_,_,result) =
    loop (ks: [m]i32, shp: [m]i32, II1, A, result: [m]t)
    while (length A > 0) do

    -- 1. Find pivots by segment ends and use neutral for empty segments
    let seg_ends   = scan (+) 0 shp
    let pivot_inds = map2 (\e s -> if s > 0 then e-1 else 0) seg_ends shp
    let pivots     = map2 (\i s -> if s > 0 then A[i] else neutral) pivot_inds shp

    -- 2. Count less-than and equal-to pivot per segment and use less/equal functions
    let owners64   = map i64.i32 II1
    let less_flags = map2 (\a j -> if less  a pivots[j] then 1 else 0) A II1
    let equal_flags= map2 (\a j -> if equal a pivots[j] then 1 else 0) A II1
    let less_counts = hist (+) 0 m owners64 less_flags
    let equal_counts = hist (+) 0 m owners64 equal_flags

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

    -- 5. Define greater and filter A and II1 to keep only elements in segments with kind 0 or 2
    -- and use the provided less and equal functions
    let greater (x: t) (y: t) : bool =
      not (less x y || equal x y)

    let keep_idx =
      filter (\i ->
              let j = II1[i]
              let a = A[i]
              let p = pivots[j]
              let k = kinds[j]
              let go_left  = k == 0 && less a p
              let go_right = k == 2 && greater a p
              in  go_left || go_right)
            (indices A)

    let A'   = map (\i -> A[i])   keep_idx
    let II1' = map (\i -> II1[i]) keep_idx
    in (ks', shp', II1', A', result')

  in result

-- calls the generic function with (<), (==), and 0f32 for neutral value
entry check_floats [m][n]
  (ks: [m]i32) (shp: [m]i32) (II1: [n]i32) (A: [n]f32) =
  rankSearchBatch ks shp II1 A (<) (==) 0f32

-- ==
-- entry: check_floats
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

-- Helper to compare arrays for testing
let eq_array 't [n]
  (eq: t -> t -> bool)
  (xs: [n]t) (ys: [n]t) : bool =
  reduce (&&) true (map2 eq xs ys)

-- Months =====================

type month = #Jan | #Feb | #Mar | #Apr | #May | #Jun
           | #Jul | #Aug | #Sep | #Oct | #Nov | #Dec

-- Convert month to integer for comparison
let month_ix (m: month) : i32 =
  match m case #Jan -> 1  case #Feb -> 2  case #Mar -> 3  case #Apr -> 4
          case #May -> 5  case #Jun -> 6  case #Jul -> 7  case #Aug -> 8
          case #Sep -> 9  case #Oct -> 10 case #Nov -> 11 case #Dec -> 12

-- Less-than for months
let lt_month (a: month) (b: month) : bool = month_ix a < month_ix b

-- Test entry (specialised for months)
entry check_months : bool =
  let ks  = [2i32, 1i32]
  let shp = [5i32, 4i32]
  let II1 = [0i32,0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32]
  let A   = [#Mar,#Jan,#Dec,#Feb,#Aug,  #Sep,#Apr,#Jan,#Jul]
  let got     = rankSearchBatch ks shp II1 A lt_month (==) #Jan
  let expect  = [#Feb, #Jan]
  in eq_array (==) got expect

-- ==
-- entry: check_months
-- input {}
-- output {true}

-- Card ranks only (Ace..King) =====================

type rank_only = #Ace | #Two | #Three | #Four | #Five | #Six | #Seven
               | #Eight | #Nine | #Ten | #Jack | #Queen | #King

-- Convert rank to integer for comparison
let rank_only_ix (r: rank_only) : i32 =
  match r
  case #Ace   -> 1  case #Two   -> 2  case #Three -> 3  case #Four -> 4
  case #Five  -> 5  case #Six   -> 6  case #Seven -> 7  case #Eight-> 8
  case #Nine  -> 9  case #Ten   -> 10 case #Jack  -> 11 case #Queen-> 12
  case #King  -> 13

-- Less-than for rank
let lt_rank_only (a: rank_only) (b: rank_only) : bool =
  rank_only_ix a < rank_only_ix b

-- Specialised entry (nice to have for manual runs).
entry rankSearchBatchRanks [m][n]
  (ks:[m]i32) (shp:[m]i32) (II1:[n]i32) (A:[n]rank_only) : [m]rank_only =
  rankSearchBatch ks shp II1 A lt_rank_only (==) #Ace

-- Test entry (specialised for ranks)
entry check_ranks : bool =
  let ks  = [2i32, 3i32]
  let shp = [5i32, 4i32]
  let II1 = [0i32,0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32]
  let A   = [#Ace,#Five,#King,#Two,#Seven,  #Queen,#Queen,#Ace,#Four]
  let got    = rankSearchBatchRanks ks shp II1 A
  let expect = [#Two, #Queen]
  in eq_array (==) got expect

-- ==
-- entry: check_ranks
-- input {}
-- output {true}
