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

-- Months =====================

type month = #Jan | #Feb | #Mar | #Apr | #May | #Jun
           | #Jul | #Aug | #Sep | #Oct | #Nov | #Dec

let month_ix (m: month) : i32 =
  match m case #Jan -> 1  case #Feb -> 2  case #Mar -> 3  case #Apr -> 4
          case #May -> 5  case #Jun -> 6  case #Jul -> 7  case #Aug -> 8
          case #Sep -> 9  case #Oct -> 10 case #Nov -> 11 case #Dec -> 12

let lt_month (a: month) (b: month) : bool = month_ix a < month_ix b

-- Build ADT data inside; check result against the expected months.
entry check_months : bool =
  let ks  = [2i32, 1i32]
  let shp = [5i32, 4i32]
  let II1 = [0i32,0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32]
  let A   = [#Mar,#Jan,#Dec,#Feb,#Aug,  #Sep,#Apr,#Jan,#Jul]
  let got     = rankSearchBatch ks shp II1 A lt_month (==) #Jan
  let expect  = [#Feb, #Jan]
  in got == expect

-- ==
-- entry: check_months
-- input {}
-- output {true}

-- Card ranks only (Ace..King) =====================

type rank_only = #Ace | #Two | #Three | #Four | #Five | #Six | #Seven
               | #Eight | #Nine | #Ten | #Jack | #Queen | #King

let rank_only_ix (r: rank_only) : i32 =
  match r
  case #Ace   -> 1  case #Two   -> 2  case #Three -> 3  case #Four -> 4
  case #Five  -> 5  case #Six   -> 6  case #Seven -> 7  case #Eight-> 8
  case #Nine  -> 9  case #Ten   -> 10 case #Jack  -> 11 case #Queen-> 12
  case #King  -> 13

let lt_rank_only (a: rank_only) (b: rank_only) : bool =
  rank_only_ix a < rank_only_ix b

-- Specialised entry (nice to have for manual runs).
entry rankSearchBatchRanks [m][n]
  (ks:[m]i32) (shp:[m]i32) (II1:[n]i32) (A:[n]rank_only) : [m]rank_only =
  rankSearchBatch ks shp II1 A lt_rank_only (==) #Ace

-- Build ADT data inside; check result against expected ranks.
entry check_ranks : bool =
  let ks  = [2i32, 3i32]
  let shp = [5i32, 4i32]
  let II1 = [0i32,0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32]
  let A   = [#Ace,#Five,#King,#Two,#Seven,  #Queen,#Queen,#Ace,#Four]
  let got    = rankSearchBatchRanks ks shp II1 A
  let expect = [#Two, #Queen]
  in got == expect

-- ==
-- entry: check_ranks
-- input {}
-- output {true}

-- Months as strings (calendar order) =====================

-- Map month name -> calendar index (three-letter abbreviations)
let month_rank (s: [3]u8) : i32 =
  if s == "Jan" then 1 else if s == "Feb" then 2 else if s == "Mar" then 3 else
  if s == "Apr" then 4 else if s == "May" then 5 else if s == "Jun" then 6 else
  if s == "Jul" then 7 else if s == "Aug" then 8 else if s == "Sep" then 9 else
  if s == "Oct" then 10 else if s == "Nov" then 11 else 12

let lt_month_str (a: [3]u8) (b: [3]u8) : bool =
  month_rank a < month_rank b

-- exact equality for 3-byte strings
let eq_bytes3 (a: [3]u8) (b: [3]u8) : bool =
  all id (map2 (==) a b)

-- Specialise to fixed-length month strings (3 bytes)
entry rankSearchBatchMonthStr [m][n]
  (ks:[m]i32) (shp:[m]i32) (II1:[n]i32) (A:[n][3]u8) : [m][3]u8 =
  -- neutral must also be 3 chars
  rankSearchBatch ks shp II1 A lt_month_str eq_bytes3 "Jan"

-- Validation with months as strings (calendar order)
-- ==
-- entry: rankSearchBatchMonthStr
-- input {[2i32, 1i32]
--        [5i32, 4i32]
--        [0i32,0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32]
--        ["Mar","Jan","Dec","Feb","Aug",  "Sep","Apr","Jan","Jul"]}
-- output {["Feb","Jan"]}
