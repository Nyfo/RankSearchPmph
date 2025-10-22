-- Tests
-- ==
-- input {[2i32, 3i32] [6i32, 4i32] [0i32,0i32,0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32] [1.0f32, 3.0f32, 2.0f32, 2.0f32, 5.0f32, 4.0f32, 7.0f32, -1.0f32, 0.0f32, 7.0f32]}
-- output {[2.0f32, 7.0f32]}
-- input {[1i32,1i32,2i32] [3i32,0i32,2i32] [0i32,0i32,0i32, 2i32,2i32] [9.0f32, 8.0f32, 8.0f32, -2.0f32, -2.0f32]}
-- output {[8.0f32, 0.0f32, -2.0f32]}
-- input {[1i32] [5i32] [0i32,0i32,0i32,0i32,0i32] [4.5f32, -2.0f32, -2.0f32, 0.0f32, 10.0f32]}
-- output {[-2.0f32]}
-- input {[5i32] [5i32] [0i32,0i32,0i32,0i32,0i32] [3.0f32, 1.0f32, 2.0f32, 5.0f32, 4.0f32]}
-- output {[5.0f32]}
-- input {[1i32, 1i32] [3i32, 3i32] [0i32,0i32,0i32, 1i32,1i32,1i32] [5.0f32, 5.0f32, 5.0f32,  -1.0f32, -1.0f32, -1.0f32]}
-- output {[5.0f32, -1.0f32]}
-- input {[2i32, 1i32, 2i32] [3i32, 0i32, 4i32] [0i32,0i32,0i32,  2i32,2i32,2i32,2i32] [7.0f32, 3.0f32, 9.0f32,   0.0f32, -5.0f32, 0.0f32, 10.0f32]}
-- output {[7.0f32, 0.0f32, 0.0f32]}
-- input {[2i32, 3i32] [4i32, 5i32] [0i32,1i32,0i32,1i32,0i32,1i32,0i32,1i32,1i32] [8.0f32, 0.0f32, 3.0f32, -1.0f32, 7.0f32, 10.0f32, 5.0f32, 10.0f32, 2.0f32]}
-- output {[5.0f32, 2.0f32]}
-- input {[3i32] [6i32] [0i32,0i32,0i32,0i32,0i32,0i32] [2.0f32, 2.0f32, 2.0f32, 2.0f32, 2.0f32, 2.0f32]}
-- output {[2.0f32]}
-- input {[4i32] [7i32] [0i32,0i32,0i32,0i32,0i32,0i32,0i32] [-3.0f32, -3.0f32, 9.0f32, 0.0f32, 0.0f32, 5.0f32, 1.0f32]}
-- output {[0.0f32]}
-- input {[3i32, 2i32] [6i32, 4i32] [0i32,0i32,0i32,0i32,0i32,0i32, 1i32,1i32,1i32,1i32] [100.0f32, -100.0f32, 0.0f32, 50.0f32, -50.0f32, 1.0f32,  3.0f32, 3.0f32, 3.0f32, 3.0f32]}
-- output {[0.0f32, 3.0f32]}

let sgmScan [n] 't (op:t->t->t) (ne:t) (flags:[n]bool) (vals:[n]t) : [n]t =
  scan (\(f1,v1) (f2,v2) -> (f1 || f2, if f2 then v2 else op v1 v2))
       (false, ne) (zip flags vals) |> unzip |> (.1)

let mkFlagArray 't [m] (aoa_shp:[m]i64) (zero:t) (aoa_val:[m]t) : []t =
  let shp_rot = map (\i -> if i == 0 then 0 else aoa_shp[i-1]) (iota m)
  let shp_scn = scan (+) 0 shp_rot
  let aoa_len = if m == 0 then 0 else shp_scn[m-1] + aoa_shp[m-1]
  let shp_ind = map2 (\shp ind -> if shp == 0 then -1 else ind) aoa_shp shp_scn
  in scatter (replicate aoa_len zero) shp_ind aoa_val

let rankSearchBatch [m][n]
  (ks: [m]i32) (shp: [m]i32) (II1: [n]i32) (A: [n]f32) : [m]f32 =
  let result0 = replicate m 0f32
  let (_,_,_,_,result) =
    loop (ks, shp, II1, A, result) = (ks, shp, II1, A, result0)
    while (length A > 0) do

      -- 1. pivots
      let seg_ends   : [m]i32 = scan (+) 0 shp
      let pivot_inds : [m]i32 = map2 (\e s -> if s > 0 then e-1 else 0) seg_ends shp
      let pivots     : [m]f32 = map2 (\i s -> if s > 0 then A[i] else 0f32) pivot_inds shp

      -- 2. flags and counts
      let owners64     : [n]i64 = map i64.i32 II1
      let less_flags   : [n]i32 = map2 (\a j -> if a <  pivots[j] then 1 else 0) A II1
      let equal_flags  : [n]i32 = map2 (\a j -> if a == pivots[j] then 1 else 0) A II1
      let greater_flags: [n]i32 = map2 (\l e -> if l==0 && e==0 then 1 else 0) less_flags equal_flags
      let L : [m]i32 = hist (+) 0 m owners64 less_flags
      let E : [m]i32 = hist (+) 0 m owners64 equal_flags

      -- 3. choose branch
      let kinds : [m]i32 =
        map4 (\s k l e ->
                if s == 0 then -1
                else if k <= l     then 0
                else if k <= l + e then 1
                else                    2)
             shp ks L E

      let ks'  : [m]i32 =
        map4 (\kind k l e -> if kind==0 then k else if kind==2 then k-l-e else -1)
             kinds ks L E

      let shp' : [m]i32 =
        map4 (\s kind l e ->
                if kind == 1 || kind == -1 then 0
                else if kind == 0 then l
                else s - l - e)
             shp kinds L E

      -- 4. write solutions
      let result' : [m]f32 =
        map (\i -> if kinds[i] == 1 then pivots[i] else result[i]) (iota m)

      -- 5. strict 4.a compaction
      let nA_i32 : i32 = if m == 0 then 0 else seg_ends[m-1]
      let nA     : i64 = i64.i32 nA_i32

      -- start-of-segment flags with pinned size [nA]bool
      let seg_flags0 : []bool =
        mkFlagArray (map i64.i32 shp) false (replicate m true)
      let seg_flags  : [nA]bool = seg_flags0 :> [nA]bool

      -- segmented ranks for < and >
      let lt_inc : [nA]i32 = sgmScan (+) 0 seg_flags less_flags
      let gt_inc : [nA]i32 = sgmScan (+) 0 seg_flags greater_flags
      let lt_ex  : [nA]i32 = map2 (-) lt_inc less_flags
      let gt_ex  : [nA]i32 = map2 (-) gt_inc greater_flags

      -- bases for next layout
      let ends'   : [m]i32 = scan (+) 0 shp'
      let starts' : [m]i32 = map2 (-) ends' shp'
      let n'_i32  : i32    = if m == 0 then 0 else ends'[m-1]
      let n'      : i64    = i64.i32 n'_i32

      -- per-element keep decision for chosen branch
      let keep_f : [nA]i32 =
        map (\i ->
              let j  : i32 = II1[i]
              let kd : i32 = kinds[j]
              in if (kd==0 && less_flags[i]==1) || (kd==2 && greater_flags[i]==1)
                 then 1 else 0)
            (iota nA)

      -- destinations: kept → start'[seg] + rank, dropped → parked in tail
      let dest_all : [nA]i64 =
        map (\i ->
              let j   : i32 = II1[i]
              let base: i32 = starts'[j]
              let kd  : i32 = kinds[j]
              let off : i32 = if kd==0 then lt_ex[i] else gt_ex[i]
              in if keep_f[i] == 1
                 then i64.i32 (base + off)
                 else n' + i)
            (iota nA)

      let total  : i64      = n' + nA
      let bigA   : [total]f32 = scatter (replicate total 0f32) dest_all A
      let bigII1 : [total]i32 = scatter (replicate total 0i32) dest_all II1

      let A'   : []f32 = bigA[:n']
      let II1' : []i32 = bigII1[:n']

      in (ks', shp', II1', A', result')

  in result

let main [m][n] (ks:[m]i32) (shp:[m]i32) (II1:[n]i32) (A:[n]f32) =
  rankSearchBatch ks shp II1 A
