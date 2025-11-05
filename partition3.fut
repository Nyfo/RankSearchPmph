-- Test
-- ==
-- input {[5i32] [0i32,0i32,0i32,0i32,0i32]
--         [2.0f32, 3.0f32, 5.0f32, 1.0f32, 3.0f32] [3.0f32]}
-- output {[0i32] [2i32] [4i32]
--         [2.0f32, 1.0f32, 3.0f32, 3.0f32, 5.0f32]}
-- input {[3i32,2i32] [0i32,0i32,0i32, 1i32,1i32]
--         [2.0f32, 3.0f32, 1.0f32,  -1.0f32, 1.0f32] [3.0f32, 0.0f32]}
-- output {[0i32,3i32] [2i32,4i32] [3i32,4i32]
--         [2.0f32, 1.0f32, 3.0f32,  -1.0f32, 1.0f32]}
-- input {[0i32,4i32,1i32] [1i32,1i32,1i32,1i32, 2i32]
--         [3.0f32, 2.0f32, 1.0f32, 2.0f32, 8.0f32] [5.0f32, 2.0f32, 7.0f32]}
-- output {[0i32,0i32,4i32] [0i32,1i32,4i32] [0i32,3i32,4i32]
--         [1.0f32, 2.0f32, 2.0f32, 3.0f32, 8.0f32]}
-- input {[4i32] [0i32,0i32,0i32,0i32]
--         [1.0f32, 0.0f32, -2.0f32, 3.0f32] [10.0f32]}
-- output {[0i32] [4i32] [4i32]
--         [1.0f32, 0.0f32, -2.0f32, 3.0f32]}
-- input {[5i32] [0i32,0i32,0i32,0i32,0i32]
--         [4.0f32, 4.0f32, 4.0f32, 4.0f32, 4.0f32] [4.0f32]}
-- output {[0i32] [0i32] [5i32]
--         [4.0f32, 4.0f32, 4.0f32, 4.0f32, 4.0f32]}
-- input {[5i32] [0i32,0i32,0i32,0i32,0i32]
--         [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32] [0.0f32]}
-- output {[0i32] [0i32] [0i32]
--         [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32]}
-- input {[2i32,3i32] [0i32,0i32, 1i32,1i32,1i32]
--         [-1.0f32, 0.0f32, 5.0f32, 7.0f32, 1.0f32] [0.0f32, 5.0f32]}
-- output {[0i32,2i32] [1i32,3i32] [2i32,4i32]
--         [-1.0f32, 0.0f32, 1.0f32, 5.0f32, 7.0f32]}
-- input {[0i32,3i32,2i32] [1i32,1i32,1i32, 2i32,2i32]
--         [2.0f32, 2.0f32, 1.0f32, 5.0f32, 5.0f32] [0.0f32, 2.0f32, 5.0f32]}
-- output {[0i32,0i32,3i32] [0i32,1i32,3i32] [0i32,3i32,5i32]
--         [1.0f32, 2.0f32, 2.0f32, 5.0f32, 5.0f32]}


let sgmScan [n] 't
            (op: t -> t -> t)
            (ne: t)
            (flags: [n]bool)
            (vals: [n]t)
            : [n]t =
  scan (\(f1, v1) (f2, v2) -> (f1 || f2, if f2 then v2 else op v1 v2))
       (false, ne)
       (zip flags vals)
  |> unzip
  |> (.1)

let mkFlagArray 't [m] 
            (aoa_shp: [m]i64) (zero: t)
            (aoa_val: [m]t) : []t =
  let shp_rot = map (\i -> if i == 0 then 0 else aoa_shp[i-1]) (iota m)
  let shp_scn = scan (+) 0 shp_rot
  let aoa_len = if m == 0 then 0 else shp_scn[m-1] + aoa_shp[m-1]
  let shp_ind = map2 (\shp ind -> if shp == 0 then -1 else ind) aoa_shp shp_scn
  in scatter (replicate aoa_len zero) shp_ind aoa_val

-- Partition-3 per segment
let partition3_seg [m][n]
  (shp: [m]i32)
  (II1: [n]i32)
  (A:   [n]f32)
  (piv: [m]f32)
  : ([m]i32, [m]i32, [m]i32, [n]f32) =

  -- segment starts: base[j] is the first index of segment j
  let ends  : [m]i32 = scan (+) 0 shp
  let base  : [m]i32 = map2 (-) ends shp

  -- flag array: true at each segment head, false elsewhere
  let F0    : []bool  = mkFlagArray (map i64.i32 shp) false (replicate m true)
  let F     : [n]bool = F0 :> [n]bool

  -- three predicates per element (relative to its segment pivot)
  -- the partition3 version of the cs in the partition2 pseudocode
  let lt_cs  : [n]bool = map2 (\a j -> a < piv[j]) A II1
  let eq_cs  : [n]bool = map2 (\a j -> a == piv[j]) A II1
  let gt_cs  : [n]bool = map2 (\lt eq -> not lt && not eq) lt_cs eq_cs

  -- boolean versions
  let t_lt  : [n]i32  = map i32.bool lt_cs
  let t_eq  : [n]i32  = map i32.bool eq_cs
  let t_gt  : [n]i32  = map i32.bool gt_cs

  -- segmented inclusive scans:
  -- is_lt[i] = scan of number of (<) within its segment
  -- is_eq[i] = scan of number of (==) within its segment
  -- is_gt[i] = scan of number of (>) within its segment
  let is_lt0 : [n]i32 = sgmScan (+) 0 F t_lt
  let is_eq0 : [n]i32 = sgmScan (+) 0 F t_eq
  let is_gt0 : [n]i32 = sgmScan (+) 0 F t_gt

  -- per-segment totals from the last element of each segment
  let end_ix : [m]i32 = map2 (\b s -> if s > 0 then b + s - 1 else 0) base shp
  let L : [m]i32 = map (\j -> if shp[j] > 0 then is_lt0[end_ix[j]] else 0) (iota m)
  let E : [m]i32 = map (\j -> if shp[j] > 0 then is_eq0[end_ix[j]] else 0) (iota m)

  -- start indices for each partition within each segment
  -- the partition3 version of the i variable in the partition2 pseudocode.
  -- here, we compute the segment-wise starts for lt, eq, and gt partitions.
  let start_lt : [m]i32 = base
  let start_eq : [m]i32 = map2 (+) start_lt L
  let start_gt : [m]i32 = map2 (+) start_eq E
  
  -- add the appropriate segment starts to the segmented scans
  -- the partition3 version of the isT and isF variables in the partition2 pseudocode.
  let is_lt : [n]i32 = map2 (\seg iL -> start_lt[seg] + iL) II1 is_lt0
  let is_eq : [n]i32 = map2 (\seg iE -> start_eq[seg] + iE) II1 is_eq0
  let is_gt : [n]i32 = map2 (\seg iG -> start_gt[seg] + iG) II1 is_gt0

  -- final indices for each element based on its predicate
  -- the partition3 version of the inds variable in the partition2 pseudocode.
  let inds : [n]i32 =
    map (\(lt, eq, iLT, iEQ, iGT) ->
            if lt then iLT - 1
                  else if eq then iEQ - 1
                  else iGT - 1)
         (zip5 lt_cs eq_cs is_lt is_eq is_gt)

  -- 3-way partition
  let B : [n]f32 = scatter (replicate n 0f32) (map i64.i32 inds) A
  in (start_lt, start_eq, start_gt, B)

let main [m][n]
  (shp: [m]i32) (II1: [n]i32) (A: [n]f32) (piv: [m]f32) =
  partition3_seg shp II1 A piv