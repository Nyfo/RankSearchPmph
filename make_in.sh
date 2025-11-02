#!/usr/bin/env bash

make_in () {
  M=$1         # segments
  S=$2         # segment size
  N=$((M*S))
  OUT=$3
  K=$(((S+1)/2))   # median k (1-based)

  { # ks, shp, II1, A  — in the order your main expects
    futhark dataset -g "[$M]i32" --i32-bounds=$K:$K
    futhark dataset -g "[$M]i32" --i32-bounds=$S:$S
    awk -v m=$M -v s=$S 'BEGIN{
      printf "["; sep="";
      for(i=0;i<m;i++) for(j=0;j<s;j++){
        printf "%s%di32", sep, i; sep=", ";
      }
      print "]"}'
    futhark dataset -g "[$N]f32" --f32-bounds=-100:100
  } > "$OUT"
  echo "wrote $OUT  (m=$M, s=$S, n=$N)"
}

pairs=(
  "10 1000000"
  "20 500000"
  "50 200000"
  "100 100000"
  "200 50000"
  "1000 10000"
  "5000 2000"
  "10000 1000"
  "100000 100"
  "1000000 10"
)

for p in "${pairs[@]}"; do
  set -- $p
  make_in "$1" "$2" "rsb_m${1}_s${2}.in"
done
