fun t:ts g:gs = 0
  --| t < g     = zipWith (+) [0,0,0] (fun ts (g:gs))
  --| t > g     = zipWith (+) [0,0,0] (fun (t:ts) gs)
  --| otherwise = zipWith (+) [0,1,0] (fun ts gs)