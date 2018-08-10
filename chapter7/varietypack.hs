f   :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (x, _, z) (m, _, o) = ((x, m), (z, o))