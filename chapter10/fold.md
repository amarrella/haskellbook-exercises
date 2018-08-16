Write evaluation steps for `foldl (flip (*)) 1 [1..3]`

`foldl (flip (*)) 1 [1..3]`
```
f = flip *
(((1 `f` 1) `f` 2) `f` 3)
((1 `f` 2) `f` 3)
(2 `f` 3)
6
```