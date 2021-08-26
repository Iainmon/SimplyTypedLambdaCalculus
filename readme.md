# Simply Typed Lambda Calculus in Haskell

```lisp
T := (\a.(\b.(a)))
K := T
S := (\x.(\y.(\z.((x z)(y z)))))
I := (\x.x)
F := S K

cons := (\h.(\l.(\con.(con h l))))
head := (\l.l (\a.(\b.a)))
tail := (\l.l (\a.(\b.b)))
mylist := (cons x (cons y nil))
pair of (head mylist) (tail mylist)

flip := (\a.(\b.(\c.(a c b))))

(\x.x:Type) (y:Type)
```

```haskell
*Main> debug "expr.lc"
Debugging expr.lc
Type correctness: True
-- Program Source --
T := (\a.(\b.(a)))
K := T
S := (\x.(\y.(\z.((x z)(y z)))))
I := (\x.x)
F := S K
cons := (\h.(\l.(\con.(con h l))))
head := (\l.l (\a.(\b.a)))
tail := (\l.l (\a.(\b.b)))
mylist := (cons x (cons y nil))
pair of (head mylist) (tail mylist)
flip := (\a.(\b.(\c.(a c b))))
(\x.x:Type) (y:Type)

-- Program Assignments --
flip | (\a.(\b.(\c.((a c) b)))) : (*->(*->(*->*)))
mylist | (\con.((con x) (\con.((con y) nil)))) : (*->*)
tail | (\l.(l (\a.(\b.b)))) : (*->*)
head | (\l.(l (\a.(\b.a)))) : (*->*)
cons | (\h.(\l.(\con.((con h) l)))) : (*->(*->(*->*)))
F | (\y.(\z.z)) : (*->(*->*))
I | (\x.x) : (*->*)
S | (\x.(\y.(\z.((x z) (y z))))) : (*->(*->(*->*)))
K | (\a.(\b.a)) : (*->(*->*))
T | (\a.(\b.a)) : (*->(*->*))

-- Program Scope --
T
K, T
S, K, T
I, S, K, T
F, I, S, K, T
cons, F, I, S, K, T
head, cons, F, I, S, K, T
tail, head, cons, F, I, S, K, T
mylist, tail, head, cons, F, I, S, K, T
mylist, tail, head, cons, F, I, S, K, T
flip, mylist, tail, head, cons, F, I, S, K, T
flip, mylist, tail, head, cons, F, I, S, K, T

-- Program Stack Frames --
(\a.(\b.a)) | (*->(*->*))
(\a.(\b.a)) | (*->(*->*))
(\x.(\y.(\z.((x z) (y z))))) | (*->(*->(*->*)))
(\x.x) | (*->*)
(\y.(\z.z)) | (*->(*->*))
(\h.(\l.(\con.((con h) l)))) | (*->(*->(*->*)))
(\l.(l (\a.(\b.a)))) | (*->*)
(\l.(l (\a.(\b.b)))) | (*->*)
(\con.((con x) (\con.((con y) nil)))) | (*->*)
(((pair of) x) (\con.((con y) nil))) | *
(\a.(\b.(\c.((a c) b)))) | (*->(*->(*->*)))
y | Type

-- Program Output --
Output Length: 1
Result: y
Result Type: Type
```

```haskell
putStrLn $ showLisp $ cenc 100 -- chuch encoding, shown in lisp syntax
(lambda (f) 
    (lambda (x) 
        (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    )
)
```

## Instructions

```haskell
ghci> :l Main
ghci> debug "exprgraph.lc"
```