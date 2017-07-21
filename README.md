# json-tracer
Type-safe polymorphic json-structured tracing library

[https://hackage.haskell.org/package/json-tracer]

This library provides two modules
- `Data.PolyDict`: type-safe, polymorphic, and json-structured dictionary
- `Control.Monad.CTrace`: a monad that enables contextual tracing

# PolyDict
PolyDict is a hash dict like JSON, but it is typed.
`Dict n` is a dictinary whose fields are typed accoding to `Assoc n`. 
That is, each field has type `Key k` (which is a proxy of type-level symbol `k`) and 
`Assoc n k` is the type of values associated with the key. 

Basically, users define a `data` that represents the namespace of the `Dict`.
For example:
```haskell
data Main
```

Then, one can define the type of `Dict n` by adding rules for type family `Assoc n k` like this:
```haskell
type instance Assoc Main "elapsed_time" = NominalDiffTime
type instance Assoc Main "tag" = String
```

The RHS type of the `Assoc n k` must satisfy the `DictValue v` constraint.
```hakell
type family DictValue v :: Constraint where
    DictValue v = (Eq v, Show v, ToJSON v)
```
As far as the author knows, any `ToJSON v` value satisfy this constraint.

Note: `Dict n` is allowed as the RHS type as `Dict n` satisfies the `DictValue` constraint. Hence recursive structures can be handled.

Since the definition of type family is open, users don't have to define all rules at the same module.
It's totally free to add other fields on demand, as long as there are no conflicting keys.
When such confliction occurs, the compiler reports it as an error.
```haskell
type instance Assoc Main "tag" = Int 
-- this would be compile error because the key "tag" is conflicting to the previous definition.
```

Values in `Dict` are obtained and updated by `lookup` and `insert` function.
```haskell
lookup :: (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> Dict n -> Maybe v
insert :: (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> v -> Dict n -> Dict n
```
With the `OverloadedLabels` extention, user can write `#foo` as the key for the field `"foo"`.

## Examples
```haskell
ghci> let v = insert #tag "sample" empty 
ghci> v
{"tag": "sample"}
ghci> lookup #tag v
Just "sample"
ghci> lookup #elapsed_time v
Nothing
```

Instead, lenses can be used to access thouse fields with `access` function.
```haskell
access  :: forall n k v. (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> Lens' (Dict n) (Maybe v)
access' :: forall n k v. (KnownSymbol k, DictValue v, Assoc n k ~ v) => Key k -> v -> Lens' (Dict n) v
```

## Examples
```haskell
ghci> let v = empty & access #tag ?~ "sample"
ghci> v
{"tag": "sample"}
ghci> v ^. access #tag
Just "sample"
```

# Tracer Monad
`TracerT c m a` is the type of a monad transformer that enables contextual tracing.
`update` and `zoom` operations can be performed in this monad transformer.

`update` is the action to modifies the value of context.
```haskell
update :: Monad m => (c -> c) -> TracerT c m ()
```
For example, you can count the number of calls of function `f` 
by inserting `update succ :: TracerT Int m ()` for each call of `f`.

Note: although you can modify the value, you cannot get the current value in this monad. 
This is intentional, to make it easy to disable tracing.

`zoom` is the action to change the context of tracing.
```haskell
zoom :: ASetter' c c' -> TracerT c' m a -> TracerT c m a
```

# Complete Example
```haskell
{-# LANGUAGE TypeFamilies, DataKinds, OverloadedLabels #-}
import Data.PolyDict
import Control.Monad.CTrace
import Lens.Micro
import Control.Monad

data Main
data Sub

type instance Assoc Main "sub" = Dict Sub
type instance Assoc Sub  "count" = Int

subFunc :: Monad m => Int -> TracerT (Dict Sub) m ()
subFunc n = replicateM_ n (update (access' #count 0 %~ succ))

mainFunc :: Monad m => TracerT (Dict Main) m ()
mainFunc = zoom (access' #sub empty) (subFunc 42)

main :: IO ()
main = do
    (_,d) <- ioTracerT empty mainFunc
    print d
-- > {"sub": {"count": 42}}
```
