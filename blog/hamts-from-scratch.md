--------------------------------------------------------------------------------
title: HAMTs from Scratch
published: 2018-07-29
tags: programming, haskell
--------------------------------------------------------------------------------

_This blog post is also an [IHaskell notebook](https://github.com/vaibhavsagar/notebooks/blob/master/hamt/HAMTsFromScratch.ipynb) and the source is available [separately](https://github.com/vaibhavsagar/notebooks/tree/master/hamt/hamt)._

I wanted an explanation for HAMTs (Hash Array Mapped Tries) that was more detailed than [Marek Majkowski's introduction](https://idea.popcount.org/2012-07-25-introduction-to-hamt/) and more approachable than [_Ideal Hash Trees_ by Phil Bagwell](https://lampwww.epfl.ch/papers/idealhashtrees.pdf), the paper that introduced them. If you haven't heard of them before, HAMTs are a way of efficiently representing a hashtable as a [trie](https://en.wikipedia.org/wiki/Trie), and although they were first envisioned as a mutable data structure they are easily adapted to work as a [persistent data structure](https://en.wikipedia.org/wiki/Persistent_data_structure). They form the backbone of the [`unordered-containers`](http://hackage.haskell.org/package/unordered-containers) library but the [implementation has been lovingly optimised](https://github.com/tibbe/unordered-containers/blob/efa43a2ab09dc6eb72893d12676a8e188cb4ca63/Data/HashMap/Base.hs) to the point where I found it impenetrable. [Edward Z. Yang's implementation](https://github.com/ezyang/hamt/blob/a43559795630980eb16ab832a003d8e6acd21cf6/HAMT.hs) is much easier to follow and after adapting it I think I'm in a good place to provide my own take on them.

Let's start with a few imports! I'll be using these packages:

- [`base`](http://hackage.haskell.org/package/base)
- [`bytestring`](http://hackage.haskell.org/package/bytestring)
- [`memory`](http://hackage.haskell.org/package/memory)
- [`pretty-show`](http://hackage.haskell.org/package/pretty-show)
- [`timeit`](http://hackage.haskell.org/package/timeit)
- [`vector`](http://hackage.haskell.org/package/vector)


```haskell
import Data.Bits             (Bits (bit, complement, popCount, shiftR, (.&.), (.|.)),
                              FiniteBits (finiteBitSize))
import Data.ByteArray.Hash   (FnvHash32 (..), fnv1Hash)
import Data.ByteString.Char8 (pack)
import Data.Char             (intToDigit)
import Data.Semigroup        ((<>))
import Data.Vector           (Vector, drop, singleton, take, (!), (//))
import Data.Word             (Word16, Word32)
import Numeric               (showIntAtBase)
import Prelude               hiding (drop, lookup, take)
import System.TimeIt         (timeIt)
import Text.Show.Pretty      (pPrint)
```

We're going to be doing some bit twiddling. To make this easier to follow I'm going to define a `newtype` whose `Show` instance displays the binary representation.


```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Binary a = Binary a
    deriving (Enum, Ord, Real, Integral, Eq, Num, Bits, FiniteBits)

instance (FiniteBits a, Show a, Integral a) => Show (Binary a) where
    show (Binary n) = let
        str = showIntAtBase 2 intToDigit n ""
        size = finiteBitSize n
        in replicate (size - length str) '0' <> str
```

Using this `newtype` we can turn this:


```haskell
24732 :: Word16
```


    24732


into this:


```haskell
24732 :: Binary Word16
```


    0110000010011100


I'm going to use 32-bit hashes (because they're more convenient to display than 64-bit ones) and 16-bit bitmaps. 


```haskell
type Hash = Binary Word32
type Bitmap = Binary Word16
```

The width of bitmaps is $2^n$ where $n$ is the number of bits of the hash that we use at each level of the tree (more on this below). I'm setting $n=4$ which is what `unordered-containers` uses (as of this writing), but we could e.g. set $n=5$ and use 32-bit bitmaps if we wanted. 


```haskell
bitsPerSubkey :: Int
bitsPerSubkey = 4
```

`Shift` is a multiple of $n$ that we will use to focus on the correct part of the hash.


```haskell
type Shift = Int
```

I'm also going to define a `Hashable` class to decouple the choice of a hash function from the implementation of `HAMT`.


```haskell
class Hashable a where
    hash :: a -> Hash
```

For convenience, we'll use the FNV-1 hash function with strings.


```haskell
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

instance Hashable String where
    hash s = let
        FnvHash32 h = fnv1Hash (pack s)
        in Binary h
```

Here's what it looks like in practice.


```haskell
hash "1" :: Binary Word32
```


    00000101000011000101110100101110


A HAMT can be

- empty (`None`)
- a leaf node with the hash, the key, and the value (`Leaf`)
- a node with a bitmap and a (non-empty) vector of child HAMTs (`Many`)

I've chosen to ignore the possibility of collisions, but we could handle them by adding an extra constructor, e.g. `Colliding` with a hash and a vector of key-value pairs.


```haskell
data HAMT key value
    = None
    | Leaf Hash key value
    | Many Bitmap (Vector (HAMT key value))
    deriving (Show)

empty :: HAMT k v
empty = None
```

We'll need some helper functions for vectors:

- `insertAt` inserts an element at a specified index, shifting elements to the right forwards
- `updateAt` replaces an element at a specified index with a new element
- `deleteAt` removes an element at an index, shifting elements to the right backwards


```haskell
insertAt :: Vector a -> Int -> a -> Vector a
insertAt vector index a = take index vector <> singleton a <> drop index vector

updateAt :: Vector a -> Int -> a -> Vector a
updateAt vector index a = vector // [(index, a)]

deleteAt :: Vector a -> Int -> Vector a
deleteAt vector index = take index vector <> drop (index+1) vector
```

### Insert

I think the bit manipulation functions are crucial to understanding what's going on, so I'm going to motivate them by trying to define `insert` without them and coming up with them as they are needed. This initial definition won't be quite right so I'll call it `insert_` to differentiate it from the correct `insert'` function I present later. The type signature for `insert_` is

```haskell
insert_ :: Hash -> key -> value -> HAMT key value -> HAMT key value
```

Inserting a key-value pair into an empty HAMT gives us a single leaf node:

```haskell
insert_ hash key value None = Leaf hash key value
```

Inserting a key-value pair into a single leaf node where the hashes match gives us an updated leaf node (because we're pretending collisions don't exist):

```haskell
insert_ hash key value (Leaf leafHash leafKey leafValue)
    | hash == leafHash = Leaf hash key value
```

Inserting into a HAMT consisting of a single leaf node where the hashes don't match upgrades that leaf node to a `Many` node and inserts the key-value pair into that `Many` node:

```haskell
insert_ hash key value leaf@(Leaf leafHash leafKey leafValue)
    | hash /= leafHash = insert_ key value (Many someBitmap (singleton leaf))
    where someBitmap = undefined
```

#### Bit Masking

Where does `someBitmap` come from? Time for an example! Let's start with a `Leaf (hash "1") "1" 1`:


```haskell
h = hash "1"
leaf = Leaf h "1" 1

leaf
```


    Leaf 00000101000011000101110100101110 "1" 1


`someBitMap` is a 16-bit bitmap where the number of bits set (the `popCount`) is the length of the vector, which in this case is $1$. We want to set one bit, but which bit? We carve off the last $n$ bits using a mask:


```haskell
subkeyMask :: Bitmap
subkeyMask = (bit bitsPerSubkey) - 1

subkeyMask
```


    0000000000001111



```haskell
--     0101110100101110
-- .&. 0000000000001111
-----------------------
--     0000000000001110

fragment = fromIntegral h .&. subkeyMask

fragment
```


    0000000000001110


Then we interpret that fragment as a number:


```haskell
Binary position = fragment

position
```


    14


Finally, we set that bit and we have our bitmap:


```haskell
someBitmap :: Bitmap
someBitmap = Binary $ bit $ fromIntegral position

someBitmap
```


    0100000000000000


We're going to be doing this a lot, so I'll define this as `bitMask_`. The extra `_` is because it isn't quite right for the same reason as `insert_`:


```haskell
bitMask_ :: Hash -> Bitmap
bitMask_ hash = let
    fragment = fromIntegral hash .&. subkeyMask
    Binary position = fragment
    in Binary (bit (fromIntegral position))
```

Let's look at the `Many` case. If we try inserting into a node where the bit in the bitmap corresponding to the mask is `0`, this means that there is an empty slot in the vector. We can insert a leaf node into this slot and set the corresponding bit in the bitmap to `1`:

```haskell
insert_ hash key value (Many bitmap vector)
    | bitmap .&. mask == 0 = let
        leaf = Leaf (hash key) key value
        vector' = insertAt vector index leaf
        bitmap' = bitmap .|. mask
        in Many bitmap' vector'
    where
        mask = bitMask_ hash
        index = undefined
```

#### Mask Indexing

What `index` do we use? This is where `popCount` makes an appearance. Let's demonstrate by inserting `("10", 2)` into our example. First we get the mask corresponding to `hash "10"`:


```haskell
mask = bitMask_ (hash "10")

mask
```


    0000010000000000


Next we want to find the number of lower bits that have been set. We use `mask - 1` as a mask:


```haskell
mask - 1
```


    0000001111111111



```haskell
--     0100000000000000
-- .&. 0000001111111111
-----------------------
--     0000000000000000

masked = someBitmap .&. (mask - 1)

masked
```


    0000000000000000


Then we count the number of bits set with `popCount`:


```haskell
index = popCount masked

index
```


    0


And this is the index we need to insert at! We'll call this `maskIndex`:


```haskell
maskIndex :: Bitmap -> Bitmap -> Int
maskIndex bitmap mask = popCount (bitmap .&. (mask - 1))
```

The final case is where the bit in the bitmap is already set. We need to recursively update the HAMT at the corresponding index:

```haskell
insert_ hash key value (Many bitmap vector)
    | bitmap .&. mask == 1 = let
        subtree' = insert_ hash key value (vector ! index) -- WRONG!
        vector' = updateAt vector index subtree'
        in Many bitmap vector'
    where
        mask = bitMask_ hash
        index = maskIndex bitmap mask
```

But this definition is wrong, because instead of carving off the last $n$ bits of `hash`, we want to recursively carve off the next $n$ bits!

#### Shifting

This is what's missing from our definition, a `shift` parameter corresponding to how far up the `hash` we're looking. This is why we defined `Shift` above. Taking this extra parameter into account, our bit manipulation functions now become:


```haskell
subkeyMask :: Bitmap
subkeyMask = (bit bitsPerSubkey) - 1

maskIndex :: Bitmap -> Bitmap -> Int
maskIndex bitmap mask = popCount (bitmap .&. (mask - 1))

subkey :: Hash -> Shift -> Int
subkey hash shift = fromIntegral $ (fromIntegral $ shiftR hash shift) .&. subkeyMask

bitMask :: Hash -> Shift -> Bitmap
bitMask hash shift = bit (subkey hash shift)
```

We plumb through this `shift` parameter, only modifying it in the final case, to give us the correct definitions of `insert'` and `insert`:


```haskell
insert :: Hashable key => key -> value -> HAMT key value -> HAMT key value
insert key value hamt = insert' 0 (hash key) key value hamt

insert' :: Shift -> Hash -> key -> value -> HAMT key value -> HAMT key value
insert' shift hash key value None = Leaf hash key value

insert' shift hash key value leaf@(Leaf leafHash leafKey leafValue)
    | hash == leafHash = Leaf hash key value
    | otherwise = insert' shift hash key value (Many (bitMask leafHash shift) (singleton leaf))

insert' shift hash key value (Many bitmap vector)
    | bitmap .&. mask == 0 = let
        leaf = Leaf hash key value
        vector' = insertAt vector index leaf
        bitmap' = bitmap .|. mask
        in Many bitmap' vector'
    | otherwise = let
        subtree = vector ! index
        subtree' = insert' (shift+bitsPerSubkey) hash key value subtree
        vector' = updateAt vector index subtree'
        in Many bitmap vector'
    where
        mask = bitMask hash shift
        index = maskIndex bitmap mask
```

Now we can construct HAMTs and inspect them! I'll define a `fromList` function and use `pPrint` from `pretty-show` to highlight the tree structure:


```haskell
fromList :: Hashable key => [(key, value)] -> HAMT key value
fromList = foldr (uncurry insert) empty

example = fromList [("1", 1), ("10", 2), ("100", 3), ("1000", 4)]

pPrint example
```


    Many
      0100010000000000
      [ Many
          0000000100100000
          [ Leaf 00100000011101101010111101011010 "10" 2
          , Leaf 10001010111100101011011010001010 "1000" 4
          ]
      , Many
          0000001000000100
          [ Leaf 00000101000011000101110100101110 "1" 1
          , Leaf 01110100110101100000101010011110 "100" 3
          ]
      ]


### Lookup

Compared to `insert`, `lookup` is a walk in the park. It's implemented along the same lines as `insert`:

- on `None` nodes, it fails
- on `Leaf` nodes, it succeeds if the hashes match
- on `Many` nodes, it fails if the bit isn't set, and recurses into the child node otherwise


```haskell
lookup :: Hashable key => key -> HAMT key value -> Maybe value
lookup key hamt = lookup' 0 (hash key) hamt

lookup' :: Shift -> Hash -> HAMT key value -> Maybe value
lookup' shift hash None = Nothing

lookup' shift hash (Leaf leafHash leafKey leafValue)
    | hash == leafHash = Just leafValue
    | otherwise = Nothing

lookup' shift hash (Many bitmap vector)
    | bitmap .&. mask == 0 = Nothing
    | otherwise = lookup' (shift+bitsPerSubkey) hash (vector ! index)
    where
        mask = bitMask hash shift
        index = maskIndex bitmap mask
```

Let's quickly confirm that it works.


```haskell
lookup "100" example
```


    Just 3


#### Memoising Fibonacci

We now have enough of an API to use this as a hashtable! Let's use it to memoise the calculation of the Fibonacci sequence. The naive implementation does a lot of unnecessary recomputation:


```haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

timeIt $ print $ fib 30
```


    1346269
    CPU time:   1.31s


We can memoise it by storing previously calculated results and using them if they are available:


```haskell
instance Hashable Int where
    hash int = Binary (fromIntegral int)

fib' :: HAMT Int Int -> Int -> (Int, HAMT Int Int)
fib' table 0 = (1, insert 0 1 table)
fib' table 1 = (1, insert 1 1 table)
fib' table n = case lookup n table of
    Just i -> (i, table)
    Nothing -> let
        (i1, table')  = fib' table  (n-1)
        (i2, table'') = fib' table' (n-2)
        in (i1 + i2, insert n (i1 + i2) table'')

fib :: Int -> Int
fib n = fst $ fib' empty n

timeIt $ print $ fib 30
```


    1346269
    CPU time:   0.00s


### Delete

Finally we come to `delete`, which is only a little more complex than `lookup`. It needs to make sure that no `Many` node has a child `None` node, so if a `None` node:

- is an only child, it will replace the parent node
- has any sibling nodes, it will be removed from the parent node's bitmap and vector


`Leaf` nodes similarly replace their parents if they are the only child.


```haskell
delete :: Hashable key => key -> HAMT key value -> HAMT key value
delete key hamt = delete' 0 (hash key) hamt

delete' :: Shift -> Hash -> HAMT key value -> HAMT key value
delete' shift hash None = None

delete' shift hash leaf@(Leaf leafHash leafKey leafValue)
    | hash == leafHash = None
    | otherwise = leaf

delete' shift hash many@(Many bitmap vector)
    | bitmap .&. mask == 0 = many
    | otherwise = let
        subtree = vector ! index
        subtree' = delete' (shift+bitsPerSubkey) hash subtree
        in case subtree' of
            None -> if length vector == 1
                then None
                else Many (bitmap .&. complement mask) (deleteAt vector index)
            Leaf{} -> if length vector == 1
                then subtree'
                else  Many bitmap (updateAt vector index subtree')
            Many{} -> Many bitmap (updateAt vector index subtree')
    where
        mask = bitMask hash shift
        index = maskIndex bitmap mask
```

Let's see this in action.


```haskell
pPrint $ delete "1000" example
```


    Many
      0100010000000000
      [ Many
          0000000000100000 [ Leaf 00100000011101101010111101011010 "10" 2 ]
      , Many
          0000001000000100
          [ Leaf 00000101000011000101110100101110 "1" 1
          , Leaf 01110100110101100000101010011110 "100" 3
          ]
      ]


It's possible to have a situation where we have a `Many` node with only one child, because our replacement behaviour checks the length of the vector before any elements are removed from it. However, removing the last leaf will correctly delete the parent node.


```haskell
pPrint $ delete "10" $ delete "1000" example
```


    Many
      0100000000000000
      [ Many
          0000001000000100
          [ Leaf 00000101000011000101110100101110 "1" 1
          , Leaf 01110100110101100000101010011110 "100" 3
          ]
      ]


And we're done! I hope you understand HAMTs better than when you started reading this.

If you want to use this for something other than educational purposes, I would recommend adding logic to deal with hash collisions, which I intentionally omitted. There's also some low-hanging fruit in terms of performance optimisations. The first thing that comes to mind is an additional `Full` constructor for the case where all bits in the bitmap are set, and the next thing is the use of unsafe vector functions that omit bounds checking.

Thanks to [Evan Borden](https://twitter.com/evanborden), [Javier Candeira](https://twitter.com/candeira), [Jean Niklas L'orange](https://hypirion.com/), [Mark Hopkins](http://mjhopkins.github.io/), and [Tim Humphries](https://teh.id.au/) for comments and feedback.
