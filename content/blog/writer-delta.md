Title: Using The Writer Monad Outside Logging
Date: 2016-07-24
Category: programming

I hadn't given much thought to the Writer monad before today, unlike the Reader and State monads, and part of the reason is that all the tutorials I encountered online focused on using it for logging. Although this is an obvious use case, I haven't had to do much logging in Haskell so far and so I incorrectly assumed that I didn't need to know about the Writer monad at all. However, I found a use for it outside logging and thought this might be worth sharing. This usage depends on two facts about Haskell:

1. The Writer monad's output can be any Monoid, such as strings, lists, sets, etc.
2. ByteString is a Monoid.

Git Packfiles store some entries as deltas against other entries. The delta format is a bit hairy to parse, but it consists of a sequence of either copy or insert instructions. Copy instructions specify an offset in the source and a length of bytes to copy, whereas insert instructions specify a series of bytes to be copied as is.


```haskell
import qualified Data.ByteString as B

data DeltaInstruction
    = CopyInstruction   Int Int
    | InsertInstruction B.ByteString
    deriving (Show)
```

An instruction can be interpreted with reference to a source to result in a ByteString.


```haskell
substring :: Int -> Int -> B.ByteString -> B.ByteString
substring offset length bytestring = B.take length (B.drop offset bytestring)

interpretInstruction :: DeltaInstruction -> B.ByteString -> B.ByteString
interpretInstruction instruction source = case instruction of
    InsertInstruction string        -> string
    CopyInstruction   offset length -> substring offset length source
```

The concatenation of a list of interpreted instructions is a resolved delta.


```haskell
applyInstructions' :: B.ByteString -> B.ByteString -> [DeltaInstruction] -> B.ByteString
applyInstructions' source dest []     = dest
applyInstructions' source dest (i:is) = let
    dest' = B.append dest $ interpretInstruction i source
    in applyInstructions' source dest' is
```

For example:


```haskell
:set -XOverloadedStrings
testSource = "tree 9977d7726a568347a513a6b061b87fd687322650\nparent e05bba28b796e7af7e5d981955327621a6c43a5f\nauthor Vaibhav Sagar <EMAILADDRESS@gmail.com> 1467760597 +0800\ncommitter Vaibhav Sagar <EMAILADDRESS@gmail.com> 1467781641 +0800\n\nUse record wildcards.\n" :: B.ByteString
testInstructions = [InsertInstruction "tree d8dce7c53feb5fcaca4bef87106d3f9e4e50e6e",CopyInstruction 44 167,InsertInstruction "60597",CopyInstruction 216 30]
applyInstructions' testSource B.empty testInstructions
```


    "tree d8dce7c53feb5fcaca4bef87106d3f9e4e50e6e0\nparent e05bba28b796e7af7e5d981955327621a6c43a5f\nauthor Vaibhav Sagar <EMAILADDRESS@gmail.com> 1467760597 +0800\ncommitter Vaibhav Sagar <EMAILADDRESS@gmail.com> 1467760597 +0800\n\nUse record wildcards.\n"


Although this works, it isn't particularly elegant and it would be nice to abstract away the details of the empty ByteString and the append. Luckily, these correspond to the `mempty` and `mappend` methods of a Monoid, and since the Writer monad works with monoids, we might be able to use it. Let's start by defining a monadic action that appends an interpreted instruction and returns the source we fed it.


```haskell
import Control.Monad
import Control.Monad.Trans.Writer

applyInstruction :: DeltaInstruction -> B.ByteString -> Writer B.ByteString B.ByteString
applyInstruction instruction source = writer (source, interpretInstruction instruction source)
```

Our action takes two parameters and so we `map` it to our list of instructions. This gives us a list of actions expecting a source buffer to operate on.


```haskell
:t map applyInstruction testInstructions
```


    map applyInstruction testInstructions :: [ByteString -> Writer ByteString ByteString]


We need to reduce this list to a single value while executing each action in sequence, which implies a fold. `foldM` looks like what we need; according to the documentation:

```
foldM f a1 [x1, x2, ..., xm] ==
do
    a2 <- f a1 x1
    a3 <- f a2 x2
    ...
    f am xm
```

The only wrinkle is that `f` needs to take the argument first and the action second. We can accomplish this by defining

```
f a b = b a
```
or

```
f = flip ($)
```

Putting it all together, we have


```haskell
applyInstructions :: B.ByteString -> [DeltaInstruction] -> B.ByteString
applyInstructions source instructions = execWriter $ foldM (flip ($)) source $ map applyInstruction instructions
applyInstructions testSource testInstructions == applyInstructions' testSource B.empty testInstructions
```


    True


And there you have it, a use of Writer that has absolutely nothing to do with logging!
