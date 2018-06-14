--------------------------------------------------------------------------------
title: Trees That Shrink
published: 2018-06-14
tags: haskell, programming
--------------------------------------------------------------------------------

I read [this paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/trees-that-grow.pdf) a while ago and people seemed pretty excited about it, although I couldn't see why. Fortunately, someone posed me an interesting problem recently and in the process of tackling it I believe I know now.

Suppose we have a simple algebraic data type representing the lambda calculus with [de Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index), which are a way of avoiding the messy problem of variable names:


```haskell
data Expr a
    = Lit a
    | Var Int
    | Abs (Expr a)
    | App (Expr a) (Expr a)
    deriving (Show)
```

So far so good! Now we want to have let bindings in this language, and we've decided that we want named variables as well while we're at it because de Bruijn indices are not so much fun to work with. We dutifully define another data type:


```haskell
data Expr' a
    = Lit' a
    | Var' String
    | Abs' (Expr' a)
    | App' (Expr' a) (Expr' a)
    | Let' String (Expr' a) (Expr' a)
    deriving (Show)
```

Let bindings can be easily desugared, which will help us to write a simpler evaluator. However, we'd also like to make sure the desugaring has been implemented correctly, perhaps by converting to some intermediate state where both the name and the correct de Bruijn index coexist peacefully. We have a couple of options, none of which are great:

1. Define a third data type and then write an indexing pass that converts `Var String` to `Var (String, Int)` and then a desugaring pass that converts that to `Expr a`.
2. Work entirely within the bigger data type, forget about indexing, and throw errors whenever a `Let` is encountered after a desugaring pass.
3. Combine the desugaring and indexing passes into one, and forget about keeping track of the desugaring.

Let's implement the third:


```haskell
import qualified Data.Map.Strict as Map

type Env = Map.Map String Int

desugarAndAnonymise :: Env -> Expr' a -> Expr a
desugarAndAnonymise env expr = case expr of
    Lit' a -> Lit a
    Var' name -> Var (env Map.! name)
    Abs' expr' -> let
        env'  = Map.map succ env
        in Abs (desugarAndAnonymise env' expr')
    App' f x -> App (desugarAndAnonymise env f) (desugarAndAnonymise env x)
    Let' n v expr' -> desugarAndAnonymise env (App' (Abs' expr') v)
```

That wasn't a lot of fun to write, I have no idea if I did the conversion from names to indices correctly, and there's no easy way to check if I did.

These problems are (barely) manageable in this case, but what if we want to add more syntax sugar or share this data type with other libraries that have different use cases? We'd either have to write variations on a theme over and over again or say goodbye to type safety. It also becomes harder and harder to decompose our functions into smaller ones that only do one thing. There has to be a better way!

This is the subject of a recent paper called [Trees that Grow](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/trees-that-grow.pdf) by Shayan Najd and Simon Peyton Jones. They noticed the need for this approach when looking at GHC's abstract syntax tree type but the idiom is generally applicable.

The first insight is that defining different data types for each use case is the wrong approach. Conceptually all these data types are the same type, just with different annotations or decoration, and we should be able to define the base data type with enough extensibility to accommodate all the other use cases.

The second insight is that we can use some of GHC's fancier features to ease the pain of working with this ridiculously extensible data type, such as type families and pattern synonyms.

The third insight is that this can be made to work with other language features, such as generalised abstract data types and existentials! We won't use this here, but it's great to know that it's possible.

Let's see how we can use it to solve our problem. The first thing to do is turn on a bunch of language extensions, as with anything moderately fun in Haskell:


```haskell
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
```

The next steps are:

1. Add a type parameter to the data type declaration.
2. Add a field to each constructor with a new data type that uses this parameter (and any others).
3. Add an extra constructor to enable extending this data type, also with a new data type as in step 2.
4. Define type families for each of the new data types you declared in steps 2 and 3.

And we're done! This is what that looks like for our example:


```haskell
data ExpX i a
    = LitX (XLit i a) a
    | VarX (XVar i a)
    | AbsX (XAbs i a) (ExpX i a)
    | AppX (XApp i a) (ExpX i a) (ExpX i a)
    | ExpX (XExp i a)

type family XLit i a
type family XVar i a
type family XAbs i a
type family XApp i a
type family XExp i a
```

The next thing to do is to reconstruct our original data type with no extensions. To do this we'll need to use the [`Data.Void`](http://hackage.haskell.org/package/base/docs/Data-Void.html) package:


```haskell
import Data.Void

void :: Void
void = error "Attempt to evaluate void"
```

Now we can define `ExpUD` (UD for undecorated) using `Int` for our `Var` like we originally wanted and `Void` for all other extension points. It might get frustrating to construct and work with these values by hand, so we can use pattern synonyms to ease this pain!


```haskell
type ExpUD a = ExpX UD a
data UD
type instance XLit UD a = Void
type instance XVar UD a = Int
type instance XAbs UD a = Void
type instance XApp UD a = Void
type instance XExp UD a = Void

pattern LitUD :: a -> ExpUD a
pattern LitUD a <- LitX _ a
    where LitUD a = LitX void a
pattern VarUD :: Int -> ExpUD a
pattern VarUD i <- VarX i
    where VarUD i = VarX i
pattern AbsUD :: ExpUD a -> ExpUD a
pattern AbsUD a <- AbsX _ a
    where AbsUD a = AbsX void a
pattern AppUD :: ExpUD a -> ExpUD a -> ExpUD a
pattern AppUD f a <- AppX _ f a
    where AppUD f a = AppX void f a
```

This is unfortunately a bit boilerplate-y, but now we can define that intermediate data type that uses named variables as well as de Bruijn indices! As a bonus, we can now have named parameters to lambdas, which would not have been possible with the approach we tried to use above.


```haskell
type ExpAnn a = ExpX Ann a
data Ann
type instance XLit Ann a = Void
type instance XVar Ann a = (String, Int)
type instance XAbs Ann a = String
type instance XApp Ann a = Void
type instance XExp Ann a = Void

pattern LitAnn :: a -> ExpAnn a
pattern LitAnn a <- LitX _ a
    where LitAnn a = LitX void a
pattern VarAnn :: String -> Int -> ExpAnn a
pattern VarAnn s i <- VarX (s,i)
    where VarAnn s i = VarX (s, i)
pattern AbsAnn :: String -> ExpAnn a -> ExpAnn a
pattern AbsAnn s a <- AbsX s a
    where AbsAnn s a = AbsX s a
pattern AppAnn :: ExpAnn a -> ExpAnn a -> ExpAnn a
pattern AppAnn f a <- AppX _ f a
    where AppAnn f a = AppX void f a
```

Now for my favourite part, where we add let bindings! We achieve this by using a tuple to represent `let <name> = <expr> in <expr>` as `(<name>, <expr>, <expr>)`, and we use named variables and parameters in this representation.


```haskell
type ExpLet a = ExpX Let a
data Let
type instance XLit Let a = Void
type instance XVar Let a = String
type instance XAbs Let a = String
type instance XApp Let a = Void
type instance XExp Let a = (String, ExpLet a, ExpLet a)

pattern LitLet :: a -> ExpLet a
pattern LitLet a <- LitX _ a
    where LitLet a = LitX void a
pattern VarLet :: String -> ExpLet a
pattern VarLet s <- VarX s
    where VarLet s = VarX s
pattern AbsLet :: String -> ExpLet a -> ExpLet a
pattern AbsLet s a <- AbsX s a
    where AbsLet s a = AbsX s a
pattern AppLet :: ExpLet a -> ExpLet a -> ExpLet a
pattern AppLet f a <- AppX _ f a
    where AppLet f a = AppX void f a
pattern LetLet n v e <- ExpX (n,v,e)
```

Now we can write a desugarer that preserves names and rewrites our let bindings as follows:

```
let <n> = <x> in <y> <=> (\n -> y) x
```


```haskell
desugar :: Env -> ExpLet a -> ExpAnn a
desugar env expr = case expr of
    LitLet a -> LitAnn a
    VarLet name -> VarAnn name (env Map.! name)
    AbsLet name expr' -> let
        env'  = Map.map succ env
        env'' = Map.insert name 0 env'
        in AbsAnn name (desugar env'' expr')
    AppLet f x -> AppAnn (desugar env f) (desugar env x)
    LetLet n v expr' -> desugar env (AppLet (AbsLet n expr') v)
```

We can also write an anonymiser that throws the names away:


```haskell
anonymise :: ExpAnn a -> ExpUD a
anonymise expr = case expr of
    LitAnn a -> LitUD a
    VarAnn _ i -> VarUD i
    AbsAnn _ e -> AbsUD (anonymise e)
    AppAnn f x -> AppUD (anonymise f) (anonymise x)
```

And finally an evaluator that operates on undecorated expressions:


```haskell
eval :: [a] -> ExpUD a -> a
eval env expr = case expr of
    LitUD a -> a
    VarUD i -> env !! i
    AbsUD f -> eval env f
    AppUD f x -> let
        x' = eval env x
        in eval (x':env) f
```

Let's see it in action!


```haskell
identity = AbsLet "i" (VarLet "i")
konst = (AbsLet "x" (AbsLet "y" (VarLet "x")))

eval [] . anonymise . desugar Map.empty $ AppLet (AppLet konst (LitLet 1)) (LitLet 2)
```


    2


Awesome! We have composable compiler passes that are easier to write and to think about. Even with this small example, I hope the benefits are clear.

Thanks to [Andy Chu](http://andychu.net/) and [Peter Bhat Harkins](https://push.cx/) for comments and feedback.
