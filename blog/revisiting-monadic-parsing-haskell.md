--------------------------------------------------------------------------------
title: Revisiting 'Monadic Parsing in Haskell'
published: 2018-02-04
tags: haskell, programming, monads
--------------------------------------------------------------------------------

_Translated to [Russian](http://clipartmag.com/ru-revisiting-monadic-parsing-haskell) by [Clipart Team](http://clipartmag.com/)_

['Monadic Parsing in Haskell'](http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf) is a short paper that laid the groundwork for libraries like Parsec and Attoparsec. Although it was published in 1998 (almost 20 years ago!) it has aged gracefully and the code samples will run with almost no changes. However, the state of the art has advanced since then and I think the use of modern Haskell can make this material simpler to follow and implement.

Monadic parsing in Haskell is what sold me on all three. Before Haskell my experiences with parsing had involved buggy regexes for lexers and wrangling tools like `bison` and `flex`, and although I'd heard that Haskell was good for parsing I couldn't see how this could be the case when I couldn't find any robust regex libraries! An aside in some documentation pointed me to Attoparsec and when I saw the [example RFC2616 parser](https://github.com/bos/attoparsec/blob/master/examples/RFC2616.hs) it seemed like a magic trick. How could it be so small? After a few weeks of trying it myself I was convinced and I've never looked back. This was the first application of monads I encountered that actually made my life simpler, and I started to realise that there was more to monads than smugness and being inaccessible to newcomers.

The first change I want to make is the type definition. The paper uses the type

```haskell
newtype Parser a = Parser (String -> [(a,String)])
```

and although this is a famous enough definition that it has [its own rhyme](http://www.willamette.edu/~fruehr/haskell/seuss.html), I think the flexibility of lists is wasted here. The authors don't use it, and instead define a 'deterministic choice' operator `(+++)` that gives at most one result and use that everywhere instead. There is already a perfectly good datatype in Haskell for lists of at most one element, `Maybe`, so I'll use that instead of `[]`:

```haskell
newtype Parser a = Parser (String -> Maybe (a, String))
```

If we rename `String` to `s` and `Maybe` to `m`,  a more interesting pattern is revealed:

```haskell
newtype Parser s m a = Parser (s -> m (a, s))
```

This is [`StateT`](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-State-Strict.html#t:StateT)! Recognising this pattern makes instance definitions much easier, so much easier in fact that GHC can do it for us automatically with `-XGeneralizedNewtypeDeriving`! For completeness I will resist the temptation to do this, but you can try it yourself with

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Parser a = Parser (StateT String Maybe a) deriving (Functor, Applicative, Alternative, Monad)
```

The second change is also for completeness: the authors jump straight into the `Monad` instance without defining `Functor` and `Applicative` first. To be fair, the `Applicative` abstraction hadn't been [discovered](http://www.staff.city.ac.uk/~ross/papers/Applicative.html) yet, and this is also the reason why the authors define `mzero` and `mplus` (which they call `(++)`) instead of the more general `Alternative` methods `empty` and `(<|>)`. Because of our `Maybe` change, defining `Alternative` means I won't need to bother with their `(+++)`.

Finally, I'll try to avoid do-notation where possible in favour of a more Applicative style using e.g. `<*>` (which can be pronounced 'splat' if you don't already have a name for it) because most of these parsers don't require it.

Let's begin!


```haskell
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (Alternative(..))
import Control.Monad.Trans.State.Strict
import Control.Monad (guard)
import Data.Char (isSpace, isDigit, ord)
```

For convenience I've defined an `unParser` that unwraps a `Parser a` to its underlying `StateT String Maybe a`.


```haskell
newtype Parser a = Parser { unParser :: StateT String Maybe a }
runParser = runStateT . unParser
```

`fmap` is as simple as unwrapping the `Parser` and using the underlying `StateT`'s `fmap`.


```haskell
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser $ f <$> unParser p
```

More unwrapping for `Applicative` and `Alternative`.

The [`Alternative`](https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Alternative) typeclass allows us to express the idea of running one parser or another parser, resulting in the first successful parse. `empty` handles the case where both parsers fail, and `(<|>)` (which can be pronounced 'alt') performs the alternation. This is convenient enough on its own, but `Alternative` also provides `many` and `some` which correspond exactly to `many` and `many1` from the paper:

```haskell
-- many v = some v <|> pure []
-- some v = (:) <$> v <*> many v
```

but only after replacing `[]` with `Maybe` like I've done here so that `(<|>)` corresponds to `(+++)`. 


```haskell
instance Applicative Parser where
    pure :: a -> Parser a
    pure a  = Parser $ pure a
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    f <*> a = Parser $ unParser f <*> unParser a

instance Alternative Parser where
    empty :: Parser a
    empty   = Parser empty
    (<|>) :: Parser a -> Parser a -> Parser a
    a <|> b = Parser $ unParser a <|> unParser b
```

The `Monad` definition is slightly more interesting, because we have to manually construct the `StateT` value, but this also boils down to unwrapping and rewrapping.


```haskell
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    a >>= f = Parser $ StateT $ \s -> do
        (a', s') <- runParser a s
        runParser (f a') s'
```

Notice that `anyChar` is the only function below that manually constructs a `Parser`, and `satisfy` is the only one that requires the `Monad` interface.


```haskell
anyChar :: Parser Char
anyChar = Parser . StateT $ \s -> case s of
    []     -> empty
    (c:cs) -> pure (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
    c <- anyChar
    guard $ pred c
    pure c

char :: Char -> Parser Char
char = satisfy . (==)

string :: String -> Parser String
string []     = pure []
string (c:cs) = (:) <$> char c <*> string cs
```

Again, `many` and `many1` don't need to be defined because they are provided for free!


```haskell
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)
```

These are almost identical to the definitions in the paper. I've included `chainr` for completeness.


```haskell
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> pure a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
    where 
        rest a = (do
            f <- op
            b <- p
            rest (f a b)) <|> pure a

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op a = (p `chainr1` op) <|> pure a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
    where
        scan   = p >>= rest
        rest a = (do
            f <- op
            b <- scan
            rest (f a b)) <|> pure a
```

The only difference here is the replacement of `(>>)` with `(*>)`. These have the same effect, except that the latter works on `Applicative`s and also comes with a counterpart `(<*)`. When writing parsers I find these especially useful because they allow me to combine multiple parsers together when I only care about the output of one of them, e.g. `ignored *> ignored *> value <* ignored`. The calculator example uses this in `factor`.


```haskell
space :: Parser String
space = many (satisfy isSpace)

token :: Parser a -> Parser a
token p = p <* space

symbol :: String -> Parser String
symbol = token . string

apply :: Parser a -> String -> Maybe (a, String)
apply p = runParser (space *> p)
```

The calculator example is almost unchanged.


```haskell
expr, term, factor, digit :: Parser Int
expr   = term   `chainl1` addop
term   = factor `chainl1` mulop
factor = digit <|> (symbol "(" *> expr <* symbol ")")
digit  = subtract (ord '0') . ord <$> token (satisfy isDigit)

addop, mulop :: Parser (Int -> Int -> Int)
addop = (symbol "+" *> pure (+)) <|> (symbol "-" *> pure (-))
mulop = (symbol "*" *> pure (*)) <|> (symbol "/" *> pure (div))
```

Finally, the payoff!


```haskell
runParser expr "(1 + 2 * 4) / 3 + 5"
```


    Just (8,"")


What have we gained in 20 years? With only minor changes, the code is more composable and uses finer-grained abstractions. For example, if we change our minds about replacing `[]` with `Maybe`, we can switch it back and would only have to update the type signature of `apply`:

```haskell
apply :: Parser a -> String -> [(a, String)]
apply p = runParser (space *> p) -- the implementation stays the same!
```

If we want better error messages, we could use a type such as `Either String` to keep track of locations and error messages. The [`yoctoparsec`](http://hackage.haskell.org/package/yoctoparsec) library takes this even further, allowing to you to choose your own stream type.

Another big difference is the `Applicative` family of functions, which we can leverage whenever we don't have to branch on a previously parsed value (which turns out to be surprisingly often). I'm a huge fan of the `x <$> y <*> z` and the `ignored *> value <* ignored` idioms and I think it's useful to be able to parse this way.

Otherwise, the code is largely the same and I think it's pretty incredible that so little has changed in 20 years! This code is available as an [IHaskell notebook](https://github.com/vaibhavsagar/notebooks/blob/master/revisiting-monadic-parsing-haskell/Parser.ipynb) if you would like to experiment with it yourself.

_Edit: I just found ['From monoids to near-semirings: the essence of `MonadPlus` and `Alternative`'](https://lirias.kuleuven.be/bitstream/123456789/499951/1/main.pdf), which demonstrates how my `Maybe`-based parser doesn't strictly obey the `Alternative` laws. Something to keep in mind if you plan to use it or something like it!_

Thanks to [Alan O'Donnell](https://github.com/cqfd), [Andrey Mokhov](https://blogs.ncl.ac.uk/andreymokhov/), [Annie Cherkaev](https://anniecherkaev.com/), [Julia Evans](https://jvns.ca/), and [Noah Luck Easterly](https://github.com/rampion/) for comments and feedback!
