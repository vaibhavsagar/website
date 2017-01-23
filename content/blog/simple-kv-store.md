Title: A Simple Key-Value Store with Servant
Date: 2017-01-17
Category: programming
Status: draft

The [meat of the Servant
tutorial](http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#nested-apis)
starts with an imposing list of language extensions and imports and only gets
more confusing from there. I don't think this gives newbies (i.e. me) the best
first impression of Servant. Let's build the simplest possible key-value store
with it instead.

We're going to write this as a `stack` script so everything is in one file.

```bash
#!/usr/bin/env stack
```

Let's import the modules we need.

```haskell
{- stack --resolver lts-7 --install-ghc runghc
    --package aeson
    --package servant-server
    --package text
    --package transformers
    --package unordered-containers
    --package warp
    -}
```

At minimum we need `servant-server` for Servant goodness and `warp` to actually
run our web service. The plan is to create an `IORef` holding a `HashMap` and
use that as our store, which is why we need `unordered-containers`. I'd like to
store arbitrary JSON, therefore `aeson`, and I think our keys should be `Text`,
because `aeson` and `Text` are great together. That leaves `transformers`,
which we need because of `liftIO`.

It turns out that we only need two language extensions for this example.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
```

As far as I can tell, both these extensions are needed for Servant's cute API
specification EDSL. We'll let you have this one, Servant.

Time for a (hopefully manageable) list of imports!

```haskell
module Main where

import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               (Value)
import Data.IORef               (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.HashMap.Strict      (HashMap, lookup, insert, empty)
import Data.Text                (Text)
import Network.Wai.Handler.Warp (run)
import System.Environment       (getArgs)
import Prelude hiding           (lookup)
import Servant
```
All imports are explicit except `Servant`'s.

Speaking of the EDSL:

```haskell
type API
    =    "get" :> Capture "key" Text :> Get '[JSON] (Maybe Value)
    :<|> "put" :> Capture "key" Text
        :> ReqBody '[JSON] Value     :> Put '[JSON] Text
```

This API has two endpoints: a "/get/:key" endpoint that provides the value
associated with a key if the key exists in our store, or a "put/:key" endpoint
that allows us to associate some JSON with a key, returning the key used. How
this fits together is still a bit of a mystery to me, but [this blog
post](http://kseo.github.io/posts/2017-01-20-how-servant%27s-type-safe-links-work.html)
provides the best explanation I've read so far. The section of the Servant
tutorial [on the API specification
EDSL](http://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html) is
also quite good.

Let's define a type synonym so we don't have to keep writing `IORef (HashMap
Text Value)` over and over again:

```haskell
type Store = IORef (HashMap Text Value)
```

Servant uses the same operator to define the type and the serving action:

```haskell
server :: Store -> Server API
server store = getValue store :<|> putValue store
```

The order in which these actions are composed needs to match the order used in
the API definition.

Next we define `getValue` and `putValue`. We need actions of type `Handler`,
which is `ExceptT ServantErr IO`, so we `liftIO` as necessary:

```haskell
getValue :: Store -> Text -> Handler (Maybe Value)
getValue store = liftIO $ lookup key <$> readIORef store

putValue :: Store -> Text -> Value -> Handler Text
putValue store key value = liftIO $ atomicModifyIORef' store modify
    where modify kv = (insert key value kv, key)
```

Almost there! We declare the API we want to serve:

```haskell
kvAPI :: Proxy API
kvAPI = Proxy
```

Once again, Kwang Yul Seo has [a great blog
post](http://kseo.github.io/posts/2017-01-15-data-proxy.html) on this.

Finally, we define our entry point:

```haskell
main :: IO ()
main = do
    port <- read . head <$> getArgs :: IO Int
    run port . serve kvAPI . server =<< newIORef empty
```

And we're done!
