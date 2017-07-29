--------------------------------------------------------------------------------
title: Discovering Continuations with Typed Holes
published: 2017-05-22
tags: haskell, programming, monads
--------------------------------------------------------------------------------

I've been trying to wrap my head around continuations for a while. I was reading [David Luposchainsky's excellent article](https://github.com/quchen/articles/blob/master/cont_monad.md) on them and playing with his definitions in an IHaskell notebook when I found that typed holes are an excellent hammer to attack this particular nail with.

If you haven't encountered them before, [typed holes](https://wiki.haskell.org/GHC/Typed_holes) are a feature where you put one or more variables starting with `_` on the right hand side of a definition to get GHC to tell you the type of the value that fits in that hole, and you can narrow the hole as necessary to get the type of a subexpression until you have enough information to complete the definition. I like to think of this as a way of collaboratively filling in a definition with the compiler, instead of my usual approach which is to write a definition, listen carefully to GHC's complaints, and amend my definition accordingly. Typed holes are fully supported by GHCi and the full suite of editor integrations, but I personally find the edit/reload/squint cycle more tedious than coming up with the definition in IHaskell and then moving it to a module and adding type signatures after I'm satisfied that it works.

I think his type definition makes an excellent starting point:


```haskell
newtype Cont r a = Cont { (>>-) :: (a -> r) -> r }
```

This defines a type `Cont` with an infix constructor `>>-` (that looks suspiciously similar to `>>=`) that takes a function from `a` to `r` and provides an `r`. One intuition for what this means is that a value of this type knows about an `a` but for whatever reason refuses to be upfront about it and demands to know what you're going to do with it and then does it for you, providing you with a final result `r`. Another intuition is that this is a generalisation of callbacks: a value of this type expects a callback to utilise the `a`. Anyway, on to my favourite part of working with mysterious data types: defining `Functor`, `Applicative`, and `Monad` instances for them! If you've done this before, you'll know that these typeclasses have certain laws that their instances are meant to obey, and it turns out that this type is polymorphic enough that we can just follow the typed holes and the resulting definitions will be lawful. You don't have to take my word for it and should verify this for yourself, but I won't be discussing the laws here. Let's begin!


```haskell
instance Functor (Cont r) where
    fmap f cont = _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:19: error:<br/>    • Found hole: _ :: Cont r b<br/>      Where: ‘r’ is a rigid type variable bound by the instance declaration at &lt;interactive&gt;:1:10<br/>             ‘b’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 fmap :: forall a b. (a -&gt; b) -&gt; Cont r a -&gt; Cont r b<br/>               at &lt;interactive&gt;:2:5<br/>    • In the expression: _<br/>      In an equation for ‘fmap’: fmap f cont = _<br/>      In the instance declaration for ‘Functor (Cont r)’<br/>    • Relevant bindings include<br/>        cont :: Cont r a (bound at &lt;interactive&gt;:2:12)<br/>        f :: a -&gt; b (bound at &lt;interactive&gt;:2:10)<br/>        fmap :: (a -&gt; b) -&gt; Cont r a -&gt; Cont r b (bound at &lt;interactive&gt;:2:5)</pre>


We didn't really need a typed hole to tell us this, but at least we know what we have to work with. We know we have to provide a `Cont` value, so let's narrow our typed hole that way.


```haskell
instance Functor (Cont r) where
    fmap f cont = Cont $ _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:26: error:<br/>    • Found hole: _ :: (b -&gt; r) -&gt; r<br/>      Where: ‘r’ is a rigid type variable bound by the instance declaration at &lt;interactive&gt;:1:10<br/>             ‘b’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 fmap :: forall a b. (a -&gt; b) -&gt; Cont r a -&gt; Cont r b<br/>               at &lt;interactive&gt;:2:5<br/>    • In the second argument of ‘(<span>&dollar;</span>)’, namely ‘_’<br/>      In the expression: Cont <span>&dollar;</span> _<br/>      In an equation for ‘fmap’: fmap f cont = Cont <span>&dollar;</span> _<br/>    • Relevant bindings include<br/>        cont :: Cont r a (bound at &lt;interactive&gt;:2:12)<br/>        f :: a -&gt; b (bound at &lt;interactive&gt;:2:10)<br/>        fmap :: (a -&gt; b) -&gt; Cont r a -&gt; Cont r b (bound at &lt;interactive&gt;:2:5)</pre>


The type of our hole is more helpful here. Now we know (if we were previously uncertain) that we somehow need to use `f` to turn the `a` into a `b`. We also know that `Cont` takes a parameter, let's add that in and see if it helps.


```haskell
instance Functor (Cont r) where
    fmap f cont = Cont $ \k -> _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:32: error:<br/>    • Found hole: _ :: r<br/>      Where: ‘r’ is a rigid type variable bound by the instance declaration at &lt;interactive&gt;:1:10<br/>    • In the expression: _<br/>      In the second argument of ‘(<span>&dollar;</span>)’, namely ‘\ k -&gt; _’<br/>      In the expression: Cont <span>&dollar;</span> \ k -&gt; _<br/>    • Relevant bindings include<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:2:27)<br/>        cont :: Cont r a (bound at &lt;interactive&gt;:2:12)<br/>        f :: a -&gt; b (bound at &lt;interactive&gt;:2:10)<br/>        fmap :: (a -&gt; b) -&gt; Cont r a -&gt; Cont r b (bound at &lt;interactive&gt;:2:5)</pre>


In general, we know all of our definitions will be of the form `Cont $ \k -> _` and that's a safe starting point. We now know that we need to use `k` on the result of applying `f` to some `a` to finally result in an `r`, but where does the `a` come from? The only thing we can do at this point is 'unwrap' the `cont` using `>>-`. What happens when we do that?


```haskell
instance Functor (Cont r) where
    fmap f cont = Cont $ \k -> cont >>- _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:41: error:<br/>    • Found hole: _ :: a -&gt; r<br/>      Where: ‘r’ is a rigid type variable bound by the instance declaration at &lt;interactive&gt;:1:10<br/>             ‘a’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 fmap :: forall a b. (a -&gt; b) -&gt; Cont r a -&gt; Cont r b<br/>               at &lt;interactive&gt;:2:5<br/>    • In the second argument of ‘&gt;&gt;-’, namely ‘_’<br/>      In the expression: cont &gt;&gt;- _<br/>      In the second argument of ‘(<span>&dollar;</span>)’, namely ‘\ k -&gt; cont &gt;&gt;- _’<br/>    • Relevant bindings include<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:2:27)<br/>        cont :: Cont r a (bound at &lt;interactive&gt;:2:12)<br/>        f :: a -&gt; b (bound at &lt;interactive&gt;:2:10)<br/>        fmap :: (a -&gt; b) -&gt; Cont r a -&gt; Cont r b (bound at &lt;interactive&gt;:2:5)</pre>


It looks like we might have everything we need to complete this definition! We can create a function of type `a -> r` by composing `k` and `f`.


```haskell
instance Functor (Cont r) where
    fmap f cont = Cont $ \k -> cont >>- (k . f)
```

It worked! This definition states that `fmap` works by creating a continuation that expects a callback of the new type. This is pretty exciting! Let's continue to `Applicative`.


```haskell
instance Applicative (Cont r) where
    pure a = Cont $ \k -> _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:27: error:<br/>    • Found hole: _ :: r<br/>      Where: ‘r’ is a rigid type variable bound by the instance declaration at &lt;interactive&gt;:1:10<br/>    • In the expression: _<br/>      In the second argument of ‘(<span>&dollar;</span>)’, namely ‘\ k -&gt; _’<br/>      In the expression: Cont <span>&dollar;</span> \ k -&gt; _<br/>    • Relevant bindings include<br/>        k :: a -&gt; r (bound at &lt;interactive&gt;:2:22)<br/>        a :: a (bound at &lt;interactive&gt;:2:10)<br/>        pure :: a -&gt; Cont r a (bound at &lt;interactive&gt;:2:5)</pre>


That was pretty easy. We need an `r` and we have an `a` and a `k` that takes an `a` to an `r`.


```haskell
instance Applicative (Cont r) where
    pure a = Cont $ \k -> k a
```

This matches our intuition from above: creating a continuation involves hiding a value behind a function that can access it. On to `<*>`!


```haskell
instance Applicative (Cont r) where
    pure a  = Cont $ \k -> k a
    f <*> a = Cont $ \k -> _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:3:28: error:<br/>    • Found hole: _ :: r<br/>      Where: ‘r’ is a rigid type variable bound by the instance declaration at &lt;interactive&gt;:1:10<br/>    • In the expression: _<br/>      In the second argument of ‘(<span>&dollar;</span>)’, namely ‘\ k -&gt; _’<br/>      In the expression: Cont <span>&dollar;</span> \ k -&gt; _<br/>    • Relevant bindings include<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:3:23)<br/>        a :: Cont r a (bound at &lt;interactive&gt;:3:11)<br/>        f :: Cont r (a -&gt; b) (bound at &lt;interactive&gt;:3:5)<br/>        (&lt;*&gt;) :: Cont r (a -&gt; b) -&gt; Cont r a -&gt; Cont r b (bound at &lt;interactive&gt;:3:5)</pre>


From above, we know we can 'unwrap' `Cont` values using `>>-`.


```haskell
instance Applicative (Cont r) where
    pure a  = Cont $ \k -> k a
    f <*> a = Cont $ \k -> f >>- _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:3:34: error:<br/>    • Found hole: _ :: (a -&gt; b) -&gt; r<br/>      Where: ‘r’ is a rigid type variable bound by the instance declaration at &lt;interactive&gt;:1:10<br/>             ‘b’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 (&lt;*&gt;) :: forall a b. Cont r (a -&gt; b) -&gt; Cont r a -&gt; Cont r b<br/>               at &lt;interactive&gt;:3:7<br/>             ‘a’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 (&lt;*&gt;) :: forall a b. Cont r (a -&gt; b) -&gt; Cont r a -&gt; Cont r b<br/>               at &lt;interactive&gt;:3:7<br/>    • In the second argument of ‘&gt;&gt;-’, namely ‘_’<br/>      In the expression: f &gt;&gt;- _<br/>      In the second argument of ‘(<span>&dollar;</span>)’, namely ‘\ k -&gt; f &gt;&gt;- _’<br/>    • Relevant bindings include<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:3:23)<br/>        a :: Cont r a (bound at &lt;interactive&gt;:3:11)<br/>        f :: Cont r (a -&gt; b) (bound at &lt;interactive&gt;:3:5)<br/>        (&lt;*&gt;) :: Cont r (a -&gt; b) -&gt; Cont r a -&gt; Cont r b (bound at &lt;interactive&gt;:3:5)</pre>


Let's keep going.


```haskell
instance Applicative (Cont r) where
    pure a  = Cont $ \k -> k a
    f <*> a = Cont $ \k -> f >>- \f' -> a >>- \a' -> _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:3:54: error:<br/>    • Found hole: _ :: r<br/>      Where: ‘r’ is a rigid type variable bound by the instance declaration at &lt;interactive&gt;:1:10<br/>    • In the expression: _<br/>      In the second argument of ‘&gt;&gt;-’, namely ‘\ a' -&gt; _’<br/>      In the expression: a &gt;&gt;- \ a' -&gt; _<br/>    • Relevant bindings include<br/>        a' :: a (bound at &lt;interactive&gt;:3:48)<br/>        f' :: a -&gt; b (bound at &lt;interactive&gt;:3:35)<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:3:23)<br/>        a :: Cont r a (bound at &lt;interactive&gt;:3:11)<br/>        f :: Cont r (a -&gt; b) (bound at &lt;interactive&gt;:3:5)<br/>        (&lt;*&gt;) :: Cont r (a -&gt; b) -&gt; Cont r a -&gt; Cont r b (bound at &lt;interactive&gt;:3:5)</pre>


Perfect, we want an `r` and we have

- an `a` (`a'`)
- a function from `a` to `b` (`f'`)
- a function from `b` to `r` (`k`)

Let's put them together.


```haskell
instance Applicative (Cont r) where
    pure a  = Cont $ \k -> k a
    f <*> a = Cont $ \k -> f >>- \f' -> a >>- \a' -> k (f' a')
```

Okay, we unwrap the function and the argument and rewrap them in a fresh continuation, not too differently from how we defined `fmap`. Sweet! On to the big M!


```haskell
instance Monad (Cont r) where
    a >>= f = Cont $ \k -> _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:28: error:<br/>    • Found hole: _ :: r<br/>      Where: ‘r’ is a rigid type variable bound by the instance declaration at &lt;interactive&gt;:1:10<br/>    • In the expression: _<br/>      In the second argument of ‘(<span>&dollar;</span>)’, namely ‘\ k -&gt; _’<br/>      In the expression: Cont <span>&dollar;</span> \ k -&gt; _<br/>    • Relevant bindings include<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:2:23)<br/>        f :: a -&gt; Cont r b (bound at &lt;interactive&gt;:2:11)<br/>        a :: Cont r a (bound at &lt;interactive&gt;:2:5)<br/>        (&gt;&gt;=) :: Cont r a -&gt; (a -&gt; Cont r b) -&gt; Cont r b (bound at &lt;interactive&gt;:2:5)</pre>


As before, our first order of business is to unwrap the `a`.


```haskell
instance Monad (Cont r) where
    a >>= f = Cont $ \k -> a >>- \a' -> _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:41: error:<br/>    • Found hole: _ :: r<br/>      Where: ‘r’ is a rigid type variable bound by the instance declaration at &lt;interactive&gt;:1:10<br/>    • In the expression: _<br/>      In the second argument of ‘&gt;&gt;-’, namely ‘\ a' -&gt; _’<br/>      In the expression: a &gt;&gt;- \ a' -&gt; _<br/>    • Relevant bindings include<br/>        a' :: a (bound at &lt;interactive&gt;:2:35)<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:2:23)<br/>        f :: a -&gt; Cont r b (bound at &lt;interactive&gt;:2:11)<br/>        a :: Cont r a (bound at &lt;interactive&gt;:2:5)<br/>        (&gt;&gt;=) :: Cont r a -&gt; (a -&gt; Cont r b) -&gt; Cont r b (bound at &lt;interactive&gt;:2:5)</pre>


We can apply `f` to this unwrapped value to get a continuation that we can unwrap again.


```haskell
instance Monad (Cont r) where
    a >>= f = Cont $ \k -> a >>- \a' -> f a' >>- \f' -> _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:57: error:<br/>    • Found hole: _ :: r<br/>      Where: ‘r’ is a rigid type variable bound by the instance declaration at &lt;interactive&gt;:1:10<br/>    • In the expression: _<br/>      In the second argument of ‘&gt;&gt;-’, namely ‘\ f' -&gt; _’<br/>      In the expression: f a' &gt;&gt;- \ f' -&gt; _<br/>    • Relevant bindings include<br/>        f' :: b (bound at &lt;interactive&gt;:2:51)<br/>        a' :: a (bound at &lt;interactive&gt;:2:35)<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:2:23)<br/>        f :: a -&gt; Cont r b (bound at &lt;interactive&gt;:2:11)<br/>        a :: Cont r a (bound at &lt;interactive&gt;:2:5)<br/>        (&gt;&gt;=) :: Cont r a -&gt; (a -&gt; Cont r b) -&gt; Cont r b (bound at &lt;interactive&gt;:2:5)</pre>


We want an `r` and we have `k` and `f'`. Let's put them together!


```haskell
instance Monad (Cont r) where
    a >>= f = Cont $ \k -> a >>- \a' -> f a' >>- \f' -> k f'
```

And that's it! The [mother of all monads](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html) boils down to some tedious and almost mechanical wrapping and unwrapping. I think it's cool how mundane it is.

Let's have a crack at something more involved. A lot of the magic in continuations is accessed via `callCC`, which takes a function and calls it with the current continuation, hence the name. How would we define it?


```haskell
callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b
callCC f = Cont $ \k -> _
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:25: error:<br/>    • Found hole: _ :: r<br/>      Where: ‘r’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 callCC :: forall b r a. ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b<br/>               at &lt;interactive&gt;:1:11<br/>    • In the expression: _<br/>      In the second argument of ‘(<span>&dollar;</span>)’, namely ‘\ k -&gt; _’<br/>      In the expression: Cont <span>&dollar;</span> \ k -&gt; _<br/>    • Relevant bindings include<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:2:20)<br/>        f :: (b -&gt; Cont r a) -&gt; Cont r b (bound at &lt;interactive&gt;:2:8)<br/>        callCC :: ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b (bound at &lt;interactive&gt;:2:1)</pre>


Our definition involves `b`, but the only `b` we have available is wrapped up in `f`. We need to provide an argument of a certain type to `f`, and then unwrap the result of that? Time to bring out the big guns: multiple typed holes!


```haskell
callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b
callCC f = Cont $ \k -> f _1 >>- _2
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:27: error:<br/>    • Found hole: _1 :: b -&gt; Cont r a<br/>      Where: ‘r’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 callCC :: forall b r a. ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b<br/>               at &lt;interactive&gt;:1:11<br/>             ‘a’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 callCC :: forall b r a. ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b<br/>               at &lt;interactive&gt;:1:11<br/>             ‘b’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 callCC :: forall b r a. ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b<br/>               at &lt;interactive&gt;:1:11<br/>      Or perhaps ‘_1’ is mis-spelled, or not in scope<br/>    • In the first argument of ‘f’, namely ‘_1’<br/>      In the first argument of ‘&gt;&gt;-’, namely ‘f _1’<br/>      In the expression: f _1 &gt;&gt;- _2<br/>    • Relevant bindings include<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:2:20)<br/>        f :: (b -&gt; Cont r a) -&gt; Cont r b (bound at &lt;interactive&gt;:2:8)<br/>        callCC :: ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b (bound at &lt;interactive&gt;:2:1)<br/>&lt;interactive&gt;:2:34: error:<br/>    • Found hole: _2 :: b -&gt; r<br/>      Where: ‘r’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 callCC :: forall b r a. ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b<br/>               at &lt;interactive&gt;:1:11<br/>             ‘b’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 callCC :: forall b r a. ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b<br/>               at &lt;interactive&gt;:1:11<br/>      Or perhaps ‘_2’ is mis-spelled, or not in scope<br/>    • In the second argument of ‘&gt;&gt;-’, namely ‘_2’<br/>      In the expression: f _1 &gt;&gt;- _2<br/>      In the second argument of ‘(<span>&dollar;</span>)’, namely ‘\ k -&gt; f _1 &gt;&gt;- _2’<br/>    • Relevant bindings include<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:2:20)<br/>        f :: (b -&gt; Cont r a) -&gt; Cont r b (bound at &lt;interactive&gt;:2:8)<br/>        callCC :: ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b (bound at &lt;interactive&gt;:2:1)</pre>


Great, `k` fits perfectly into the second hole. That was easy.


```haskell
callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b
callCC f = Cont $ \k -> f _ >>- k
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:27: error:<br/>    • Found hole: _ :: b -&gt; Cont r a<br/>      Where: ‘r’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 callCC :: forall b r a. ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b<br/>               at &lt;interactive&gt;:1:11<br/>             ‘a’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 callCC :: forall b r a. ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b<br/>               at &lt;interactive&gt;:1:11<br/>             ‘b’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 callCC :: forall b r a. ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b<br/>               at &lt;interactive&gt;:1:11<br/>    • In the first argument of ‘f’, namely ‘_’<br/>      In the first argument of ‘&gt;&gt;-’, namely ‘f _’<br/>      In the expression: f _ &gt;&gt;- k<br/>    • Relevant bindings include<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:2:20)<br/>        f :: (b -&gt; Cont r a) -&gt; Cont r b (bound at &lt;interactive&gt;:2:8)<br/>        callCC :: ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b (bound at &lt;interactive&gt;:2:1)</pre>


We're being asked to provide a function that takes one argument and returns a continuation. Let's fill in the boilerplate and see where that takes us.


```haskell
callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b
callCC f = Cont $ \k -> f (\b -> Cont $ \k' -> _) >>- k
```


<style>/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
display: block;
padding-bottom: 1.3em;
padding-left: 0.4em;
}
.hoogle-code {
display: block;
font-family: monospace;
white-space: pre;
}
.hoogle-text {
display: block;
}
.hoogle-name {
color: green;
font-weight: bold;
}
.hoogle-head {
font-weight: bold;
}
.hoogle-sub {
display: block;
margin-left: 0.4em;
}
.hoogle-package {
font-weight: bold;
font-style: italic;
}
.hoogle-module {
font-weight: bold;
}
.hoogle-class {
font-weight: bold;
}
.get-type {
color: green;
font-weight: bold;
font-family: monospace;
display: block;
white-space: pre-wrap;
}
.show-type {
color: green;
font-weight: bold;
font-family: monospace;
margin-left: 1em;
}
.mono {
font-family: monospace;
display: block;
}
.err-msg {
color: red;
font-style: italic;
font-family: monospace;
white-space: pre;
display: block;
}
#unshowable {
color: red;
font-weight: bold;
}
.err-msg.in.collapse {
padding-top: 0.7em;
}
.highlight-code {
white-space: pre;
font-family: monospace;
}
.suggestion-warning {
font-weight: bold;
color: rgb(200, 130, 0);
}
.suggestion-error {
font-weight: bold;
color: red;
}
.suggestion-name {
font-weight: bold;
}
</style><pre class='err-msg'>&lt;interactive&gt;:2:48: error:<br/>    • Found hole: _ :: r<br/>      Where: ‘r’ is a rigid type variable bound by<br/>               the type signature for:<br/>                 callCC :: forall b r a. ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b<br/>               at &lt;interactive&gt;:1:11<br/>    • In the expression: _<br/>      In the second argument of ‘(<span>&dollar;</span>)’, namely ‘\ k' -&gt; _’<br/>      In the expression: Cont <span>&dollar;</span> \ k' -&gt; _<br/>    • Relevant bindings include<br/>        k' :: a -&gt; r (bound at &lt;interactive&gt;:2:42)<br/>        b :: b (bound at &lt;interactive&gt;:2:29)<br/>        k :: b -&gt; r (bound at &lt;interactive&gt;:2:20)<br/>        f :: (b -&gt; Cont r a) -&gt; Cont r b (bound at &lt;interactive&gt;:2:8)<br/>        callCC :: ((b -&gt; Cont r a) -&gt; Cont r b) -&gt; Cont r b (bound at &lt;interactive&gt;:2:1)</pre>


And we're done! We can get an `r` by applying `k` to `b`.


```haskell
callCC :: ((b -> Cont r a) -> Cont r b) -> Cont r b
callCC f = Cont $ \k -> f (\b -> Cont $ \k' -> k b) >>- k
```

A closer look at the definition reveals that `k'` is unused, and this function provides `f` with the option to exit early if desired, or continue as normal. There's a good explanation of why and how this works at [the aforementioned article](https://github.com/quchen/articles/blob/master/cont_monad.md#special-api-function-callcc).

Still a bit wary? That's fair. I like to poke at the definitions, [read the source](https://hackage.haskell.org/package/transformers-0.4.3.0/docs/src/Control-Monad-Trans-Cont.html), look at how Gabriel Gonzales [explains it](http://www.haskellforall.com/2014/04/how-continuation-monad-works.html), and have a cup of tea and think about life for a while. Whatever works for you!

If you looked at the source, you might have noticed something interesting: The definition for the ContT monad transformer is identical! Here it is below.


```haskell
newtype ContT r m a = ContT { (>>-) :: (a -> m r) -> m r }

instance Monad m => Functor (ContT r m) where
    fmap f cont = ContT $ \k -> cont >>- (k . f)

instance Monad m => Applicative (ContT r m) where
    pure a  = ContT $ \k -> k a
    f <*> a = ContT $ \k -> f >>- \f' -> a >>- \a' -> k (f' a')

instance Monad m => Monad (ContT r m) where
    a >>= f = ContT $ \k -> a >>- \a' -> f a' >>- \f' -> k f'

callCC :: ((b -> ContT r m a) -> ContT r m b) -> ContT r m b
callCC f = ContT $ \k -> f (\b -> ContT $ \k' -> k b) >>- k
```

I love being able to interact with these definitions like this. This is really how I want to program, and I'd encourage you to try it! The notebook is [here](https://github.com/vaibhavsagar/notebooks/blob/master/continuations/Continuation.ipynb) for you to play with if you have IHaskell set up. IHaskell isn't just useful for programming: I even used it to [write this blog post](https://github.com/vaibhavsagar/notebooks/blob/master/continuations/DiscoveringContinuationsWithTypedHoles.ipynb)!

I feel like I should end with something profound about continuations, but I'll instead link you to [this presentation by Tim Humphries](http://teh.id.au/posts/2017/05/10/lambdajam-slides/index.html) and once again nudge you to try typed holes the next time you're in a Haskell bind (pun very much intended).

Thanks to [Iain McCoy](https://twitter.com/imccoy), [Julia Evans](https://jvns.ca/), and [Carl Factora](https://ivanthetricourne.github.io/) for their feedback and suggestions on this post.
