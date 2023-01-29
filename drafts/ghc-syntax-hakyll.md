--------------------------------------------------------------------------------
title: Using `ghc-syntax-highlighter` with Hakyll
published: 2023-01-29
tags: haskell, programming
--------------------------------------------------------------------------------

In 2018, [Mark Karpov announced
`ghc-syntax-highlighter`](https://markkarpov.com/post/announcing-ghc-syntax-highlighter.html),
a project which uses GHC's own lexer to tokenise Haskell source code for the
best possible syntax highlighting. I thought this was extremely cool, and
really wanted to use it for this blog. Unfortunately, this is what the post had
to say about `pandoc`, which Hakyll uses to process Markdown:

> [`skylighting`](https://hackage.haskell.org/package/skylighting) is what
Pandoc uses btw. And from what I can tell it’s hardcoded to use only that
library for highlighting, so some creativity may be necessary to get it work.

I briefly looked into this and reached the same conclusion (and as of this
writing [it is still the
case](https://github.com/jgm/pandoc/blob/5f31a01d77f5fea46e2deca51165d5af8fc99677/src/Text/Pandoc/Highlighting.hs#L76-L106))
so, as a deeply uncreative individual, I sighed deeply and resigned myself to never knowing this particular joy.

Until, just a few days ago, I read [this lovely blog post by Tony Zorman about
customising Hakyll's syntax
highlighting](https://tony-zorman.com/posts/2023-01-21-pygmentising-hakyll.html)
which included this gem of a sentence in the very first paragraph:

> Using `pygmentize` as an example, I will show you how you can swap out
pandoc’s native syntax highlighting with pretty much any third party tool that
can output HTML.

And in fact this is an accurate description of what follows. This sounds like
exactly what I want to do, and between this and Mark's
[`mmark-ext`](https://hackage.haskell.org/package/mmark-ext) (which [implements
`ghc-syntax-highlighter` support as an extension for
`mmark`](https://hackage.haskell.org/package/mmark-ext-0.2.1.5/docs/Text-MMark-Extension-GhcSyntaxHighlighter.html))
I was able to successfully follow the instructions to get
`ghc-syntax-highlighter` working with my blog. Let me walk you through what
I did.

Here are the language extensions I will be using:

```haskell
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
```

and these additional imports:

```haskell
import           GHC.SyntaxHighlighter (Token(..), tokenizeHaskell)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Pandoc.Definition (Block (CodeBlock, RawBlock), Pandoc)
import           Text.Pandoc.Walk (walk)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
```

I chose to use [`blaze-html`](https://hackage.haskell.org/package/blaze-html)
since it is already a transitive dependency of `pandoc` and using it has no
impact on our dependency tree.

Tony uses `walkM` since an external program (`pygmentize`) is involved, but
since we are working with pure Haskell code we can get away with just `walk`:

```haskell
ghcSyntaxHighlight :: Pandoc -> Pandoc
ghcSyntaxHighlight = walk $ \case
    CodeBlock (_, (isHaskell -> True):_, _) (tokenizeHaskell -> Just tokens) ->
        RawBlock "html" . L.toStrict . renderHtml $ formatHaskellTokens tokens
    block -> block
    where isHaskell = (== "haskell")
```

This only matches Haskell code blocks which `tokenizeHaskell` is able to
successfully tokenise and otherwise falls back on existing `pandoc` behaviour.

`formatHaskellTokens` generates markup very similarly to what `pandoc` already
does:

```haskell
formatHaskellTokens :: [(Token, T.Text)] -> H.Html
formatHaskellTokens tokens =
    H.div H.! A.class_ "sourceCode" $
        H.pre H.! A.class_ "sourceCode haskell" $
            H.code H.! A.class_ "sourceCode haskell" $
                mapM_ tokenToHtml tokens
```

`tokenizeHaskell` produces a list of pairs of the token type (`KeywordToken`,
`VariableToken`, etc.) and the matched text, and the `tokenToHtml` ([adapted
from
`mmark-ext`](https://hackage.haskell.org/package/mmark-ext-0.2.1.5/docs/src/Text.MMark.Extension.GhcSyntaxHighlighter.html#tokenToHtml))
function creates a `span` element with the appropriate class name for our CSS
to style:

```haskell
tokenToHtml :: (Token, T.Text) -> H.Html
tokenToHtml (tokenClass -> className, text) =
    H.span H.!? (not $ T.null className, A.class_ (H.toValue className)) $
        H.toHtml text
```

`tokenClass` ([also adapted from
`mmark-ext`](https://hackage.haskell.org/package/mmark-ext-0.2.1.5/docs/src/Text.MMark.Extension.GhcSyntaxHighlighter.html#tokenClass))
outputs the appropriate class name for each token, and I made only minor
changes for styling purposes:

```haskell
tokenClass :: Token -> T.Text
tokenClass = \case
  KeywordTok -> "kw"
  PragmaTok -> "pp" -- Preprocessor
  SymbolTok -> "ot" -- Other
  VariableTok -> "va"
  ConstructorTok -> "dt" -- DataType
  OperatorTok -> "op"
  CharTok -> "ch"
  StringTok -> "st"
  IntegerTok -> "dv" -- DecVal
  RationalTok -> "dv" -- DecVal
  CommentTok -> "co"
  SpaceTok -> ""
  OtherTok -> "ot"
```

Finally we have to actually use `ghcSyntaxHighlight`, for which we define
a replacement for `pandocCompiler` called (imaginatively)
`customPandocCompiler` and use it everywhere:

```haskell
customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
    pandocCompilerWithTransform
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        ghcSyntaxHighlight
```

Again, since we are using pure functions, we can get away with
`pandocCompilerWithTransform` instead of `pandocCompilerWithTransformM`.

And we're done! I also had to tweak my CSS slightly since `pandoc` was
generating a `span` for each line of source code instead of each token like
`ghc-syntax-highlighter` does. For the complete listing, see
[here](https://github.com/vaibhavsagar/website/blob/6415721a318fc8fb31f4aadb7cd40ab6aad4fbc4/site.hs).
