--------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.Monad ((>=>))
import           Data.Monoid ((<>))
import           Hakyll
import           System.FilePath
import           Data.List (isSuffixOf)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import           GHC.SyntaxHighlighter (Token(..), tokenizeHaskell)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Pandoc.Definition (Block (CodeBlock, RawBlock), Pandoc)
import           Text.Pandoc.Walk (walk)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    matcher "images/*" idRoute copyFileCompiler

    matcher "css/*" idRoute compressCssCompiler

    tags <- buildTags "blog/*" (fromCapture "tags/*")

    let postCtx
            =  tagsField "tags" tags
            <> dateField "date" "%e %B %Y"
            <> defaultContext

    tagsRules tags $ \tag pat -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route   $ prependBlogRoute `composeRoutes` cleanRoute
        compile $ do
            let posts = recentFirst =<< loadAll pat
            let tagsCtx
                    =  constField "title" title
                    <> listField "posts" postCtx posts
                    <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" tagsCtx
                >>= finalise                                  tagsCtx

    matcher "blog/*" (dateRoute `composeRoutes` cleanRoute) $
        customPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"   postCtx
        >>= saveSnapshot "content"
        >>= finalise                                     postCtx

    matcher "drafts/*" cleanRoute $
        customPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= finalise                                   postCtx

    matcher "pages/*" (rootRoute `composeRoutes` cleanRoute) $
        customPandocCompiler >>= finalise defaultContext

    matcher "extra/*" rootRoute copyFileCompiler

    create ["archive/index.html"] $ do
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAll "blog/*"
            let archiveCtx
                    =  listField "posts" postCtx posts
                    <> constField "title" "Archives"
                    <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= finalise                                      archiveCtx

    matcher "index.html" idRoute $ do
        let posts = fmap (take 20) . recentFirst =<< loadAll "blog/*"
        let indexCtx
                =  listField "posts" postCtx posts
                <> constField "title" "Home"
                <> defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= finalise        indexCtx

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "blog/*" "content"
            renderAtom feedConfig feedCtx posts >>= cleanIndexUrls

    where matcher path router compiler =
            match path $ route router >> compile compiler

--------------------------------------------------------------------------------
customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
    pandocCompilerWithTransform
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        ghcSyntaxHighlight

cleanRoute, rootRoute, dateRoute, prependBlogRoute :: Routes
cleanRoute = customRoute createIndexRoute
    where createIndexRoute (toFilePath -> p) =
            takeDirectory p </> takeBaseName p </> "index.html"

rootRoute = customRoute (takeFileName . toFilePath)

dateRoute = metadataRoute createDateRoute
    where
        createDateRoute meta = let
            published = fromMaybe "1970-1-1" $ lookupString "published" meta
            datePath  = replaceAll "-" (const "/") published
            in customRoute (addDate datePath . toFilePath)
        addDate date path = takeDirectory path </> date </> takeBaseName path

prependBlogRoute = customRoute $ (</>) "blog" . toFilePath

fixupUrls :: Item String -> Compiler (Item String)
fixupUrls = relativizeUrls >=> cleanIndexUrls

finalise :: Context String -> Item String -> Compiler (Item String)
finalise ctx = loadAndApplyTemplate "templates/default.html" ctx >=> fixupUrls

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndex :: String -> String
cleanIndex url
    | index `isSuffixOf` url = take (length url - length index) url
    | otherwise              = url
    where index = "index.html"

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Vaibhav Sagar's blog"
    , feedDescription = "A blog with posts in it."
    , feedAuthorName  = "Vaibhav Sagar"
    , feedAuthorEmail = "vaibhavsagar@gmail.com"
    , feedRoot        = "https://vaibhavsagar.com"
    }

ghcSyntaxHighlight :: Pandoc -> Pandoc
ghcSyntaxHighlight = walk $ \case
    CodeBlock (_, (isHaskell -> True):_, _) (tokenizeHaskell -> Just tokens) ->
        RawBlock "html" . L.toStrict . renderHtml $ formatHaskellTokens tokens
    block -> block
    where isHaskell = (== "haskell")

formatHaskellTokens :: [(Token, T.Text)] -> H.Html
formatHaskellTokens tokens =
    H.div H.! A.class_ "sourceCode" $
        H.pre H.! A.class_ "sourceCode haskell" $
            H.code H.! A.class_ "sourceCode haskell" $
                mapM_ tokenToHtml tokens

tokenToHtml :: (Token, T.Text) -> H.Html
tokenToHtml (tokenClass -> className, text) =
    H.span H.!? (not $ T.null className, A.class_ (H.toValue className)) $
        H.toHtml text

-- | Return class corresponding to given 'TokenType'. Adapted from `mmark-ext`.
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
