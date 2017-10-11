#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell
#! nix-shell -p 'haskellPackages.ghcWithPackages (p: [ p.hakyll p.filepath ])'
#! nix-shell -Q

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.Monad ((>=>))
import           Data.Monoid ((<>))
import           Hakyll
import           System.FilePath
import           Data.List (isSuffixOf)
import           Data.Maybe (fromMaybe)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    matcher "images/*" idRoute copyFileCompiler

    matcher "css/*" idRoute compressCssCompiler

    tags <- buildTags "blog/*" (fromCapture "tags/*")

    let postCtx =
            tagsField "tags" tags       <>
            dateField "date" "%e %B %Y" <>
            defaultContext

    tagsRules tags $ \tag pat -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route   $ prependBlogRoute `composeRoutes` cleanRoute
        compile $ do
            let posts = recentFirst =<< loadAll pat
            let tagsCtx =
                    constField "title" title        <>
                    listField "posts" postCtx posts <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" tagsCtx
                >>= finalise                                  tagsCtx

    matcher "blog/*" (dateRoute `composeRoutes` cleanRoute) $
        pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"   postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/disqus.html" postCtx
        >>= finalise                                     postCtx

    matcher "drafts/*" cleanRoute $
        pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= finalise                                   postCtx

    matcher "pages/*" (rootRoute `composeRoutes` cleanRoute) $
        pandocCompiler >>= finalise defaultContext

    matcher "extra/*" rootRoute copyFileCompiler

    create ["archive/index.html"] $ do
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAll "blog/*"
            let archiveCtx =
                    listField "posts" postCtx posts <>
                    constField "title" "Archives"   <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= finalise                                      archiveCtx

    matcher "index.html" idRoute $ do
        let posts = fmap (take 10) . recentFirst =<< loadAll "blog/*"
        let indexCtx =
                listField "posts" postCtx posts <>
                constField "title" "Home"       <>
                defaultContext

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
            renderAtom feedConfig feedCtx posts

    where matcher path router compiler =
            match path $ route router >> compile compiler

--------------------------------------------------------------------------------
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

prependBlogRoute = customRoute $ combine "blog" . toFilePath

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
    , feedRoot        = "http://vaibhavsagar.com"
    }
