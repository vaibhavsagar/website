#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p 'haskellPackages.ghcWithPackages (p: with p; [ hakyll filepath ])'

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad ((>=>))
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath
import           Data.List (isSuffixOf)
import           Data.Maybe (fromMaybe)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "blog/*" (fromCapture "tags/*")

    let postCtx =
            tagsField "tags" tags       `mappend`
            dateField "date" "%e %B %Y" `mappend`
            defaultContext

    tagsRules tags $ \tag pat -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route   $ prependBlogRoute `composeRoutes` cleanRoute
        compile $ do
            let posts = recentFirst =<< loadAll pat
            let tagsCtx =
                    constField "title" title        `mappend`
                    listField "posts" postCtx posts `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" tagsCtx
                >>= finalise                                  tagsCtx

    match "blog/*" $ do
        route   $ dateRoute `composeRoutes` cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"   postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/disqus.html" postCtx
            >>= finalise                                     postCtx

    match "drafts/*" $ do
        route     cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= finalise                                   postCtx

    match "pages/*" $ do
        route   $ rootRoute `composeRoutes` cleanRoute
        compile $ pandocCompiler
            >>= finalise defaultContext

    match "extra/*" $ do
        route   rootRoute
        compile copyFileCompiler

    create ["archive/index.html"] $ do
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAll "blog/*"
            let archiveCtx =
                    listField "posts" postCtx posts `mappend`
                    constField "title" "Archives"   `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= finalise                                      archiveCtx

    match "index.html" $ do
        route idRoute
        compile $ do
            let posts = fmap (take 10) . recentFirst =<< loadAll "blog/*"
            let indexCtx =
                    listField "posts" postCtx posts `mappend`
                    constField "title" "Home"       `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= finalise        indexCtx

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "blog/*" "content"
            renderAtom feedConfig feedCtx posts

--------------------------------------------------------------------------------
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
    where createIndexRoute ident =
            takeDirectory p </> takeBaseName p </> "index.html"
            where p = toFilePath ident

rootRoute :: Routes
rootRoute = customRoute (takeFileName . toFilePath)

dateRoute :: Routes
dateRoute = metadataRoute createDateRoute
    where
        createDateRoute meta = let
            published = fromMaybe "1970-1-1" $ lookupString "published" meta
            datePath  = replaceAll "-" (const "/") published
            in customRoute (addDate datePath . toFilePath)
        addDate date path = takeDirectory path </> date </> takeBaseName path

prependBlogRoute :: Routes
prependBlogRoute = customRoute prependBlog
    where prependBlog ident = "blog" </> toFilePath ident

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
