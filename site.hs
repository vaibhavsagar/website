--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

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

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route     cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls

    tags <- buildTags "blog/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pat -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAll pat
            let tagsCtx =
                    constField "title" title               `mappend`
                    listField "posts" (postCtx tags) posts `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html"     tagsCtx
                >>= loadAndApplyTemplate "templates/default.html" tagsCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match ("blog/*" .||. "drafts/*") $ do
        route   $ dateRoute `composeRoutes` cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    match "extra/*" $ do
        route   rootRoute
        compile copyFileCompiler

    create ["archive/index.html"] $ do
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAll "blog/*"
            let archiveCtx =
                    listField "posts" (postCtx tags) posts `mappend`
                    constField "title" "Archives"          `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            let posts = fmap (take 10) . recentFirst =<< loadAll "blog/*"
            let indexCtx =
                    listField "posts" (postCtx tags) posts `mappend`
                    constField "title" "Home"              `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx tags `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "blog/*" "content"
            renderAtom feedConfig feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags =
    tagsField "tags" tags       `mappend`
    dateField "date" "%e %B %Y" `mappend`
    defaultContext

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
