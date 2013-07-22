{-# LANGUAGE OverloadedStrings #-}

{-

Build script for variadic.me using Hakyll 4.1

Based on Jasper Van der Jeugt's blog <http://jaspervdj.be>, with 
minor alterations.

-}

module Main where


--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend, mconcat)
import           Prelude             hiding (id)
import qualified Text.Pandoc         as Pandoc


--------------------------------------------------------------------------------
import           Hakyll


--------------------------------------------------------------------------------
-- | Entry point
main :: IO ()
main = hakyllWith config $ do
    -- Static files
    match ("images/*" .||. "favicon.ico" .||. "robots.txt") $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Copy JS
    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render each and every post
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ do
            pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" "Posts" `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Index
    match "index.html" $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" $ take 5 . recentFirst
            let indexContext = constField "posts" list `mappend`
                    field "tags" (\_ -> renderTagList tags) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    -- Read templates
    match "templates/*" $ compile $ templateCompiler

    -- Render some static pages
    match (fromList ["about.markdown", "projects.markdown"]) $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= return . take 10 . recentFirst
                >>= renderAtom feedConfiguration feedCtx


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]


--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum --delete -ave 'ssh' \
                       \_site/* variadic@variadic.me:variadic.me"
    }


--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "variadic.me blog feed"
    , feedDescription = "blog feed"
    , feedAuthorName  = "Erik Kronberg"
    , feedAuthorEmail = "erik@variadic.me"
    , feedRoot        = "http://variadic.me"
    }


--------------------------------------------------------------------------------
postList :: Tags -> Pattern -> ([Item String] -> [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts       <- preprocess' <$> loadAll pattern
    applyTemplateList postItemTpl (postCtx tags) posts


--------------------------------------------------------------------------------
