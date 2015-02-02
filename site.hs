--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mempty)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- static pages go here, possibly need to add internal page name
    -- metadata to handle the page-active field
    match (fromList["about.md"]) $ do
        route   $ setExtension "html"
        compile $ do
            let aboutCtx = constField "about-active" "" `mappend` defaultContext
            pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" aboutCtx
              >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "post-content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- TODO eliminate this duplication

    match "projects/*" $ do
        compile $ pandocCompiler
            >>= saveSnapshot "project-content"

    create ["projects.html"] $ do
        route idRoute
        compile $ do
            projects <- recentFirst =<< loadAllSnapshots "projects/*" "project-content"
            let projectCtx =
                    listField "projects" postCtx (return projects) `mappend`
                    constField "title" "Projects"            `mappend`
                    constField "projects-active" ""           `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/projects.html" projectCtx
                >>= loadAndApplyTemplate "templates/default.html" projectCtx
                >>= relativizeUrls

    match "talks/*" $ do
        compile $ pandocCompiler
            >>= saveSnapshot "talk-content"

    create ["talks.html"] $ do
        route idRoute
        compile $ do
            talks <- recentFirst =<< loadAllSnapshots "talks/*" "talk-content"
            let talkCtx =
                    listField "talks" postCtx (return talks) `mappend`
                    constField "title" "Talks"            `mappend`
                    constField "talks-active" ""          `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/talks.html" talkCtx
                >>= loadAndApplyTemplate "templates/default.html" talkCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    constField "archive-active" ""           `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "post-content"
            let
                moreCtx = 
                    if (length posts >= 10)
                       then constField "more" ""
                       else mempty
                indexCtx =
                    moreCtx `mappend`
                    listField "posts" postCtx (return . take 10 $ posts) `mappend`
                    constField "title" "Home"                `mappend`
                    constField "home-active" ""              `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
