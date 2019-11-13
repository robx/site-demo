--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "nogen/**" $ do
        route (gsubRoute "nogen/" (const ""))
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "spam/*.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "spam/index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "spam/*.markdown"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["spam/atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <>
                    bodyField "description"

            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "spam/*.markdown" "content"
            renderAtom feedConfig feedCtx posts

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "vllmrt.net/spam"
    , feedDescription = "Feed for the posts at vllmrt.net/spam"
    , feedAuthorName  = "Robert Vollmert"
    , feedAuthorEmail = Nothing
    , feedRoot        = "https://vllmrt.net"
    }
