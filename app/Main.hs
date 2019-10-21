--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid    ((<>))
import           Control.Monad  (liftM)
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile $ compressCssCompiler

    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html"   siteCtx
            >>= relativizeUrls

    match "posts/*/index.*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"      postCtx
            >>= loadAndApplyTemplate "templates/default.html"   postCtx
            >>= relativizeUrls

--------------------------------------------------------------------------
-- generate home page with most recent 10 posts
--
    create ["index.html"] $ do
        route idRoute
        compile $ do
            let homeCtx = listField "posts" teaserCtx (recentPosts 10) <> siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/home.html"        homeCtx
                >>= loadAndApplyTemplate "templates/default.html"     homeCtx
                >>= relativizeUrls

--------------------------------------------------------------------------
-- generate archive list of posts
--
    create ["posts/index.html"] $ do
        route idRoute 
        compile $ do
            let listCtx = listField "items" postCtx orderedPosts <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/list.html"      listCtx
                >>= loadAndApplyTemplate "templates/default.html"   listCtx

--------------------------------------------------------------------------
-- compile all the templates for use in other rules
--
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------
teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" <>
            postCtx

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"   <>
    dateField "iso" "%Y-%m-%d"     <>
    siteCtx

siteCtx :: Context String
siteCtx = 
  constField "site-title" "event -> [thought] -> Stream post"                 <>
  constField "site-tagline" "A blog really for myself"                        <>
  constField "site-author" "Galex Yen"                                        <>
  constField "site-author-github" "galexy"                                    <>
  constField "site-author-linkedin" "galexyen"                                <>
  constField "site-copyright" "© 2019 Galex Yen"                              <>
  constField "site-disqus-shortname" "event-list-thought-stream-post"         <>
  constField "site-description" "event -> thoughts is a blog by Galex Yen, software engineer, Director of Data Science at Remitly" <>
  defaultContext

orderedPosts :: Compiler [Item String]
orderedPosts = do 
    let contentPattern = fromGlob "posts/*/index.*"
    recentFirst =<< loadAllSnapshots contentPattern "content"

recentPosts :: Int -> Compiler [Item String]
recentPosts n = take n <$> orderedPosts