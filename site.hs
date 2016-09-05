--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid    (mappend)
import           Data.Monoid    ((<>))
import           Control.Monad  (liftM)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route $ idRoute
        compile $ compressCssCompiler

    match "pages/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" siteCtx
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    match "pages/*" $ version "titleLine" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler

    match "404.html" $ do
        route $ idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    match "posts/*/index.*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    blog <- buildPaginateWith grouper "posts/*/index.*"
        (\n -> if n == 1
            then "index.html"
            else fromCapture "page/*.html" (show n))

    paginateRules blog $ \pageNum pattern -> do
        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots pattern "content"
            let paginateCtx = paginateContext blog pageNum
            let ctx         =
                    constField "title" "Blog"                  <>
                    listField "posts" teaserCtx (return posts) <>
                    paginateCtx                                <>
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    siteCtx

siteCtx :: Context String
siteCtx = 
  listField "pages" postCtx (loadAll ("pages/*" .&&. hasVersion "titleLine"))  `mappend`
  constField "site-title" "event -> [thought] -> Stream post"         `mappend`
  constField "site-tagline" "A blog really for myself"                  `mappend`
  constField "site-description" "event -> thoughts is a blog by Galex Yen, software engineer, Director of Data Science at Remitly"    `mappend`
  defaultContext

-- Run sortRecentFirst on ids, and then liftM (paginateEvery 3) into it
grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper ids = (liftM (paginateEvery 3) . sortRecentFirst) ids
