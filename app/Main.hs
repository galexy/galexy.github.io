--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import              Data.List           (intersperse)
import              Data.Maybe          (fromMaybe)
import              Data.Monoid         ((<>))
import              Control.Applicative ((<|>))
import              Control.Monad       (liftM)
import              Hakyll
import              Text.Blaze.Html     (toHtml, toValue, (!))
import qualified    Text.Blaze.Html5              as H
import qualified    Text.Blaze.Html5.Attributes   as A

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

    
--------------------------------------------------------------------------
-- Build tags from posts (and used by posts)
--
    tags <- buildTags "posts/*/index.*" (fromCapture "tags/*.html")
    categories <- buildMetaCategories "posts/*/index.*" (fromCapture "categories/*.html")
    let postCtxWithTags = tagsField "tags" tags                       <> 
                          metaCategoriesField "categories" categories <>
                          postCtx

    match "posts/*/index.*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html"      postCtxWithTags
                >>= loadAndApplyTemplate "templates/default.html"   postCtxWithTags
                >>= relativizeUrls

    match "pages/*/index.*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"       siteCtx
            >>= loadAndApplyTemplate "templates/default.html"    siteCtx

--------------------------------------------------------------------------
-- generate home page with most recent 10 posts
--
    create ["index.html"] $ do
        route idRoute
        compile $ do
            let postTeaserCtx = teaserCtx postCtxWithTags
            let homeCtx = listField "posts" postTeaserCtx (recentPosts 10) <> 
                          siteCtx
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
            let listCtx = listField "items" postCtxWithTags orderedPosts <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/list.html"      listCtx
                >>= loadAndApplyTemplate "templates/default.html"   listCtx
                >>= relativizeUrls

--------------------------------------------------------------------------
-- generate tag lists
--
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title                          <>
                      listField "items" postCtxWithTags (return posts)  <> 
                      siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html"       ctx
                >>= loadAndApplyTemplate "templates/default.html"   ctx
                >>= relativizeUrls

--------------------------------------------------------------------------
-- generate category lists
--
    tagsRules categories $ \category pattern -> do
        let title = "Posts in \"" ++ category ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title                          <>
                    listField "items" postCtxWithTags (return posts)  <>
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html"       ctx
                >>= loadAndApplyTemplate "templates/default.html"   ctx
                >>= relativizeUrls

--------------------------------------------------------------------------
-- compile all the templates for use in other rules
--
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------
teaserCtx :: Context String -> Context String
teaserCtx = (teaserField "teaser" "content" <>)

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"   <>
    dateField "iso" "%Y-%m-%d"     <>
    siteCtx

siteCtx :: Context String
siteCtx =
    constField "site-title" "event -> [thought] -> Stream post"               <>
    constField "site-tagline" "A blog really for myself"                      <>
    constField "site-author" "Galex Yen"                                      <>
    constField "site-author-github" "galexy"                                  <>
    constField "site-author-linkedin" "galexyen"                              <>
    constField "site-copyright" "© 2019 Galex Yen"                            <>
    constField "site-disqus-shortname" "event-list-thought-stream-post"       <>
    constField "site-description" "event -> thoughts is a blog by Galex Yen, software engineer, Director of Data Science at Remitly" <>
    defaultContext

orderedPosts :: Compiler [Item String]
orderedPosts = do 
    let contentPattern = fromGlob "posts/*/index.*"
    recentFirst =<< loadAllSnapshots contentPattern "content"

recentPosts :: Int -> Compiler [Item String]
recentPosts n = take n <$> orderedPosts

buildMetaCategories :: MonadMetadata m 
                    => Pattern 
                    -> (String -> Identifier) 
                    -> m Tags
buildMetaCategories = buildTagsWith getMetaCategories

getMetaCategories :: MonadMetadata m => Identifier -> m [String]
getMetaCategories identifier = do
    metadata <- getMetadata identifier
    return
        $       fromMaybe []
        $       (lookupStringList "categories" metadata)
        <|>     (map trim . splitAll "," <$> lookupString "categories" metadata)

metaCategoriesField :: String
                    -> Tags
                    -> Context a
metaCategoriesField =
    tagsFieldWith getMetaCategories simpleRenderLink (mconcat . intersperse ", ")

--------------------------------------------------------------------------------
-- | Render one tag link
-- Note: Duplicate from @Hakyll.Web.Tags@
simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
simpleRenderLink _ Nothing = Nothing
simpleRenderLink tag (Just filePath) =
    Just $ H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag
