--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Web.Sass


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "fonts/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "sass/*" $ do
    route   $ gsubRoute "sass/" (const "css/") `composeRoutes` setExtension "css"
    compile $
      sassCompiler >>= makeItem . compressCss . itemBody

  match "contacts.markdown" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  match "archive.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      getResourceBody
        >>= applyAsTemplate (postListCtx posts)
        >>= loadAndApplyTemplate "templates/default.html" (postListCtx posts)
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- take 5 <$> (recentFirst =<< loadAll "posts/*")
      getResourceBody
        >>= applyAsTemplate (postListCtx posts)
        >>= loadAndApplyTemplate "templates/default.html" (postListCtx posts)
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%d.%m.%Y" `mappend`
  defaultContext

postListCtx :: [Item String] -> Context String
postListCtx posts =
  listField "posts" postCtx (return posts) `mappend`
  defaultContext
