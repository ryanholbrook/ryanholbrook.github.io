--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Monoid         (mappend)
import           Hakyll
import           System.FilePath
import           Text.Pandoc.Options
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match ("images/*" .||. "files/*" .||. "scripts/*") $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route idRoute
      compile compressCssCompiler

    -- Build tags and create a page for each tag
    tags <- sortTagsBy caseInsensitiveTags <$>
      buildTags postPattern (fromCapture "tags/*.html")
    tagsRules tags $ \tag pat -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pat
            let tagContext =
                  constField "title" title
                  <> listField "tag-posts" simplePostContext (return posts)
                  <> mainContext tags
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html"     tagContext
                >>= loadAndApplyTemplate "templates/page.html"    tagContext
                >>= loadAndApplyTemplate "templates/default.html" tagContext
                >>= cleanUrls

    -- Build tables of contents
    match "posts/*" $ version "toc" $
       compile $ pandocCompilerWith defaultHakyllReaderOptions
                                    mainWriterOptions
                                      { writerTableOfContents = True
                                      , writerTOCDepth = 4
                                      , writerIncremental = True
                                      , writerTemplate = Just "$toc$" }

    -- Build a page for each post
    match "posts/*" $ do
      route cleanRoute
      compile $
        let postContext =
              tagsField "tags" tags
              <> field "previousPostUrl" (previousPostUrl postPattern)
              <> field "nextPostUrl"     (nextPostUrl postPattern)
              <> field "toc"
                   (\item -> loadBody ((itemIdentifier item)
                                       { identifierVersion = Just "toc"}))
              <> mainContext tags

        in
        pandocMainCompiler
          >>= loadAndApplyTemplate "templates/post-body.html" postContext
          >>= saveSnapshot "content" -- Save content for landing page (index.html)
                                     -- Must come after post-body so the post $toc$
                                     -- gets built into the content snapshots.
          >>= loadAndApplyTemplate "templates/post.html"    postContext
          >>= loadAndApplyTemplate "templates/default.html" postContext
          >>= cleanUrls

    -- Build a page with a list of all posts
    create ["archive.html"] $ do
        route cleanRoute
        compile $
            let archiveContext =
                  constField "title" "Archives"
                  <> mainContext tags
            in
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveContext
                >>= loadAndApplyTemplate "templates/page.html"    archiveContext
                >>= loadAndApplyTemplate "templates/default.html" archiveContext
                >>= cleanUrls

    -- Build static pages
    match ("about.org" .||. "404.org") $ do
        route cleanRoute
        compile $
          let pageContext = mainContext tags
          in
          pandocMainCompiler
            >>= loadAndApplyTemplate "templates/page.html"    pageContext
            >>= loadAndApplyTemplate "templates/default.html" pageContext
            >>= cleanUrls

    -- Build notebooks
    -- TODO

    -- Build main pages
    paginateBy10 <- buildPaginateWith grouperBy10 postPattern postsPageIdBy10
    paginateRules paginateBy10 $ \page pat -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots pat "content"
            let indexCtx =
                    listField "posts" simplePostContext (return posts)
                    <> constField "title" "Math for Machines"
                    <> paginateContext paginateBy10 page
                    <> mainContext tags

            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= cleanUrls

    -- Build sitemap
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postPattern
            pages <- loadAll (fromList ["about.org", "archive.html"])
            let ctx =
                  defaultContext
                  <> constField "root" root
                  <> dateField "date" "%F" -- need WC3 format
                sitemapCtx =
                  ctx
                  <> listField "posts" ctx (return posts)
                  <> listField "pages" ctx (return pages)

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
                >>= cleanUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

root :: String
root = "https://mathformachines.com"

postPattern :: Pattern
postPattern = "posts/*" .&&. hasNoVersion


-----------
-- Contexts

-- Minimal context for post metadata
simplePostContext :: Context String
simplePostContext =
  dateField "date" "%B %e, %Y"
  <> constField "root" root
  <> defaultContext

-- Context for rendering default.html
mainContext :: Tags -> Context String
mainContext tags =
  -- Preload the list of posts so we don’t create a dependency cycle
  -- when trying to compile "posts"
  let postList = do
        identifiers <- sortRecentFirst =<< getMatches postPattern
        return [Item identifier "" | identifier <- identifiers]
  in
  defaultContext
  <> simplePostContext
  <> listField "archive-list" --display all posts
       simplePostContext postList
  <> listField "recent-list" -- display 10 most recent posts
       simplePostContext (take 10 <$> postList)
  <> tagCloudField "alltags" 100 120 tags


------------
-- Compilers

mainWriterOptions :: WriterOptions
mainWriterOptions =
      let mathExtensions = extensionsFromList [ Ext_tex_math_dollars
                                              , Ext_tex_math_double_backslash
                                              , Ext_latex_macros]
          defaultExtensions = writerExtensions defaultHakyllWriterOptions
          newExtensions = defaultExtensions <> mathExtensions
      in defaultHakyllWriterOptions
         { writerExtensions     = newExtensions
         , writerHTMLMathMethod = MathJax ""
         , writerHighlightStyle = Nothing}

pandocMainCompiler :: Compiler (Item String)
pandocMainCompiler =
  pandocCompilerWith defaultHakyllReaderOptions mainWriterOptions


-------------
-- Pagination

-- Pagination for Main page
postsPageIdBy10 :: PageNumber -> Identifier
postsPageIdBy10 n = fromFilePath $
  if n == 1
  then "index.html"
  else show n ++ "/index.html"

grouperBy10 :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouperBy10 = fmap (paginateEvery 3) . sortRecentFirst

-- Pagination for Post pages
withRelatedPost ::
  (MonadMetadata m, Alternative m)
  => (Identifier -> [Identifier] -> Maybe t)
  -> (t -> m b)
  -> Pattern
  -> Item a
  -> m b
withRelatedPost r f pat item = do
    idents <- getMatches pat >>= sortRecentFirst
    let id = itemIdentifier item
        newId = r id idents
    case newId of
        Just i  -> f i
        Nothing -> empty

withPreviousPost ::
  (MonadMetadata m, Alternative m)
  => (Identifier -> m b)
  -> Pattern
  -> Item a
  -> m b
withPreviousPost = withRelatedPost itemAfter
    where
        itemAfter x xs = lookup x $ zip xs (tail xs)

withNextPost ::
  (MonadMetadata m, Alternative m)
  => (Identifier -> m b)
  -> Pattern -> Item a
  -> m b
withNextPost = withRelatedPost itemBefore
    where
        itemBefore x xs = lookup x $ zip (tail xs) xs

previousPostUrl :: Pattern -> Item String -> Compiler String
previousPostUrl = withPreviousPost (fmap (maybe empty toUrl) . getRoute)

nextPostUrl :: Pattern -> Item String -> Compiler String
nextPostUrl = withNextPost (fmap (maybe empty toUrl) . getRoute)


-------------
-- Clean URLs

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident =
      takeDirectory p </> takeBaseName p </> "index.html"
      where p = toFilePath ident

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pat replacement)
    where
      pat = "/index.html"
      replacement = const "/"

cleanUrls :: Item String -> Compiler (Item String)
cleanUrls = relativizeUrls
            >=> cleanIndexUrls
            >=> cleanIndexHtmls
