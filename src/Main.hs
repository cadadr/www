-- Main.hs

{-

Copyright (c) 2020, İ. Göktuğ Kayaalp

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of İ. Göktuğ Kayaalp nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad (when)
import Data.Monoid (mappend)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.Environment (getArgs, lookupEnv)
import System.FilePath.Posix (combine)

import Hakyll
import Hakyll.Core.Configuration (defaultConfiguration)
import Hakyll.Core.Identifier (Identifier, fromFilePath)
import Hakyll.Core.Identifier.Pattern (Pattern, fromGlob)
import Hakyll.Web.Sass (sassCompiler)

cfg = defaultConfiguration

contentDir :: FilePath
contentDir = "content"

templateDir :: FilePath
templateDir = "templates"


main :: IO ()
main = do
  (action:_) <- getArgs
  -- This takes inpiration from
  -- https://jip.dev/posts/drafts-in-hakyll/.
  draftsEnv <- lookupEnv "DRAFTS"
  let previewDir = "_preview"
  let draftsEnabled =
        case draftsEnv of
          Just draftsEnv' -> draftsEnv' == "yes"
          Nothing -> False

  -- When cleaning, remove _preview even if we’re not in the drafts
  -- mode.
  when (action == "clean" && not draftsEnabled) $ do
    exists <- doesDirectoryExist previewDir
    when exists $ do
      putStrLn $ "Removing " ++ previewDir ++ "..."
      removeDirectoryRecursive previewDir

  let cfg' = if draftsEnabled
             then cfg { destinationDirectory = previewDir }
             else cfg

  hakyllWith cfg' $ do
    -- Pages
    match (if draftsEnabled
           then pathTo "pages/*.markdown" .||. pathTo "pages/drafts/*.markdown"
           else pathTo "pages/*.markdown") $ do
      route $ delRouteAndSetExtension "pages/(drafts/)?" "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate (tmpl "page.html") postCtx
        >>= relativizeUrls

    -- Static files
    match (pathTo "static/*") $ do
      route $ delRoute "static/"
      compile copyFileCompiler

    match (pathTo "static/img/*") $ do
      route $ delRoute "static/"
      compile copyFileCompiler

    match (pathTo "static/blog/*") $ do
      route $ delRoute "static/"
      compile copyFileCompiler

    -- SCSS
    match "scss/*.scss" $ do
      route $ (gsubRoute "scss/" (const "")) `composeRoutes` setExtension "css"
      compile sassCompiler

    -- Personal blog.
    let diyorkiBlogName = "Göktuğ diyor ki ..."
    let diyorkiBlogUrl = "diyoki/index.html"

    let diyokiPattern
          = if draftsEnabled
            then (pathTo "blog-diyoki/*.markdown"
                  .||. pathTo "blog-diyoki/drafts/*.markdown")
            else pathTo "blog-diyoki/*.markdown"
    let diyokiSnap = "content-blog-diyoki"

    match diyokiPattern $ do
      let diyorkiBlogCtx =
            constField "blog" diyorkiBlogName `mappend`
            constField "blogUrl" diyorkiBlogUrl `mappend`
            postCtx
      -- XXX(2020-07-18): no need to remove drafts/ here because the
      -- blog front page links to the urls with draft/ in them
      -- anyways.
      route $ delRouteAndSetExtension "blog-" "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate (tmpl "blog-diyoki/post.html") diyorkiBlogCtx
        >>= saveSnapshot diyokiSnap
        >>= loadAndApplyTemplate (tmpl "blog-diyoki/page.html") diyorkiBlogCtx
        >>= relativizeUrls

    -- TODO(2020-05-22): paginate
    create [fromFilePath diyorkiBlogUrl] $ do
      route $ idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots diyokiPattern diyokiSnap
        let archiveCtx =
              listField "posts" postCtx (return posts) `mappend`
              constField "blog" diyorkiBlogName `mappend`
              constField "title" diyorkiBlogName `mappend`
              constField "notitle" "yes" `mappend`
              constField "blogUrl" diyorkiBlogUrl `mappend`
              defaultContext
        makeItem ""
          >>= loadAndApplyTemplate (tmpl "blog-diyoki/listing.html") archiveCtx
          >>= loadAndApplyTemplate (tmpl "blog-diyoki/page.html") archiveCtx
          >>= relativizeUrls

    create ["diyoki/diyoki.atom.xml"] $ do
      route idRoute
      compile $ do
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots diyokiPattern diyokiSnap
        renderAtom' fc postCtx posts

    loadTemplatesAt Nothing
    loadTemplatesAt $ Just "blog-diyoki"
  where
    fc :: FeedConfiguration
    fc = FeedConfiguration
      { feedTitle       = "Göktuğ diyor ki ..."
      , feedDescription = "Göktuğ's rants and ramblings."
      , feedAuthorName  = "Göktuğ Kayaalp"
      , feedAuthorEmail = "self [at] gkayaalp [dot] com"
      , feedRoot        = "https://www.gkayaalp.com/"
      }


loadTemplatesAt :: Maybe FilePath -> Rules ()
loadTemplatesAt (Just p) = loadTemplatesAt' (combine p "*")
loadTemplatesAt Nothing  = loadTemplatesAt' "*"
loadTemplatesAt' :: FilePath -> Rules ()
loadTemplatesAt' p = match (fromGlob $ combine templateDir p) $ compile templateBodyCompiler


delRouteAndSetExtension :: String -> String -> Routes
delRouteAndSetExtension d x = delRoute d `composeRoutes` setExtension x


delRoute :: String -> Routes
delRoute r = gsubRoute (combine contentDir r) (const "")

pathTo :: FilePath -> Pattern
pathTo = fromGlob . combine contentDir

tmpl :: FilePath -> Identifier
tmpl = fromFilePath . combine templateDir


postCtx :: Context String
postCtx =
    dateField "date" "%F" `mappend`
    defaultContext


-- Use my templates
renderAtom' :: FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)
renderAtom' cf cx ps = do
  a <- loadBody $ tmpl "atom.xml"
  i <- loadBody $ tmpl "atom-item.xml"
  renderAtomWithTemplates a i cf cx ps
