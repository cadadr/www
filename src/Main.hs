{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Monoid (mappend)
import System.FilePath.Posix (combine)
import Hakyll
import Hakyll.Core.Identifier (Identifier, fromFilePath)
import Hakyll.Core.Identifier.Pattern (Pattern, fromGlob)


contentDir :: FilePath
contentDir = "content"

templateDir :: FilePath
templateDir = combine contentDir "templates"


main :: IO ()
main = hakyll $ do
  match (pathTo "static/*") $ do
    route $ delRoute "static/"
    compile copyFileCompiler

  match (pathTo "pages/*.markdown") $ do
    route $ delRoute "pages/" `composeRoutes` setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate (tmpl "page.html") postCtx
      >>= relativizeUrls

  match (fromGlob $ combine templateDir "*") $ compile templateBodyCompiler


delRoute :: String -> Routes
delRoute r = gsubRoute (combine contentDir r) (const "")

pathTo :: FilePath -> Pattern
pathTo = fromGlob . combine contentDir

tmpl :: FilePath -> Identifier
tmpl = fromFilePath . combine templateDir


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
