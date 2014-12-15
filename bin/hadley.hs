{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Default
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as T
import Language.Haskell.Exts.SrcLoc (SrcSpan(..))
import Language.Haskell.HLint3
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
-- import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Pandoc (readMarkdown, writeHtml)
import Text.Pandoc.Options (writerHtml5)

data Project = Project
  { projectName :: Text
  , projectREADME :: FilePath
  }

getProject = return Project
  { projectName = "Hadley"
  , projectREADME = "README.md"
  }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> main' Nothing
    [refresh] -> main' (Just $ read refresh)

main' mrefresh = do
  putStrLn "Hadley."
  project@Project{..} <- getProject
  createDirectoryIfMissing True "_static"

  -- Made-up index.html.
  T.writeFile ("_static" </> "index.html")
    $ renderHtml
    $ flip (document mrefresh projectName) (return ())
    $ do
      H.strong $ H.toHtml projectName
      H.div $ do
        H.a ! A.href (H.toValue projectREADME) $ "README.md"
        " "
        H.a ! A.href (H.toValue $ "/raw/" ++ projectREADME) $ "raw"
      H.div $ do
        H.a ! A.href "/bin/hadley.hs" $ "bin/hadley.hs"
        " "
        H.a ! A.href "/hlint/bin/hadley.hs" $ "hlint"
        " "
        H.a ! A.href "/raw/bin/hadley.hs" $ "raw"

  content <- readFile projectREADME

  -- Render the README.
  createDirectoryIfMissing True ("_static" </> projectREADME)
  T.writeFile ("_static" </> projectREADME </> "index.html")
    $ renderHtml
    $ wrapReadme mrefresh project
    $ writeHtml def { writerHtml5 = True }
    $ readMarkdown def content

  -- Raw README.
  createDirectoryIfMissing True ("_static" </> "raw")
  writeFile ("_static" </> "raw" </> projectREADME) content

  content' <- readFile ("bin" </> "hadley.hs")

  -- Render the script.
  createDirectoryIfMissing True ("_static" </> "bin" </> "hadley.hs")
  T.writeFile ("_static" </> "bin" </> "hadley.hs" </> "index.html")
    $ renderHtml
    $ wrapHs mrefresh project
    $ H.br >> H.pre (H.code $ H.toHtml content')

  -- Raw script.
  createDirectoryIfMissing True ("_static" </> "raw" </> "bin")
  writeFile ("_static" </> "raw" </> "bin" </> "hadley.hs") content'

  -- HLint output.
  (flags, classify, hint) <- autoSettings
  Right m <- parseModuleEx flags "bin/hadley.hs" Nothing
  createDirectoryIfMissing True ("_static" </> "hlint" </> "bin")
  T.writeFile ("_static" </> "hlint" </> "bin/hadley.hs")
    $ renderHtml
    $ renderIdeas mrefresh project
    $ applyHints classify hint [m]

wrapReadme mrefresh Project{..} content = flip (document mrefresh projectName) (return ()) $ do
  H.div $ do
    H.strong $ H.toHtml projectREADME
    " "
    H.a ! A.href (H.toValue $ "/raw/" ++ projectREADME) $ "raw"
  H.div content

renderIdeas :: Maybe Int -> Project -> [Idea] -> Html
renderIdeas mrefresh Project{..} ideas = flip (document mrefresh projectName) (return ()) $ do
  H.div $ H.strong $ H.toHtml ("HLint" :: Text)
  H.div $ do
    H.strong "bin/hadley.hs"
    " "
    H.a ! A.href "/bin/hadley.hs" $ "src"
    " "
    H.a ! A.href "/raw/bin/hadley.hs" $ "raw"
  if null ideas
    then H.div "No suggestions."
    else H.div $ mapM_ (H.div . htmlIdea) ideas

htmlIdea :: Idea -> Html
htmlIdea Idea{..} = do
  H.div $ do
    htmlSrcSpan ideaSpan >> " - "
    H.toHtml (show ideaSeverity) >> " - "
    H.toHtml ideaHint
  H.div $ "Found: " >> H.toHtml ideaFrom
  H.div $ "Why not: " >> H.toHtml (show ideaTo)
  H.div $ H.toHtml $ show ideaNote
  H.div $ H.toHtml ideaModule >> H.toHtml ideaDecl

htmlSrcSpan SrcSpan{..} =
  H.code $ do
    H.toHtml srcSpanFilename -- TODO Link.
    ":" >> H.toHtml (show srcSpanStartLine)
    ":" >> H.toHtml (show srcSpanStartColumn)

wrapHs mrefresh Project{..} content = flip (document mrefresh projectName) (return ()) $ do
  H.div $ do
    H.strong "bin/hadley.hs"
    " "
    H.a ! A.href "/hlint/bin/hadley.hs" $ "hlint"
    " "
    H.a ! A.href "/raw/bin/hadley.hs" $ "raw"
  H.div content

document :: Maybe Int -> Text -> Html -> Html -> Html
document mrefresh title content menu = do
  H.docType
  html_
  H.meta ! A.charset "utf-8"
  H.title $ H.toHtml title
  H.style "@import url(/style.css);"
  maybe
    (return ())
    (\r -> H.meta ! A.httpEquiv "refresh" ! A.content (H.toValue $ show r))
    mrefresh

  H.header $ H.a ! A.href "/" $ H.toHtml title
  H.div ! A.id "main" $ content

  H.div ! A.id "menu" $ menu

-- | Self-closing <html> tag.
html_ :: Html
html_ = H.preEscapedToHtml ("<html>" :: String)
