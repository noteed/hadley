{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad (unless, when)
import Data.Default
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as T
import Data.Version (showVersion)
import Language.Haskell.Exts.SrcLoc (SrcSpan(..))
import Language.Haskell.HLint3
import Paths_hadley (getDataFileName, version)
import System.Console.CmdArgs.Implicit hiding (def)
import System.Directory
  ( copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , getHomeDirectory, removeDirectoryRecursive
  , setCurrentDirectory
  )
import System.Exit (exitFailure, ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.Process (readProcessWithExitCode)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
-- import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Pandoc (readMarkdown)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Writers (writeHtml5)

data Project = Project
  { projectName :: Text
  , projectREADME :: FilePath
  , projectCabal :: FilePath
  }

getProject :: IO Project
getProject = return Project
  { projectName = "Hadley"
  , projectREADME = "README.md"
  , projectCabal = "hadley.cabal"
  }

data Conf = Conf
  { confProject :: Project
  , confTargetDirectory :: FilePath
  , confRefresh :: Maybe Int
  }

------------------------------------------------------------------------------
-- Command-line
------------------------------------------------------------------------------

main :: IO ()
main = (runCmd =<<) $ cmdArgs $
  modes
    [ cmdBuild
    , cmdGenerate
    , cmdClone
    ]
  &= summary versionString
  &= program "hadley"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "hadley " ++ showVersion version ++ " - Copyright (c) 2014 Vo Minh Thu."

-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdBuild
  { cmdUrl :: String
  , cmdTarget :: FilePath
  }
  | CmdGenerate
  { cmdRefreshTime :: Maybe Int
  , cmdSource :: FilePath
  , cmdTarget :: FilePath
  }
  | CmdClone
  { cmdUrl :: String
  }
  deriving (Data, Typeable)

-- | Create a 'Build' command.
cmdBuild :: Cmd
cmdBuild = CmdBuild
  { cmdUrl = def
    &= argPos 0
    &= typ "REPOSITORY_URL"
  , cmdTarget = "_static"
    &= help "Directory where generated HTML pages are put."
    &= explicit
    &= name "target"
  } &= help
      "Clone a repository and generate static HTML pages."
    &= explicit
    &= name "build"

-- | Create a 'Generate' command.
cmdGenerate :: Cmd
cmdGenerate = CmdGenerate
  { cmdRefreshTime = def
    &= help "Embed a HTTP meta tag with a refresh value."
    &= explicit
    &= name "refresh"
  , cmdSource = "."
    &= help "Source directory to process."
    &= explicit
    &= name "source"
  , cmdTarget = "_static"
    &= help "Directory where generated HTML pages are put."
    &= explicit
    &= name "target"
  } &= help
      "Generate static HTML pages."
    &= explicit
    &= name "generate"

-- | Create a 'Command' command.
cmdClone :: Cmd
cmdClone = CmdClone
  { cmdUrl = def
    &= argPos 0
    &= typ "REPOSITORY_URL"
  } &= help "Clone a Git repository."
    &= explicit
    &= name "clone"

runCmd :: Cmd -> IO ()
runCmd CmdBuild{..} = do
  runCmd (CmdClone cmdUrl)
  home <- getHomeDirectory
  let dir = home </> ".hadley" </> "clone"
  runCmd (CmdGenerate Nothing dir cmdTarget)

runCmd CmdGenerate{..} = do
  setCurrentDirectory cmdSource
  let mrefresh = cmdRefreshTime
      target = cmdTarget
  putStrLn "Generating HTML pages..."

  project@Project{..} <- getProject
  let commands =
        [ ("cabal", ["update"])
        , ("cabal", ["install", "--only-dependencies", "--enable-tests"])
        , ("cabal", ["configure", "--enable-tests"])
        , ("cabal", ["build"])
        , ("cabal", ["haddock", "--executables", "--hyperlink-source"])
          -- TODO Link to Hadley-rendered pages instead.
          -- TODO Paths_hadley has a "source" link that leads to a 404.
        ]

  createDirectoryIfMissing True (target </> "css")

  -- Copy the CSS. Handle running `hadley` from source, or once installed
  -- through Cabal.
  e <- doesFileExist "static/css/style.css"
  styleCss <- if e then return "static/css/style.css"
                   else getDataFileName "static/css/style.css"
  copyFile styleCss $ target </> "css" </> "style.css"

  -- Made-up index.html.
  T.writeFile (target </> "index.html")
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
      H.div $ do
        H.a ! A.href "hadley.cabal" $ "hadley.cabal"
        " "
        H.a ! A.href "/raw/hadley.cabal" $ "raw"

      mapM_ (uncurry indexCommand) commands

      H.div $
        H.a ! A.href "/doc" $ "Documentation"

  content <- TIO.readFile projectREADME

  -- Render the README.
  createDirectoryIfMissing True (target </> projectREADME)
  doc' <- runIOorExplode $ do
    doc <- readMarkdown def content
    writeHtml5 def doc
  T.writeFile (target </> projectREADME </> "index.html")
    $ renderHtml
    $ wrapReadme mrefresh project
    $ doc'

  -- Raw README.
  createDirectoryIfMissing True (target </> "raw")
  TIO.writeFile (target </> "raw" </> projectREADME) content

  contentCabal <- readFile projectCabal

  -- Render the `.cabal` file.
  createDirectoryIfMissing True (target </> projectCabal)
  T.writeFile (target </> projectCabal </> "index.html")
    $ renderHtml
    $ wrapCabal mrefresh project
    $ H.br >> H.pre (H.code $ H.toHtml contentCabal)

  -- Raw `.cabal`.
  createDirectoryIfMissing True (target </> "raw")
  writeFile (target </> "raw" </> projectCabal) contentCabal

  content' <- readFile ("bin" </> "hadley.hs")

  -- Render the script.
  createDirectoryIfMissing True (target </> "bin" </> "hadley.hs")
  T.writeFile (target </> "bin" </> "hadley.hs" </> "index.html")
    $ renderHtml
    $ wrapHs mrefresh project
    $ H.br >> H.pre (H.code $ H.toHtml content')

  -- Raw script.
  createDirectoryIfMissing True (target </> "raw" </> "bin")
  writeFile (target </> "raw" </> "bin" </> "hadley.hs") content'

  -- HLint output.
  (flags, classify, hint) <- autoSettings
  Right m <- parseModuleEx flags "bin/hadley.hs" Nothing
  createDirectoryIfMissing True (target </> "hlint" </> "bin")
  T.writeFile (target </> "hlint" </> "bin/hadley.hs")
    $ renderHtml
    $ renderIdeas mrefresh project
    $ applyHints classify hint [m]

  let conf = Conf project target mrefresh

  mapM_ (\(a, b) -> generateCommand conf a b "") commands
  e' <- doesDirectoryExist (target </> "doc")
  when e' $ removeDirectoryRecursive (target </> "doc")
  -- Use cp -r instead of renameDirectory because of the limitation
  -- "rename: unsupported operation (Invalid cross-device link)"
  -- when using Docker volumes.
  -- renameDirectory "dist/doc/html/hadley/hadley" (target </> "doc")
  (code, out, err) <- readProcessWithExitCode
    "cp" ["-r", "dist/doc/html/hadley/hadley", target </> "doc"] ""
  return ()

runCmd CmdClone{..} = do
  home <- getHomeDirectory
  let dir = home </> ".hadley" </> "clone"
  createDirectoryIfMissing True dir
  putStrLn "Cloning..."
  (code, out, err) <- readProcessWithExitCode
    "git" ["clone", cmdUrl, dir] ""
  case code of
    ExitSuccess -> putStr out >> putStrLn "Done."
    ExitFailure _ -> do
      putStrLn "Clone failed. Output was:"
      unless (null out) $ putStrLn out
      unless (null err) $ putStrLn err
      exitFailure

generateCommand :: Conf -> String -> [String] -> String -> IO ()
generateCommand Conf{..} cmd arguments input = do
  let project = confProject
      target = confTargetDirectory
      mrefresh = confRefresh
      filename = cmd ++ case arguments of {x:_ -> "-" ++ x ; _ -> ""}
  putStrLn $ "Running " ++ filename ++ "..."
  (code, out, err) <- readProcessWithExitCode
    cmd arguments input

  -- Render the command output.
  T.writeFile (target </> filename <.> "html")
    $ renderHtml
    $ wrapCommand mrefresh project
    $ htmlProcess cmd arguments input code out err

  -- Raw command output.
  writeFile (target </> "raw" </> filename <.> "txt") (out ++ err)
  case code of
    ExitSuccess -> putStrLn "Done."
    ExitFailure _ -> do
      putStrLn "Command exited with non-zero code."
      unless (null out) $ putStrLn out
      unless (null err) $ putStrLn err
      exitFailure

indexCommand :: String -> [String] -> Html
indexCommand cmd arguments = do
  let filename = cmd ++ case arguments of {x:_ -> "-" ++ x ; _ -> ""}
  H.div $ do
    H.a ! A.href (H.toValue $ filename ++ ".html") $
      H.code $ H.toHtml $ unwords $ cmd : arguments
    " "
    H.a ! A.href (H.toValue $ "/raw/" ++ filename ++".txt") $ "raw"

------------------------------------------------------------------------------
-- HTML
------------------------------------------------------------------------------

wrapReadme :: Maybe Int -> Project -> Html -> Html
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
  H.div $ H.toHtml (show ideaModule) >> H.toHtml (show ideaDecl)

htmlSrcSpan :: SrcSpan -> Html
htmlSrcSpan SrcSpan{..} =
  H.code $ do
    H.toHtml srcSpanFilename -- TODO Link.
    ":" >> H.toHtml (show srcSpanStartLine)
    ":" >> H.toHtml (show srcSpanStartColumn)

wrapCabal :: Maybe Int -> Project -> Html -> Html
wrapCabal mrefresh Project{..} content = flip (document mrefresh projectName) (return ()) $ do
  H.div $ do
    H.strong "hadley.cabal"
    " "
    H.a ! A.href "/raw/hadley.cabal" $ "raw"
  H.div content

wrapHs :: Maybe Int -> Project -> Html -> Html
wrapHs mrefresh Project{..} content = flip (document mrefresh projectName) (return ()) $ do
  H.div $ do
    H.strong "bin/hadley.hs"
    " "
    H.a ! A.href "/hlint/bin/hadley.hs" $ "hlint"
    " "
    H.a ! A.href "/raw/bin/hadley.hs" $ "raw"
  H.div content

wrapCommand :: Maybe Int -> Project -> Html -> Html
wrapCommand mrefresh Project{..} content = flip (document mrefresh projectName) (return ()) $ do
  H.div $ do
    H.strong $ H.code "ls -la"
    " "
    H.a ! A.href "/raw/ls-la.txt" $ "raw"
  H.div content

document :: Maybe Int -> Text -> Html -> Html -> Html
document mrefresh title content menu = do
  H.docType
  html_
  H.meta ! A.charset "utf-8"
  H.title $ H.toHtml title
  H.style "@import url(/css/style.css);"
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

-- | Render the result of readProcessWithExitCode.
htmlProcess cmd arguments input code out err = do
  H.strong "command:"
  H.br
  H.code . H.toHtml $ unwords $ cmd : arguments
  H.br
  H.strong "stderr:"
  H.pre . H.toHtml $ err
  H.strong "stdout:"
  H.pre . H.toHtml $ out
