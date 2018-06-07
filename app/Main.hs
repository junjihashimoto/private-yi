{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


--module Yaml where

import Control.Monad.State.Lazy (execStateT)
import Data.List                (intersperse)
import Lens.Micro.Platform      ((.=))
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))
import qualified Data.Text                      as T (Text)
import qualified Data.Map as M

import Options.Applicative

import Yi hiding (option)
import Yi.Config.Simple.Types
import Yi.Config.Simple hiding (option)
import Yi.Mode.Common
import Yi.Buffer.Misc (lineMoveRel)
import Lens.Micro.Platform ((.~),(%~))


import Yi.Config.Default.HaskellMode    (configureHaskellMode)
import Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import Yi.Config.Default.MiscModes      (configureMiscModes)

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.Yaml as Y
import Control.Monad (forM_)
import qualified System.Directory as S

#ifdef VIM
import Yi.Config.Default.Vim (configureVim)
#endif
#ifdef VTY
import Yi.Config.Default.Vty (configureVty)
#endif
#ifdef EMACS
import Yi.Config.Default.Emacs (configureEmacs)
#endif
#ifdef PANGO
import Yi.Config.Default.Pango (configurePango)
#endif


$(A.deriveJSON A.defaultOptions ''IndentSettings)

data ModeParam = ModeParam {
  modepSuffixies :: [String]
    -- ^ What type of files does this mode apply to?
  , modepIndentSettings :: IndentSettings
} deriving (Show,Eq)

$(A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 5} ''ModeParam)

data UserParam = UserParam {
  userpFrontend :: T.Text
  , userpKeymap :: T.Text
  , userpModeParams :: M.Map T.Text ModeParam
} deriving (Show,Eq)

$(A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 5} ''UserParam)

myConfig :: UserParam -> ConfigM ()
myConfig param = do
  case userpFrontend param of
    "vty" -> configureVty
--    "pango" -> configurePango
    _ -> configureVty

  case userpKeymap param of
    "emacs" -> configureEmacs
    "vim" -> configureVim
    _ -> configureVim
  configureHaskellMode
  configureJavaScriptMode
  configureMiscModes
  forM_ (M.toList $ userpModeParams param) $ \(name,p)  -> do
    modifyModeByName name $ \mode -> mode {
      modeApplies = anyExtension $ modepSuffixies p,
      modeIndentSettings = modepIndentSettings p
      }


data CommandLineOptions = CommandLineOptions {
    startOnLine :: Maybe Int
  , files :: [String]
  }

commandLineOptions :: Parser (Maybe CommandLineOptions)
commandLineOptions = flag' Nothing
                       ( long "version"
                      <> short 'v'
                      <> help "Show the version number")
  <|> (Just <$> (CommandLineOptions
    <$> optional (option auto
        ( long "line"
       <> short 'l'
       <> metavar "NUM"
       <> help "Open the (last) file on line NUM"))
    <*> many (argument str (metavar "FILES..."))
  ))

main :: IO ()
main = do
    mayClo <- execParser opts
    case mayClo of
      Nothing -> putStrLn "Yi 0.17.1"
      Just clo -> do
        home <- S.getHomeDirectory
        mparam <- Y.decodeFileEither (home <> "/.yi.yml") :: IO (Either Y.ParseException UserParam)
        let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) (files clo))
            moveLineAction  = YiA $ withCurrentBuffer (lineMoveRel (fromMaybe 0 (startOnLine clo)))
        case mparam of
          Right param -> do
            cfg <- execStateT
              (runConfigM (myConfig param >> (startActionsA .= (openFileActions ++ [moveLineAction]))))
              defaultConfig
            startEditor cfg Nothing
          Left err -> putStrLn $ show err
  where
   opts = info (helper <*> commandLineOptions)
     ( fullDesc
    <> progDesc "Edit files"
    <> header "Yi - a flexible and extensible text editor written in haskell")

{-
myConfi2g :: Maybe String -> Maybe String -> ConfigM ()
myConfi2g f k = do
  -- Lookup f in the frontends list or pick the first element of the frontends list if
  -- f is nothing or do nothing if f is not found in the frontends list.
  case f of
    Nothing -> snd (head frontends)
    Just f' -> fromMaybe (return ()) (lookup f' frontends)
  -- Same as above, but then with k and keymaps
  case k of
    Nothing -> snd (head keymaps)
    Just k' -> fromMaybe (return ()) (lookup k' keymaps)
  configureHaskellMode
  configureJavaScriptMode
  configureMiscModes
  modifyModeByName "javascript" $ \mode -> mode {
    modeApplies = anyExtension ["js", "ts"],
    modeIndentSettings = IndentSettings {
          expandTabs = True
        , tabSize = 2
        , shiftWidth = 2
        }
    }-}
{-
pythonMode :: TokenBasedMode StyleName
pythonMode = base
  & modeNameA .~ "python"
  & modeAppliesA .~ anyExtension [ "py" ]
  & modeToggleCommentSelectionA .~ Just (toggleCommentB "#")
  & modeIndentSettingsA %~ (\x -> x { expandTabs = True, tabSize = 4 })
  where
    base = styleMode Python.lexer
data IndentSettings = IndentSettings
  { expandTabs :: !Bool -- ^ Insert spaces instead of tabs as possible
  , tabSize    :: !Int  -- ^ Size of a Tab
  , shiftWidth :: !Int  -- ^ Indent by so many columns
  } deriving (Eq, Show, Typeable)

-}
