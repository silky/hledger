{-|
hledger-ui - a hledger add-on providing a curses-style interface.
Copyright (c) 2007-2015 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hledger.UI.Main where

-- import Control.Applicative
import Control.Lens ((^.))
import Control.Monad
import Data.Default
import Data.Monoid              -- 
-- import Data.List
-- import Data.Maybe
import Data.Time.Calendar
-- import Safe
import System.Exit

import qualified Graphics.Vty as V
import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import Brick.Util (fg, on)
import Brick.Widgets.Core
  ( Widget(..)
  -- , ViewportType(Horizontal, Vertical, Both)
  -- , hLimit
  -- , vLimit
  -- , viewport
  , hBox
  , vBox
  , str
  , withAttr
  , (<+>)
  )

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.Options
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import qualified Hledger.UI.AccountsScreen
import qualified Hledger.UI.RegisterScreen

-- | The available screens.
appScreens = [
   Hledger.UI.AccountsScreen.screen
  ,Hledger.UI.RegisterScreen.screen
  ]

----------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  opts <- getHledgerUIOpts
  -- when (debug_ $ cliopts_ opts) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show opts)
  run opts
    where
      run opts
          | "help" `inRawOpts` (rawopts_ $ cliopts_ opts)            = putStr (showModeHelp uimode) >> exitSuccess
          | "version" `inRawOpts` (rawopts_ $ cliopts_ opts)         = putStrLn prognameandversion >> exitSuccess
          | "binary-filename" `inRawOpts` (rawopts_ $ cliopts_ opts) = putStrLn (binaryfilename progname)
          | otherwise                                                = withJournalDo' opts runBrickUi

withJournalDo' :: UIOpts -> (UIOpts -> Journal -> IO ()) -> IO ()
withJournalDo' opts cmd = do
  -- journalFilePathFromOpts (cliopts_ opts) >>= readJournalFile Nothing >>=
  --   either error' (cmd opts . journalApplyAliases (aliasesFromOpts $ cliopts_ opts))
  -- XXX head should be safe for now
  (head `fmap` journalFilePathFromOpts (cliopts_ opts)) >>= readJournalFile Nothing Nothing True >>=
    either error' (cmd opts . journalApplyAliases (aliasesFromOpts $ cliopts_ opts))

runBrickUi :: UIOpts -> Journal -> IO ()
runBrickUi opts j = do
  d <- getCurrentDay

  let
    args = words' $ query_ $ reportopts_ $ cliopts_ opts

    st :: AppState
    st = screenEnter d args (head appScreens)
         AppState{
            aopts=opts
           ,aargs=args
           ,ajournal=j
           ,aloc=Loc{locScreen=head appScreens, locArgs=args}
           ,ahist=[]
           }

    app :: M.App (AppState) V.Event
    app = M.App {
        M.appLiftVtyEvent = id
      , M.appStartEvent   = return
      , M.appAttrMap      = const attrMap
      , M.appChooseCursor = M.showFirstCursor
      , M.appHandleEvent  = \st ev -> (screenHandler $ locScreen $ aloc st) st ev
      , M.appDraw         = \st -> (screenDraw $ locScreen $ aloc st) st
      }

  void $ M.defaultMain app st
