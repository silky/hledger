-- The accounts screen, showing accounts and balances like the CLI balance command.

{-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.AccountsScreen
--  (screen)
where

import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Monoid              -- 
import Data.Time.Calendar (Day)
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
  , hBox
  , vBox
  , str
  , withAttr
  , (<+>)
  )

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.Cli.Options (defaultBalanceLineFormat)
import Hledger.UI.Options
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
import qualified Hledger.UI.RegisterScreen

screen = AccountsScreen{
      asHandler = handleAccountsScreen
     ,asDraw    = drawAccountsScreen
     ,asLoad    = loadAccountsScreen
     ,asItems   = L.list "" drawAccountsItem []
     }

handleAccountsScreen :: AppState -> V.Event -> M.EventM (M.Next AppState)
handleAccountsScreen st@AppState{aloc=l@Loc{locScreen=s@AccountsScreen{asItems=is}, locArgs=args}} e = do
    d <- liftIO getCurrentDay
    case e of
        V.EvKey V.KEsc []        -> M.halt st
        V.EvKey (V.KChar 'q') [] -> M.halt st
        V.EvKey (V.KLeft) []     -> M.continue $ popLoc st
        V.EvKey (V.KRight) []    -> M.continue $ screenEnter d args Hledger.UI.RegisterScreen.screen st
                                    -- where _curItem = L.listSelectedElement is

        -- fall through to the item list event handler (handles up/down)
        ev                       -> M.continue st{aloc=l{locScreen=s{asItems=T.handleEvent ev is}}}

handleAccountsScreen _ _ = error "event handler called with wrong screen type, should not happen"

drawAccountsScreen :: AppState -> [Widget]
drawAccountsScreen AppState{aloc=Loc{locScreen=AccountsScreen{asItems=is}}} = [ui]
    where
      label = "Item " <+> cur <+> " of " <+> total
      cur = case is^.(L.listSelectedL) of
              Nothing -> "-"
              Just i -> str (show (i + 1))
      total = str $ show $ length $ is^.(L.listElementsL)
      box = B.borderWithLabel label $
            -- hLimit 25 $
            -- vLimit 15 $
            L.renderList is
      ui = C.vCenter $ vBox [ C.hCenter box
                            , " "
                            , C.hCenter "Press Esc to exit."
                            ]
drawAccountsScreen _ = error "draw function called with wrong screen type, should not happen"

drawAccountsItem :: Bool -> BalanceReportItem -> Widget
drawAccountsItem sel item =
    let selStr i = if sel
                   then withAttr customAttr (str $ showitem i)
                   else str $ showitem i
        showitem = unlines . balanceReportItemAsText defreportopts defaultBalanceLineFormat
    in
     selStr item

loadAccountsScreen :: Day -> AppState -> AppState
loadAccountsScreen d st@AppState{aopts=opts, ajournal=j, aloc=l@Loc{locScreen=s@AccountsScreen{}}} =
  st{aloc=l{locScreen=s{asItems=is'}}}
   where
    is' = L.list (T.Name "") drawAccountsItem items
    (items,_total) = balanceReport ropts q j
      where
        q = queryFromOpts d ropts{query_=unwords' $ locArgs l}
        ropts = reportopts_ cliopts
        cliopts = cliopts_ opts
loadAccountsScreen _ _ = error "load function called with wrong screen type, should not happen"

              -- RegisterScreen -> lines $ postingsReportAsText cliopts $ postingsReport ropts q j
              -- PrintScreen    -> lines $ entriesReportAsText $ entriesReport ropts q j
