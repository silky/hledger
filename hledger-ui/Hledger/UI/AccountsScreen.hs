-- The accounts screen, showing accounts and balances like the CLI balance command.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.UI.AccountsScreen
--  (screen)
where

import Control.Lens ((^.))
-- import Control.Monad
import Control.Monad.IO.Class
-- import Data.Default
import Data.Maybe
-- import Data.Monoid
import Data.Time.Calendar (Day)
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import qualified Brick.Main as M
-- import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
-- import Brick.Util (fg, on)
import Brick.Widgets.Core
  -- ( Widget(..)
  -- , hBox
  -- , vBox
  -- , str
  -- , withAttr
  -- , (<+>)
  -- )

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
-- import Hledger.Cli.Options (defaultBalanceLineFormat)
import Hledger.UI.Options
import Hledger.UI.UITypes
import Hledger.UI.UIUtils
-- import qualified Hledger.UI.RegisterScreen

-- screen = AccountsScreen{
--       asHandler = handleAccountsScreen
--      ,asDraw    = drawAccountsScreen
--      ,asLoad    = loadAccountsScreen
--      ,asItems   = L.list "" drawAccountsItem []
--      }

handleAccountsScreen :: [Screen2] -> AppState -> V.Event -> M.EventM (M.Next AppState)
handleAccountsScreen screens st@AppState{aloc=l@Loc{locScreen=Screen2{scrName,scrHandler,scrDraw,scrLoad,scrItems}, locArgs=args}} e = do
    d <- liftIO getCurrentDay
    let mregisterscreen = screenLookup "register" screens
    case e of
        V.EvKey V.KEsc []        -> M.halt st
        V.EvKey (V.KChar 'q') [] -> M.halt st
        V.EvKey (V.KLeft) []     -> M.continue $ popLoc st
        V.EvKey (V.KRight) [] | isJust mregisterscreen
                                 -> M.continue $ screenEnter d args (fromJust mregisterscreen) st
                                      -- where
                                      --   args' = case L.listSelectedElement scrItems of
                                      --            Just (_,acct) -> (show acct"):args
                                      --            Nothing       -> args
                                   
        -- fall through to the item list event handler (handles up/down)
        ev                       -> M.continue st{aloc=l{locScreen=Screen2{scrName,scrHandler,scrDraw,scrLoad,scrItems=T.handleEvent ev scrItems}}}

-- handleAccountsScreen _ _ = error "event handler called with wrong screen type, should not happen"

drawAccountsScreen :: AppState -> [Widget]
drawAccountsScreen AppState{aloc=Loc{locScreen=Screen2{scrItems}}} = [ui]
    where
      _label = _cur <+> " of " <+> _total
      _cur = case scrItems^.(L.listSelectedL) of
              Nothing -> "-"
              Just i -> str (show (i + 1))
      _total = str $ show $ length $ scrItems^.(L.listElementsL)
      _box = B.borderWithLabel _label $
            -- hLimit 25 $
            -- vLimit 15 $
            L.renderList scrItems
      ui =      L.renderList scrItems
      -- ui = C.vCenter $ vBox [ C.hCenter box
      --                       , " "
      --                       , C.hCenter "Press Esc to exit."
      --                       ]
-- drawAccountsScreen _ = error "draw function called with wrong screen type, should not happen"

drawAccountsItem :: Bool -> BalanceReportItem -> Widget
drawAccountsItem sel item =
    let selStr i = if sel
                   then withAttr customAttr (str $ showitem i)
                   else str $ showitem i
        showitem = unlines . balanceReportItemAsText defreportopts defaultBalanceLineFormat
    in
     selStr item

loadAccountsScreen :: Day -> AppState -> AppState
loadAccountsScreen d st@AppState{aopts=opts, ajournal=j, aloc=l@Loc{locScreen=Screen2{scrName,scrHandler,scrDraw,scrLoad}}} =
  st{aloc=l{locScreen=Screen2{scrName,scrHandler,scrDraw,scrLoad,scrItems=is'}}}
   where
    is' = L.list (T.Name "") drawAccountsItem items
    (items,_total) = balanceReport ropts q j
      where
        q = queryFromOpts d ropts{query_=unwords' $ locArgs l}
        ropts = reportopts_ cliopts
        cliopts = cliopts_ opts
-- loadAccountsScreen _ _ = error "load function called with wrong screen type, should not happen"

              -- RegisterScreen -> lines $ postingsReportAsText cliopts $ postingsReport ropts q j
              -- PrintScreen    -> lines $ entriesReportAsText $ entriesReport ropts q j
