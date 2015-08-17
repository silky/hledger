-- The register screen, showing account postings, like the CLI register command.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.UI.RegisterScreen
--  (screen)
where

import Control.Lens ((^.))
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
  -- -- , hBox
  -- , vBox
  -- , str
  -- , withAttr
  -- , (<+>)
  -- )

import Hledger
import Hledger.Cli hiding (progname,prognameandversion,green)
import Hledger.UI.Options
import Hledger.UI.UITypes
import Hledger.UI.UIUtils

-- screen = RegisterScreen{
--       rsHandler = handleRegisterScreen
--      ,rsDraw    = drawRegisterScreen
--      ,rsLoad    = loadRegisterScreen
--      ,rsItems   = L.list "" drawRegisterItem []
--      }

-- handleRegisterScreen :: AppState -> V.Event -> M.EventM (M.Next AppState)
-- handleRegisterScreen st@AppState{aloc=l@Loc{locScreen=s@RegisterScreen{rsItems=is}}} e =
--     case e of
--         V.EvKey V.KEsc []        -> M.halt st
--         V.EvKey (V.KChar 'q') [] -> M.halt st
--         V.EvKey (V.KLeft) []     -> M.continue $ popLoc st
--         -- V.EvKey (V.KRight) []    -> error (show curItem) where curItem = L.listSelectedElement is
--         -- fall through to the item list event handler (handles up/down)
--         ev                       -> M.continue st{aloc=l{locScreen=s{rsItems=T.handleEvent ev is}}}
-- handleRegisterScreen _ _ = error "event handler called with wrong screen type, should not happen"

-- drawRegisterScreen :: AppState -> [Widget]
-- drawRegisterScreen AppState{aloc=Loc{locScreen=RegisterScreen{rsItems=is}}} = [ui]
--     where
--       label = "Item " <+> cur <+> " of " <+> total
--       cur = case is^.(L.listSelectedL) of
--               Nothing -> "-"
--               Just i -> str (show (i + 1))
--       total = str $ show $ length $ is^.(L.listElementsL)
--       box = B.borderWithLabel label $
--             -- hLimit 25 $
--             -- vLimit 15 $
--             L.renderList is
--       ui = C.vCenter $ vBox [ C.hCenter box
--                             , " "
--                             , C.hCenter "Press Esc to exit."
--                             ]
-- drawRegisterScreen _ = error "draw function called with wrong screen type, should not happen"

handleRegisterScreen :: AppState -> V.Event -> M.EventM (M.Next AppState)
handleRegisterScreen st@AppState{aloc=l@Loc{locScreen=Screen2{scrName,scrHandler,scrDraw,scrLoad,scrItems}}} e =
    case e of
        V.EvKey V.KEsc []        -> M.halt st
        V.EvKey (V.KChar 'q') [] -> M.halt st
        V.EvKey (V.KLeft) []     -> M.continue $ popLoc st
        -- V.EvKey (V.KRight) []    -> error (show curItem) where curItem = L.listSelectedElement scrItems
        -- fall through to the item list event handler (handles up/down)
        --ev                       -> M.continue st{aloc=l{locScreen=s{scrItems=T.handleEvent ev scrItems}}}
        ev                       -> M.continue st{aloc=l{locScreen=Screen2{scrName,scrHandler,scrDraw,scrLoad,scrItems=T.handleEvent ev scrItems}}}
-- handleRegisterScreen _ _ = error "event handler called with wrong screen type, should not happen"

drawRegisterScreen :: AppState -> [Widget]
drawRegisterScreen AppState{aloc=Loc{locScreen=Screen2{scrItems}}} = [ui]
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
-- drawRegisterScreen _ = error "draw function called with wrong screen type, should not happen"

drawRegisterItem :: Bool -> PostingsReportItem -> Widget
-- drawRegisterItem :: Bool -> Int -> Widget
drawRegisterItem sel item = 
    let selStr i = (if sel then withAttr customAttr else id) $ str $ showitem i
        -- showitem' = show
        showitem (_,_,_,p,b) =
          postingsReportItemAsText defcliopts $
          mkpostingsReportItem False False PrimaryDate Nothing p b

-- type PostingsReportItem = (Maybe Day    -- The posting date, if this is the first posting in a
--                                         -- transaction or if it's different from the previous
--                                         -- posting's date. Or if this a summary posting, the
--                                         -- report interval's start date if this is the first
--                                         -- summary posting in the interval.
--                           ,Maybe Day    -- If this is a summary posting, the report interval's
--                                         -- end date if this is the first summary posting in
--                                         -- the interval.
--                           ,Maybe String -- The posting's transaction's description, if this is the first posting in the transaction.
--                           ,Posting      -- The posting, possibly with the account name depth-clipped.
--                           ,MixedAmount  -- The running total after this posting (or with --average, the running average).
--                           )

        w = selStr item
        -- w@Widget{_hSize,_vSize,render} = selStr item
        -- render' = do
        --   ctx <- getContext
        --   let width = ctx ^. availWidthL
        --       showitem' (_,_,_,p,b) =
        --         postingsReportItemAsText defcliopts{available_width_=width} $
        --         mkpostingsReportItem False False PrimaryDate Nothing p b
        --       selStr i = (if sel then withAttr customAttr else id) $ str $ showitem' i
        --   render
        -- w'=w{render=render'}
    in
     w
        

loadRegisterScreen :: Day -> AppState -> AppState
loadRegisterScreen d st@AppState{aopts=opts, ajournal=j, aloc=l@Loc{locScreen=Screen2{scrName,scrHandler,scrDraw,scrLoad}}} =
  st{aloc=l{locScreen=Screen2{scrName,scrHandler,scrDraw,scrLoad,scrItems=is'}}}
   where
    is' = L.list (T.Name "") drawRegisterItem items
    -- items = [1..10000]
    (_label,items) = postingsReport ropts q j
      where
        q = queryFromOpts d ropts{query_=unwords' $ locArgs l}
        ropts = reportopts_ cliopts
        cliopts = cliopts_ opts
-- loadRegisterScreen _ _ = error "load function called with wrong screen type, should not happen"
