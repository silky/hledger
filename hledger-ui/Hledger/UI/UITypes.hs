module Hledger.UI.UITypes where

import Data.Time.Calendar (Day)
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import Brick.Widgets.Core
  ( Widget(..)
  )

import Hledger
import Hledger.UI.Options

----------------------------------------------------------------------
-- AppState

-- | The application state when running the ui command.
-- Remember this is part of, but distinct from, brick's M.App.
data AppState = AppState {
     aopts :: UIOpts           -- ^ command-line opts at startup
    ,aargs :: [String]         -- ^ command-line args at startup
    ,ajournal :: Journal       -- ^ parsed journal
    ,aloc :: Loc               -- ^ the current screen/context
    ,ahist :: [Loc]            -- ^ the previous screens/contexts (the user's navigation trail)
    } deriving (Show)

-- currentScreen :: AppState -> Screen
-- currentScreen = locScreen . aloc

----------------------------------------------------------------------
-- Loc

-- | A location/context within the user interface - a screen, cursor position, current settings etc.
data Loc = Loc {
     locScreen :: Screen           -- ^ the currently active screen
    ,locArgs :: [String]           -- ^ the currently active query args
    } deriving (Show)

-- | Push a new UI location on to the stack.
pushLoc :: Loc -> AppState -> AppState
pushLoc l st = st'
  where
    st' = st{ahist=(aloc st:ahist st)
            ,aloc=l
            }

popLoc :: AppState -> AppState
popLoc st@AppState{ahist=l:ls} = st{aloc=l, ahist=ls}
popLoc st = st

-- clearLocs :: AppState -> AppState
-- clearLocs a = a{alocs=[]}

----------------------------------------------------------------------
-- Screen

instance Show (L.List a) where
  show _ = "<a List widget>"

-- -- | A screen within the user interface.
-- data Screen = Screen {
--    scrName :: String  -- ^ the unique name/id of this screen
--   ,scrDraw :: AppState -> [Widget]                                 -- ^ brick draw function to use while in this screen
--   ,scrHandler :: AppState -> V.Event -> M.EventM (M.Next AppState)  -- ^ brick event handler to use while in this screen
--   ,scrWidgets :: L.List ReportItem                                 -- ^ brick widgets to draw while in this screen; for now, must be a List of ReportItem
--   } deriving (Show)

-- class ReportItem

-- instance ReportItem EntriesReportItem
-- instance ReportItem PostingsReportItem
-- instance ReportItem BalanceReportItem
-- instance ReportItem MultiBalanceReportRow

-- | Types of screen within the user interface. Each one has its own data model type. This is pretty clunky.
data Screen =
    AccountsScreen {
     asDraw :: AppState -> [Widget]                                 -- ^ brick draw function
    ,asHandler :: AppState -> V.Event -> M.EventM (M.Next AppState) -- ^ brick event handler
    ,asLoad :: Day -> AppState -> AppState                          -- ^ initialise the brick data model
    ,asItems :: L.List BalanceReportItem                            -- ^ brick data model
    }
  | RegisterScreen {
     rsDraw :: AppState -> [Widget]
    ,rsHandler :: AppState -> V.Event -> M.EventM (M.Next AppState)
    ,rsLoad :: Day -> AppState -> AppState
    ,rsItems :: L.List PostingsReportItem
    }
  deriving (Show)

screenHandler :: Screen -> (AppState -> V.Event -> M.EventM (M.Next AppState))
screenHandler AccountsScreen{asHandler=f} = f
screenHandler RegisterScreen{rsHandler=f} = f

screenDraw :: Screen -> (AppState -> [Widget])
screenDraw AccountsScreen{asDraw=f} = f
screenDraw RegisterScreen{rsDraw=f} = f

screenLoad :: Day -> AppState -> AppState
screenLoad d st =
  case locScreen $ aloc st of
    AccountsScreen{asLoad=f} -> f d st
    RegisterScreen{rsLoad=f} -> f d st

-- | Enter a new screen, with possibly new args, saving the old ui
-- location in the navigation history.
screenEnter :: Day -> [String] -> Screen -> AppState -> AppState
screenEnter d args s st =
  screenLoad d $
  pushLoc Loc{locScreen=s, locArgs=args}
  st

