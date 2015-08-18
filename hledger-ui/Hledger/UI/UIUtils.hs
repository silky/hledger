----------------------------------------------------------------------
-- UIUtils

{-# LANGUAGE OverloadedStrings #-}

module Hledger.UI.UIUtils
where

-- import Control.Lens ((^.))
-- import Control.Monad
-- import Data.Default
import Data.Monoid              -- 
import qualified Graphics.Vty as V
-- import qualified Brick.Types as T
-- import qualified Brick.Main as M
import qualified Brick.AttrMap as A
-- import qualified Brick.Widgets.Border as B
-- import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import Brick.Util (
   -- fg
  on
  )
-- import Brick.Widgets.Core
--   ( Widget(..)
--   , hBox
--   , vBox
--   , str
--   , withAttr
--   , (<+>)
--   )

attrMap :: A.AttrMap
attrMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.black `on` V.white)
    -- , (customAttr,            fg V.cyan)
    ]

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

