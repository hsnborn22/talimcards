{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget, ViewportType(Vertical))
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

data Name = Info | Button1 | Button2 | Button3 | Button4 | Prose | TextBox
          deriving (Show, Ord, Eq)

data St =
    St { _clicked :: [T.Extent Name]
       , _lastReportedClick :: Maybe (Name, T.Location)
       , _prose :: String
       , _currentScreen :: Screen
       , _currentChoices :: [String]
       , _meanings :: Map.Map String String
       , _knowledge :: Map.Map String Int 
       , _buttonTrue :: [Int]
       }

data Screen = Presentation | MultiChoice | TextInput | Feedback

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
    [ buttonLayer st
    , proseLayer st
    , infoLayer st
    ]

attrFunction :: Int -> AttrName
attrFunction a
    | (a == 0) = attrName "button"
    | (a == 1) = attrName "buttonCorrect"
    | (a == 2) = attrName "buttonWrong"
    | otherwise = attrName "button"

buttonLayer :: St -> Widget Name
buttonLayer st =
    C.vCenterLayer $
      C.hCenterLayer (padBottom (Pad 1) $ str "Click a button:") <=>
      C.hCenterLayer (hBox $ padLeftRight 1 <$> buttons) <=>
      C.hCenterLayer (padTopBottom 1 $ str "Or enter text and then click in this editor:")
    where
        buttons = mkButton <$> buttonData
        buttonData = [ (Button1, (st^.currentChoices) !! 0, attrFunction ((st^. buttonTrue) !! 0))
                     , (Button2, (st^.currentChoices) !! 1, attrFunction ((st^. buttonTrue) !! 1))
                     , (Button3,  (st^.currentChoices) !! 2, attrFunction ((st^. buttonTrue) !! 2))
                     , (Button4,  (st^.currentChoices) !! 3, attrFunction ((st^. buttonTrue) !! 3))
                     ]
        mkButton (name, label, attr) =
            let wasClicked = (fst <$> st^.lastReportedClick) == Just name
            in clickable name $
               withDefAttr attr $
               B.border $
               padTopBottom 1 $
               padLeftRight (if wasClicked then 2 else 3) $
               str (if wasClicked then "<" <> label <> ">" else label)

proseLayer :: St -> Widget Name
proseLayer st =
  B.border $
  C.hCenterLayer $
  vLimit 4 $
  viewport Prose Vertical $
  vLimit 8 $  -- Limit the height to 8
  vBox [ C.hCenter (str line) | line <- lines (st^.prose) ]

infoLayer :: St -> Widget Name
infoLayer st = T.Widget T.Fixed T.Fixed $ do
    c <- T.getContext
    let h = c^.T.availHeightL
        msg = case st^.lastReportedClick of
                Nothing ->
                    "Click and hold/drag to report a mouse click"
                Just (name, T.Location l) ->
                    "Mouse down at " <> show name <> " @ " <> show l
    T.render $ translateBy (T.Location (0, h-1)) $ clickable Info $
               withDefAttr (attrName "info") $
               C.hCenter $ str msg

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent ev@(T.MouseDown n _ _ loc) = do
    lastReportedClick .= Just (n, loc)
    choices <- use currentChoices
    
    pros <- use prose 
    meaningsMap <- use meanings
    let value = Map.lookup pros meaningsMap

    let strToComp = case value of
                      Just v -> v
                      Nothing -> ""
    case n of 
        Button1 -> do
            let currStr = choices !! 0
            if currStr == strToComp then do 
                currentScreen .= Feedback
                buttonTrue .= [1,0,0,0]
            else do
                currentScreen .= Feedback
                buttonTrue .= [2,0,0,0]

        _ -> return ()
appEvent (T.MouseUp {}) =
    lastReportedClick .= Nothing
appEvent (T.VtyEvent (V.EvMouseUp {})) =
    lastReportedClick .= Nothing
appEvent (T.VtyEvent (V.EvKey V.KUp [V.MCtrl])) =
    M.vScrollBy (M.viewportScroll Prose) (-1)
appEvent (T.VtyEvent (V.EvKey V.KDown [V.MCtrl])) =
    M.vScrollBy (M.viewportScroll Prose) 1
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    M.halt
appEvent ev =
    return ()

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ (attrName "info",      V.white `on` V.magenta)
    , (attrName "button",   V.white `on` V.blue)
    , (attrName "buttonWrong",   V.white `on` V.red)
    , (attrName "buttonCorrect",   V.black `on` V.green)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    ]

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = do
              vty <- M.getVtyHandle
              liftIO $ V.setMode (V.outputIface vty) V.Mouse True
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appChooseCursor = M.showFirstCursor
          }

main :: IO ()
main = do
    let myMap = Map.fromList [("Parola da Imparare", "Scelta 2"), ("Parola da imp2", "Scelta 2")]
    let myMap2 = Map.fromList [("Parola da Imparare", 0), ("Parola da imp2", 0)]
    void $ M.defaultMain app $ St [] Nothing "Parola da Imparare" MultiChoice ["Scelta 1", "Scelta 2", "Scelta 3", "Scelta 4"] myMap myMap2 [0,0,0,0] 
