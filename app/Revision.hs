{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Revision (St3(..), appRevision, Name3(..), learnCompletion, Screen3(..) , levelMapTempText) where

import Lens.Micro ((^.))
import Data.Maybe (listToMaybe)
import Data.List (sort,foldl', nub)
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.AttrMap as A
import qualified Brick.Widgets.ProgressBar as P
import Brick.Util (fg, bg, on, clamp)
import Brick.Types (Widget, ViewportType(Vertical))
import qualified Brick.Main as M
import qualified Data.Text.Zipper as TextZipper
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core (translateBy, clickable, viewport, hLimit, hBox, padBottom ,padTopBottom, padTop, padLeftRight, str, vLimit, txt, withDefAttr, emptyWidget, Padding(..), vBox, strWrap, updateAttrMap, overrideAttr , (<=>))
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import System.Random (randomRIO)
import RandomUtils (shuffle, getCompletionRateSingle)

data Name3 = Info | Button1 | Button2 | Button3 | Button4 | Prose | TextBox | Edit2
          deriving (Show, Ord, Eq)

data St3 =
    St3 { _clicked :: [T.Extent Name3]
       , _lastReportedClick :: Maybe (Name3, T.Location)
       , _prose :: String
       , _currentScreen :: Screen3
       , _meanings :: Map.Map String String
       , _knowledge :: Map.Map String Int 
       , _wordsQueue :: [String]
       -- this map stores 0 for each key if the key was not introduced yet to the user, and 1 if it was
       -- introduced
       -- this map stores how many each item was reviewed by the user during a session
       , _levelMapTempText :: Map.Map String Int
       , _learnCompletion :: Int
       , _edit2 :: E.Editor String Name3
       , _textInputCorrect :: Int -- set to 1 when the input of the user is correct, 0 otherwise
       , _levelMapError :: Map.Map String Int -- this map contains how many times we got words wrong
       }

data Screen3 = Presentation | MultiChoice | TextInput | Feedback | Feedback2 deriving (Show, Eq)

makeLenses ''St3

drawUi :: St3 -> [Widget Name3]
drawUi st =
    [ buttonLayer st
    , proseLayer st
    ]

attrFunction2 :: Int -> AttrName
attrFunction2 a
    | (a == 0) = attrName "buttonWrong"
    | (a == 1) = attrName "buttonCorrect"
    | otherwise = attrName "button"

calculateDisplayString :: Int -> String -> Map.Map String String -> String
calculateDisplayString a key map = 
    case a of
        0 -> "Wrong, the correct answer was: " ++ key
        1 -> "Correct!"
        _ -> ""

buttonLayer :: St3 -> Widget Name3
buttonLayer st 
    | (st ^. currentScreen == TextInput || st ^. currentScreen == Feedback2) = C.vCenterLayer $ C.hCenter $ hLimit 60 $ vLimit 5 $ e1 <=>
                                                                  C.hCenterLayer (padTopBottom 1 $ str $ calculateDisplayString (st ^. textInputCorrect) (st ^. prose) (st ^. meanings) ) <=>
                                                                  C.hCenterLayer (padTop (Pad 2) cBar)
    | otherwise = C.vCenterLayer $ C.hCenterLayer (padBottom (Pad 1) $ str mnVal ) 
    where
          e1 = withDefAttr (attrFunction2 (st ^. textInputCorrect ) ) $ (E.renderEditor (str . unlines) True (st ^. edit2))          -- e1 = (E.renderEditor (str . unlines) True (st ^. edit2))
          cBar = overrideAttr P.progressCompleteAttr cDoneAttr1 $ overrideAttr P.progressIncompleteAttr cToDoAttr1 $ bar' '▰' '▱' $ (getCompletionRateSingle (st ^. levelMapTempText)) 
          bar' cc ic v = P.customProgressBar cc ic Nothing v
          mnVal = case Map.lookup (st ^. prose) (st ^. meanings) of
              Just value -> value 
              Nothing -> ""

proseLayer :: St3 -> Widget Name3
proseLayer st =
  B.border $
  C.hCenterLayer $
  vLimit 4 $
  viewport Prose Vertical $
  vLimit 8 $  -- Limit the height to 8
  vBox [ C.hCenter (str line ) | line <- lines (wordOrLang) ]
  where
      word = Map.lookup (st^.prose) (st ^. meanings)
      wordOrLang = case word of 
          Just v -> v
          Nothing -> ""

appEvent (T.VtyEvent e) = 
    case e of 
          V.EvKey V.KEsc [] -> M.halt
          V.EvKey V.KEnter [] -> do
              currScr <- use currentScreen
              case currScr of 
                  TextInput -> do
                          pros <- use prose 
                          meaningsMap <- use meanings
                          let value = Map.lookup pros meaningsMap

                          let strToComp = case value of
                                            Just v -> v
                                            Nothing -> ""
                          editorContent <- use (edit2 . E.editContentsL)
                          let userInput = head $ TextZipper.getText editorContent

                          if userInput == pros
                                then do
                                    currentScreen .= Feedback2
                                    textInputCorrect %= (\x -> 1)
                                else do
                                    currentScreen .= Feedback2
                                    textInputCorrect %= (\x -> 0)

                                    levelMapError %= (\x -> Map.adjust (+1) pros x)

                  Feedback2 -> do
                        status <- use textInputCorrect
                        qWord <- use wordsQueue
                        currentWord <- use prose
                        wordsQueue %= (\x -> reorderQueue status currentWord qWord)
                        levelMapTempText %= (\x -> Map.alter (fmap (+status)) currentWord x) 
                        textInputCorrect %= (\x -> 2)
                        -- we get the updated words queue
                        qWord2 <- use wordsQueue
                        -- the next word will be the first value of our queue of words.
                        prose %= (\x -> head qWord2)
                        prose2 <- use prose
                        lvlTmpMap2 <- use levelMapTempText
                        currentScreen .= TextInput
                        -- we select the next screen:
                        -- reset the value of the text input field
                        edit2 %= (\x -> (E.editor Edit2 (Just 1) ""))
                        let zeros = replicate (length (Map.elems lvlTmpMap2)) 0 
                        if (allValuesBigger (Map.elems lvlTmpMap2) zeros )
                            then do
                                learnCompletion .= 1
                                M.halt
                            else do
                                return ()

                  _ -> return()
 
          ev -> do
              currScr <- use currentScreen
              if currScr == TextInput
                    then do
                        zoom edit2 $ E.handleEditorEvent (T.VtyEvent ev)
                    else do
                        appEvent3 (T.VtyEvent e)
          _ -> return ()

appEvent ev = return()

appEvent3 (T.VtyEvent (V.EvKey V.KUp [V.MCtrl])) =
    M.vScrollBy (M.viewportScroll Prose) (-1)
appEvent3 (T.VtyEvent (V.EvKey V.KDown [V.MCtrl])) =
    M.vScrollBy (M.viewportScroll Prose) 1
appEvent3 (T.VtyEvent (V.EvKey V.KEsc [])) =
    M.halt
appEvent3 (T.VtyEvent (V.EvKey V.KEnter [])) = return ()
appEvent3 ev = return()
  
allValuesBigger :: [Int] -> [Int] -> Bool
allValuesBigger arr1 arr2 = all (\(x,y) -> x > y) (zip arr1 arr2)

reorderQueue :: Int -> String -> [String] -> [String]
reorderQueue status word queue 
      | (status == 1) = (tail queue) ++ queue 
      | (status == 0) = insertAt 2 word (tail queue)
      | otherwise = queue

insertAt :: Int -> a -> [a] -> [a]
insertAt index value xs
    | index < 0 = xs  -- Return the original list for negative indices
    | index > length xs = xs ++ [value]  -- Append if index is greater than the length
    | otherwise = take index xs ++ [value] ++ drop index xs

cDoneAttr1, cToDoAttr1 :: A.AttrName
cDoneAttr1 = A.attrName "C1:done"
cToDoAttr1 = A.attrName "C1:remaining"

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ (attrName "info",      V.white `on` V.magenta)
    , (attrName "button",   V.white `on` V.blue)
    , (attrName "buttonWrong",   V.white `on` V.red)
    , (attrName "buttonCorrect",   V.black `on` V.green)
    , (attrName "standard", V.black `on` V.yellow)
    , (cDoneAttr1,                fg V.blue)
    , (cToDoAttr1,                fg V.brightWhite)
    , (P.progressIncompleteAttr,  fg V.yellow)
    ]

appRevision :: M.App St3 e Name3
appRevision =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = do
              vty <- M.getVtyHandle
              liftIO $ V.setMode (V.outputIface vty) V.Mouse True
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appChooseCursor = M.showFirstCursor
          }
