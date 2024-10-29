{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module SpacedRep2 (St2 (..), appLearn2, Screen2(..), Name2(..), learnCompletion, levelMapTempText) where

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
import Brick.Util
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
import RandomUtils (shuffle, getCompletionRate)

removeElement :: Eq a => a -> [a] -> [a]
removeElement x xs = [y | y <- xs, y /= x]

-- Define the Queue type
newtype Queue a = Queue (Seq.Seq a)

-- Create an empty queue
emptyQueue :: Queue a
emptyQueue = Queue Seq.empty

-- Enqueue an element
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue seq) = Queue (seq Seq.|> x)

-- Dequeue an element
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue seq) =
    case Seq.viewl seq of
        Seq.EmptyL -> Nothing
        x Seq.:< xs -> Just (x, Queue xs)

-- Function to remove a particular value from the queue
removeValue :: Eq a => a -> Queue a -> Queue a
removeValue val (Queue seq) = Queue $ Seq.filter (/= val) seq

data Name2 = Info | Button1 | Button2 | Button3 | Button4 | Prose | TextBox | Edit2
          deriving (Show, Ord, Eq)

data St2 =
    St2 { _clicked :: [T.Extent Name2]
       , _lastReportedClick :: Maybe (Name2, T.Location)
       , _prose :: String
       , _currentScreen :: Screen2
       , _currentChoices :: [String]
       , _meanings :: Map.Map String String
       , _knowledge :: Map.Map String Int 
       , _buttonTrue :: [Int]
       , _wordsQueue :: [String]
       -- this map stores 0 for each key if the key was not introduced yet to the user, and 1 if it was
       -- introduced
       -- this map stores how many each item was reviewed by the user during a session
       , _levelMapTemp :: Map.Map String Int
       , _levelMapTempText :: Map.Map String Int
       , _buttonPressed :: Int
       , _learnCompletion :: Int
       , _possibleChoices :: [String]
       , _edit2 :: E.Editor String Name2
       , _textInputCorrect :: Int -- set to 1 when the input of the user is correct, 0 otherwise
       }

data Screen2 = Presentation | MultiChoice | TextInput | Feedback | Feedback2 deriving (Show, Eq)

makeLenses ''St2

drawUi :: St2 -> [Widget Name2]
drawUi st =
    [ buttonLayer st
    , proseLayer st
    ]

attrFunction :: Int -> AttrName
attrFunction a
    | (a == 0) = attrName "button"
    | (a == 1) = attrName "buttonCorrect"
    | (a == 2) = attrName "buttonWrong"
    | otherwise = attrName "button"

attrFunction2 :: Int -> AttrName
attrFunction2 a
    | (a == 0) = attrName "buttonWrong"
    | (a == 1) = attrName "buttonCorrect"
    | otherwise = attrName "button"

calculateDisplayString :: Int -> String -> Map.Map String String -> String
calculateDisplayString a key map = 
    case a of
        0 -> "Wrong, the correct answer was: " ++ value
        1 -> "Correct!"
        _ -> ""
    where
        value2 = Map.lookup key map
        value = case value2 of 
            Just v -> v
            Nothing -> ""

buttonLayer :: St2 -> Widget Name2
buttonLayer st 
    | (st ^. currentScreen == MultiChoice || st ^. currentScreen == Feedback) = C.vCenterLayer $
          C.hCenterLayer (padBottom (Pad 1) $ str "Click a button:") <=>
          C.hCenterLayer (hBox $ padLeftRight 1 <$> buttons) <=>
          C.hCenterLayer (padTopBottom 1 $ str "Or enter text and then click in this editor:") <=>
          C.hCenterLayer cBar 
    | (st ^. currentScreen == TextInput || st ^. currentScreen == Feedback2) = C.vCenterLayer $ C.hCenter $ hLimit 60 $ vLimit 5 $ e1 <=>
                                                                  C.hCenterLayer (padTopBottom 1 $ str $ calculateDisplayString (st ^. textInputCorrect) (st ^. prose) (st ^. meanings) ) <=>
                                                                  C.hCenterLayer cBar 
    | otherwise = C.vCenterLayer $ C.hCenterLayer (padBottom (Pad 1) $ str mnVal ) 
    where
          cBar = overrideAttr P.progressCompleteAttr cDoneAttr1 $ overrideAttr P.progressIncompleteAttr cToDoAttr1 $ bar' '▰' '▱' $ (getCompletionRate (st ^. levelMapTempText) (st ^. levelMapTemp)) 
          bar' cc ic v = P.customProgressBar cc ic Nothing v
          e1 = withDefAttr (attrFunction2 (st ^. textInputCorrect ) ) $ (E.renderEditor (str . unlines) True (st ^. edit2))          -- e1 = (E.renderEditor (str . unlines) True (st ^. edit2))
          buttons = mkButton <$> buttonData
          buttonData = [ (Button1, (st^.currentChoices) !! 0, attrFunction ((st^. buttonTrue) !! 0))
                      , (Button2, (st^.currentChoices) !! 1, attrFunction ((st^. buttonTrue) !! 1))
                      , (Button3,  (st^.currentChoices) !! 2, attrFunction ((st^. buttonTrue) !! 2))
                      , (Button4,  (st^.currentChoices) !! 3, attrFunction ((st^. buttonTrue) !! 3))
                      ]
          mnVal = case Map.lookup (st ^. prose) (st ^. meanings) of
              Just value -> value 
              Nothing -> ""
          mkButton (name, label, attr) =
              let wasClicked = (fst <$> st^.lastReportedClick) == Just name
              in clickable name $
                   withDefAttr attr $
                   B.border $
                   padTopBottom 1 $
                   padLeftRight (if wasClicked then 2 else 3) $
                   str (if wasClicked then "<" <> label <> ">" else label)



proseLayer :: St2 -> Widget Name2
proseLayer st =
  B.border $
  C.hCenterLayer $
  vLimit 4 $
  viewport Prose Vertical $
  vLimit 8 $  -- Limit the height to 8
  vBox [ C.hCenter (str line) | line <- lines (st^.prose) ]

appEventQuiz :: T.BrickEvent Name2 e -> T.EventM Name2 St2 ()
appEventQuiz ev@(T.MouseDown n _ _ loc) = do
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
                buttonPressed .= 0
            else do
                currentScreen .= Feedback
                buttonTrue .= [2,0,0,0]
        Button2 -> do
            let currStr = choices !! 1
            if currStr == strToComp then do 
                currentScreen .= Feedback
                buttonTrue .= [0,1,0,0]
                buttonPressed .= 1
            else do
                currentScreen .= Feedback
                buttonTrue .= [0,2,0,0]
        Button3 -> do 
            let currStr = choices !! 2
            if currStr == strToComp then do 
                currentScreen .= Feedback
                buttonTrue .= [0,0,1,0]
                buttonPressed .= 2
            else do
                currentScreen .= Feedback
                buttonTrue .= [0,0,2,0]
        Button4 -> do
            let currStr = choices !! 3
            if currStr == strToComp then do 
                currentScreen .= Feedback
                buttonTrue .= [0,0,0,1]
                buttonPressed .= 3
            else do
                currentScreen .= Feedback
                buttonTrue .= [0,0,0,2]
        _ -> return ()

appEvent :: T.BrickEvent Name2 e -> T.EventM Name2 St2 ()
appEvent3 :: T.BrickEvent Name2 e -> T.EventM Name2 St2 ()
appEvent ev@(T.MouseDown n _ _ loc) = do
    currScr <- use currentScreen
    if currScr == MultiChoice
        then appEventQuiz ev
        else return ()

appEvent (T.MouseUp n _ loc) = do
    -- Handle the button action on MouseUp event
    mClick <- use lastReportedClick
    case mClick of
        Just (button, _) | button == n -> appEventQuiz (T.MouseDown button V.BLeft [V.MShift] loc)
        _ -> return ()
        
appEvent (T.VtyEvent (V.EvMouseUp {})) =
    lastReportedClick .= Nothing

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

                          if userInput == strToComp
                                then do
                                    currentScreen .= Feedback2
                                    textInputCorrect %= (\x -> 1)
                                else do
                                    currentScreen .= Feedback2
                                    textInputCorrect %= (\x -> 0)
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
                        lvlTmpMap <- use levelMapTemp
                        lvlTmpMap2 <- use levelMapTempText
                        -- we select the next screen:
                        currentScreen %= (\x -> selectNextScreen2 prose2 lvlTmpMap lvlTmpMap2)
                        -- reset the value of the text input field
                        edit2 %= (\x -> (E.editor Edit2 (Just 1) ""))
                        if (allValuesBigger (Map.elems lvlTmpMap2) [1,1,1,1,1] && allValuesBigger (Map.elems lvlTmpMap) [1,1,1,1,1])
                            then do
                                learnCompletion .= 1
                                M.halt
                            else do
                                return ()

                        crScr2 <- use currentScreen
                        if crScr2 == MultiChoice 
                            then do
                                possibleChoic <- use possibleChoices
                                myMap <- use meanings
                                let currentValue = case Map.lookup prose2 myMap of
                                                        Just value -> value
                                                        Nothing -> ""

                                temp1 <- liftIO $ shuffle $ removeElement currentValue possibleChoic
                                temp2 <- liftIO $ shuffle $ currentValue : (take 3 temp1)
                                currentChoices .= temp2
                            else do
                                return ()
                            
                  Feedback -> do
                        pross <- use prose
                        buttonTr <- use buttonTrue
                        buttonPrs <- use buttonPressed
                        lvlMap <- use levelMapTemp
                        lvlMapText <- use levelMapTempText

                        if (buttonTr !! buttonPrs) == 1 then
                            levelMapTemp .= Map.adjust (+1) pross lvlMap
                        else 
                            levelMapTemp .= lvlMap
                        lvlMap2 <- use levelMapTemp 

                        screen <- use currentScreen

                        qWord <- use wordsQueue
                        let status = case (buttonTr !! buttonPrs) of
                                      1 -> 1
                                      _ -> 0

                        wordsQueue %= (\x -> reorderQueue status pross qWord)
                        qWord2 <- use wordsQueue
                        -- the next word will be the first value of our queue of words.
                        prose %= (\x -> head qWord2)
                        prose2 <- use prose
                        lvlTmpMap <- use levelMapTemp
                        lvlTmpMap2 <- use levelMapTempText
                        -- we select the next screen:
                        currentScreen %= (\x -> selectNextScreen2 prose2 lvlTmpMap lvlTmpMap2)
                        -- reset the value of the text input field
                        buttonTrue .= [0,0,0,0]
                        buttonPressed .= 0
                        if (allValuesBigger (Map.elems lvlTmpMap2) [1,1,1,1,1] && allValuesBigger (Map.elems lvlTmpMap) [1,1,1,1,1])
                            then do
                                learnCompletion .= 1
                                M.halt
                            else do
                                return ()

                        crScr2 <- use currentScreen
                        if crScr2 == MultiChoice 
                            then do
                                possibleChoic <- use possibleChoices
                                myMap <- use meanings
                                let currentValue = case Map.lookup prose2 myMap of
                                                        Just value -> value
                                                        Nothing -> ""

                                temp1 <- liftIO $ shuffle $ removeElement currentValue possibleChoic
                                temp2 <- liftIO $ shuffle $ currentValue : (take 3 temp1)
                                currentChoices .= temp2
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
      | (status == 0) = insertAt (div (length $ tail queue) 2) word (tail queue)
      | otherwise = queue

insertAt :: Int -> a -> [a] -> [a]
insertAt index value xs
    | index < 0 = xs  -- Return the original list for negative indices
    | index > length xs = xs ++ [value]  -- Append if index is greater than the length
    | otherwise = take index xs ++ [value] ++ drop index xs

selectNextScreen2 :: String -> Map.Map String Int -> Map.Map String Int -> Screen2
selectNextScreen2 currentValue m1 m2
      | (value1 >= value2) = TextInput
      | otherwise = MultiChoice
      where
          value11 = Map.lookup currentValue m1
          value1 = case value11 of 
              Just v -> v
              Nothing -> 0
          value21 = Map.lookup currentValue m2
          value2 = case value21 of 
              Just v -> v
              Nothing -> 0
          
          
isEmpty :: Queue a -> Bool
isEmpty (Queue seq) = Seq.null seq

-- Function to check if the values of the map match an array
hasValuesEqualTo :: (Ord a, Eq a, Num a) => Map.Map k a -> [a] -> Bool
hasValuesEqualTo m l = Map.elems m == l

-- Function to find the first key with a value of 0
findFirstKeyWithZero :: Ord k => Map.Map k Int -> k
findFirstKeyWithZero m = head [k | (k, v) <- Map.toList m, v == 0]

-- Function to find and remove the first key with value inp
findAdequateKey :: Queue String -> Map.Map String Int -> Int -> Maybe String
findAdequateKey q m1 inp
    | isEmpty q = Nothing  -- Return Nothing if the queue is empty
    | otherwise = 
        let mm = dequeue q  -- Dequeue the first element
        in case mm of
            Nothing -> Nothing  -- No key found, return Nothing
            Just (key, newQ) -> 
                if Map.lookup key m1 < Just inp  -- Check if the value is less than inp
                then Just key  -- Return the key if value is inp
                else findAdequateKey newQ m1 inp  -- Recur with the new queue


selectNextWord :: Queue String -> Map.Map String Int -> Map.Map String Int -> Screen2 -> String
selectNextWord q m1 m2 s 
        | (s == Presentation) = valueSer
        | otherwise = forValue2
        where 
            forValue = findAdequateKey q m1 (commonLevel - 1)
            forValue2 = case forValue of 
                Just v -> v
                Nothing -> ""
            commonLevel = Map.foldr (\v acc -> if v == 1 then acc + 1 else acc) 0 m2
            valueSer = findFirstKeyWithZero m2


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

appLearn2 :: M.App St2 e Name2
appLearn2 =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = do
              vty <- M.getVtyHandle
              liftIO $ V.setMode (V.outputIface vty) V.Mouse True
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appChooseCursor = M.showFirstCursor
          }
