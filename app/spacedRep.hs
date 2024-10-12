{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

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
import System.Random (randomRIO)
import RandomUtils (shuffle)

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
       , _wordsQueue :: Queue String
       -- this map stores 0 for each key if the key was not introduced yet to the user, and 1 if it was
       -- introduced
       , _introducedMap :: Map.Map String Int
       -- this map stores how many each item was reviewed by the user during a session
       , _levelMapTemp :: Map.Map String Int
       , _buttonPressed :: Int
       , _learnCompletion :: Int
       , _possibleChoices :: [String]
       }

data Screen = Presentation | MultiChoice | TextInput | Feedback deriving (Show, Eq)

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
buttonLayer st 
    | (st ^. currentScreen == MultiChoice || st ^. currentScreen == Feedback) = C.vCenterLayer $
          C.hCenterLayer (padBottom (Pad 1) $ str "Click a button:") <=>
          C.hCenterLayer (hBox $ padLeftRight 1 <$> buttons) <=>
          C.hCenterLayer (padTopBottom 1 $ str "Or enter text and then click in this editor:")
    | otherwise = C.vCenterLayer $ C.hCenterLayer (padBottom (Pad 1) $ str mnVal ) 
    where
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

appEventQuiz :: T.BrickEvent Name e -> T.EventM Name St ()
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

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
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

appEvent (T.VtyEvent (V.EvKey V.KUp [V.MCtrl])) =
    M.vScrollBy (M.viewportScroll Prose) (-1)
appEvent (T.VtyEvent (V.EvKey V.KDown [V.MCtrl])) =
    M.vScrollBy (M.viewportScroll Prose) 1
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    M.halt
appEvent (T.VtyEvent (V.EvKey V.KEnter [])) = do
    screenInfo <- use currentScreen
    case screenInfo of 
        Presentation -> do
            -- after pressing enter of a presentation of a word, we queue the word and set the word status
            -- to presented (1)
            pross <- use prose
            intrMap <- use introducedMap
            wrdQueu <- use wordsQueue
            lvlMap <- use levelMapTemp
            introducedMap .= Map.adjust (+1) pross intrMap
            intrMap2 <- use introducedMap
            currentScreen .= selectNextScreen lvlMap intrMap2
            screen2 <- use currentScreen
            wrdQueu2 <- use wordsQueue
            prose .= selectNextWord wrdQueu2 lvlMap intrMap2 screen2 
            pross2 <- use prose
            wordsQueue .= removeValue pross2 wrdQueu2
            wrdQueu3 <- use wordsQueue
            wordsQueue .= enqueue pross2 wrdQueu3
            -- shuffle the possible choices
            possibleChoic <- use possibleChoices
            myMap <- use meanings 
            let currentValue = case Map.lookup pross2 myMap of
                                      Just value -> value
                                      Nothing -> ""

            temp1 <- liftIO $ shuffle $ removeElement currentValue possibleChoic
            temp2 <- liftIO $ shuffle $ currentValue : (take 3 temp1) 
            currentChoices .= temp2 
        Feedback -> do 
             
            pross <- use prose
            intrMap <- use introducedMap
            wrdQueu <- use wordsQueue
            lvlMap <- use levelMapTemp 
            
            buttonTr <- use buttonTrue
            buttonPrs <- use buttonPressed
            
            if ((buttonTr !! buttonPrs) == 1) then 
                levelMapTemp .= Map.adjust (+1) pross lvlMap
            else
                levelMapTemp .= lvlMap
            lvlMap2 <- use levelMapTemp
            
            currentScreen .= selectNextScreen lvlMap2 intrMap
            screen2 <- use currentScreen
            prose .= selectNextWord wrdQueu lvlMap2 intrMap screen2
            pross2 <- use prose
            wordsQueue .= enqueue pross2 (removeValue pross2 wrdQueu)
            buttonTrue .= [0,0,0,0]
            buttonPressed .= 0

            if (hasValuesEqualTo lvlMap2 [4,4,4,4,4]) then do
                learnCompletion .= 1
                M.halt
            else do
                return ()

            -- shuffle the possible choices
            possibleChoic <- use possibleChoices
            myMap <- use meanings 
            let currentValue = case Map.lookup pross2 myMap of
                                      Just value -> value
                                      Nothing -> ""

            temp1 <- liftIO $ shuffle $ removeElement currentValue possibleChoic
            temp2 <- liftIO $ shuffle $ currentValue : (take 3 temp1) 
            currentChoices .= temp2 
            
        _ -> return ()
appEvent ev =
  return ()
  
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

selectNextScreen :: Map.Map String Int -> Map.Map String Int -> Screen 
selectNextScreen m1 m2 
        | (hasValuesEqualTo m1 [0,0,0,0,0] && hasValuesEqualTo m2 [1,0,0,0,0]) = Presentation
        | (hasValuesEqualTo m1 [1,1,0,0,0] && hasValuesEqualTo m2 [1,1,0,0,0]) = Presentation
        | (hasValuesEqualTo m1 [2,2,2,0,0] && hasValuesEqualTo m2 [1,1,1,0,0]) = Presentation
        | (hasValuesEqualTo m1 [3,3,3,3,0] && hasValuesEqualTo m2 [1,1,1,1,0]) = Presentation
        | otherwise = MultiChoice

selectNextWord :: Queue String -> Map.Map String Int -> Map.Map String Int -> Screen -> String
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
    let myMap = Map.fromList [("Parola da Imparare", "Scelta 1"), ("Parola da imp2", "Scelta 2"), ("Parola da imp3", "Scelta 3"), ("Parola da imp4", "Scelta 4"), ("Parola da imp5", "Scelta 1")]
    let myMap2 = Map.fromList [("Parola da Imparare", 0), ("Parola da imp2", 0),  ("Parola da imp3", 0), ("Parola da imp4", 0 ), ("Parola da imp5", 0)]
    let myMap3 = Map.fromList [("Parola da Imparare", 0), ("Parola da imp2", 0),  ("Parola da imp3", 0), ("Parola da imp4", 0 ), ("Parola da imp5", 0)]
    let myMap4 = Map.fromList [("Parola da Imparare", 0), ("Parola da imp2", 0),  ("Parola da imp3", 0), ("Parola da imp4", 0 ), ("Parola da imp5", 0)]
    void $ M.defaultMain app $ St [] Nothing "Parola da Imparare" Presentation ["Scelta 1", "Scelta 2", "Scelta 3", "Scelta 4"] myMap myMap2 [0,0,0,0] (enqueue "Parola da Imparare" emptyQueue) myMap3 myMap4 0 0 ["Scelta 1", "Scelta 2", "Scelta 3", "Scelta 4", "junk", "more junk", "idk", "HAHA"]  
