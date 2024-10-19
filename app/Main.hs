{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Main where

import qualified Control.Exception as E
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec

import System.IO
import qualified Data.Map as Map
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH
import Data.Maybe (fromMaybe)
import Control.Monad.State (get, liftIO, void)
import qualified Data.Text.Zipper as TextZipper
import qualified Data.Text as Text
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import Brick.AttrMap (AttrName, attrName, attrMap)
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Brick.Widgets.Center
  ( center
  , hCenter
  , vCenter
  )
import Brick.Widgets.Border
  ( borderWithLabel
  )
import Brick.Widgets.Core
  ( vBox, (<=>), padTop, hBox
  , hLimit,str , vLimit, txt
  , withDefAttr, emptyWidget
  , Padding(..)
  )
import Debug.Trace (trace)
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.AttrMap as A
import Data.Time
import Data.Time.Format (defaultTimeLocale)
import Brick.Util (on, fg)
import qualified Brick.Types as T
import qualified Brick.Widgets.Table as Table
import qualified Data.Sequence as Seq
import FlashMap (trim, trimQuotes, parseCommaPairs, parseDate, parseDates, writeCommaPairs, localTimeToString, convertDateMapToStrMap, writeDatePairs, parseKnowledgePairs, parseDatePairs, changeKey)
import SpacedRep (appLearn, St(..), drawUi, aMap, Screen(..), emptyQueue, enqueue, dequeue) 
import RandomUtils (shuffle)


createMap :: (Ord k) => [k] -> Map.Map k Int
createMap keys = Map.fromList [(key, 0) | key <- keys]

createMapNothing :: (Ord k) => [k] -> Map.Map k (Maybe LocalTime)
createMapNothing keys = Map.fromList [(key, Nothing) | key <- keys]

data Row = Row String String String String 

-- |runApp function
-- this function will run the app passed as an input
runApp :: M.App AppState String () -> AppState -> IO ()
runApp app state = void $ M.defaultMain app state 

data Name = FileBrowser1 | Edit1 | List1 | Info | Button1 | Button2 | Button3 | Button4 | Prose | TextBox
          deriving (Eq, Show, Ord)

data ExitState = ExitOpt | Learn1Opt | Learn2Opt | ReviewOpt 
          deriving (Eq, Show, Ord)

data AppState = AppState {_currentFilePath :: String
    ,_currentFileContent :: String
    ,_meanings :: Map.Map String String
    ,_knowledge :: Map.Map String Int
    ,_revision :: Map.Map String (Maybe LocalTime)
    ,_tabularList :: L.List Name  Row
    ,_colIndex :: Int
    ,_isEdit :: Bool
    ,_edit1 :: E.Editor String Name 
    ,_exitState :: ExitState
    ,_learnError :: Int
    ,_outboundMeanings :: Map.Map String String
    ,_outboundKnowledge :: Map.Map String Int
    ,_outboundOptions :: [String]
}

makeLenses ''AppState

drawUI :: FB.FileBrowser Name -> [Widget Name]
drawUI b = [center $ ui <=> help]
    where
        ui = hCenter $
             vLimit 15 $
             hLimit 50 $
             borderWithLabel (txt "Choose a file") $
             FB.renderFileBrowser True b
        help = padTop (Pad 1) $
               vBox [ case FB.fileBrowserException b of
                          Nothing -> emptyWidget
                          Just e -> hCenter $ withDefAttr errorAttr $
                                    txt $ Text.pack $ E.displayException e
                    , hCenter $ txt "Up/Down: select"
                    , hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
                    , hCenter $ txt "Enter: change directory or select file"
                    , hCenter $ txt "Esc: quit"
                    ]

appEvent :: BrickEvent Name e -> T.EventM Name (FB.FileBrowser Name) ()
appEvent (VtyEvent ev) = do
    b <- get
    case ev of
        V.EvKey V.KEsc [] | not (FB.fileBrowserIsSearching b) ->
            M.halt
        _ -> do
            FB.handleFileBrowserEvent ev
            -- If the browser has a selected file after handling the
            -- event (because the user pressed Enter), shut down.
            case ev of
                V.EvKey V.KEnter [] -> do
                    b' <- get
                    case FB.fileBrowserSelection b' of
                        [] -> return ()
                        _ -> M.halt
                _ -> return ()
appEvent _ = return ()

-- Define the second app
appB :: M.App AppState String Name
appB = M.App
    { M.appDraw = drawUIB
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = handleEventB
    , M.appStartEvent = return ()
    , M.appAttrMap = const theMapB
    }

createUnnamedEditor :: Maybe Int -> String -> E.Editor String ()
createUnnamedEditor maxLines initialContent = E.editor () maxLines initialContent

drawUIB :: AppState -> [Widget Name]
drawUIB s = [ui]
    where
        e1 = (E.renderEditor (str . unlines) True (s^.edit1))
        l = s^.tabularList
        label = str $ "Row " <> cur <> " / col " <> show (s^.colIndex + 1)
        cur = case l^.(L.listSelectedL) of
                Nothing -> "-"
                Just i -> show (i + 1)
        box = borderWithLabel label $
              hLimit totalWidth $
              vLimit 15 $
              listDrawElement 0 False headerRow <=>
              L.renderList (listDrawElement (s^.colIndex)) True l
        ui = if s ^. isEdit 
             then vCenter $ vBox [ hCenter box
                              , str " "
                              , hCenter $ str "Press +/- to add/remove list elements."
                              , hCenter $ str "Use arrow keys to change selection."
                              , hCenter $ str "Press Esc to exit edit mode."
                              , str " "
                              , hCenter $ hLimit 30 $ vLimit 5 e1
                              ]
               else vCenter $ vBox [hCenter box  
                              , str " "
                              , hCenter $ str "Press +/- to add/remove list elements."
                              , hCenter $ str "Use arrow keys to change selection."
                              , hCenter $ str "Press Esc to exit."
                              , hCenter $ str (learningMessage (s ^. learnError))
                              ]

learningMessage :: Int -> String
learningMessage status
            | (status == 0) = ""
            | otherwise = "Cannot start learning session: too few unknown cards (at least 5 unknown cards are needed)."

handleEventB :: T.BrickEvent Name e -> T.EventM Name AppState ()
handleEventB (VtyEvent e) =
    case e of
        V.EvKey (V.KChar '+') [] -> do
            els <- use (tabularList.L.listElementsL)
            let el = Row (show pos) (show $ pos * 3) (show 0) "Nothing"
                pos = Vec.length els
            tabularList %= L.listInsert pos el
            meaningMap <- use meanings
            knowledgeMap <- use knowledge
            revisionMap <- use revision
            let updatedMeanings = Map.insert (show pos) (show $ pos * 3) meaningMap
            let updatedKnowledge = Map.insert (show pos) 0 knowledgeMap
            let updatedRevision = Map.insert (show pos) Nothing revisionMap
            -- update the maps storing the key-value pairs for flashcard front-back
            -- knowledge levels and next revision date
            meanings .= updatedMeanings
            knowledge .= updatedKnowledge
            revision .= updatedRevision
            -- Update the files to keep the files and the maps in the program consistent.
            -- get the filepaths for the files to update
            currentFile <- use currentFilePath 

            let currentFile2 = "flashcardsKnowledge/" ++ (intercalate "/" (reverse $ take 2 $ reverse $ splitOn "/" currentFile ))
            let currentFile3 = "flashcardsRevision/" ++ (intercalate "/" (reverse $ take 2 $ reverse $ splitOn "/" currentFile ))
            liftIO $ writeCommaPairs updatedMeanings currentFile
            liftIO $ writeCommaPairs (Map.map show updatedKnowledge) currentFile2
            liftIO $ writeDatePairs updatedRevision currentFile3
             

        V.EvKey (V.KChar '-') [] -> do
            sel <- use (tabularList.L.listSelectedL)
            lst <- use tabularList
            meaningMap <- use meanings
            knowledgeMap <- use knowledge
            revisionMap <- use revision
            let currentElWrap = L.listSelectedElement lst
            case currentElWrap of
                Just (index, row) -> do
                        -- Successfully extracted the Row
                        let Row first sec th forth = row
                        let updatedMeanings = Map.delete first meaningMap
                        let updatedKnowledge = Map.delete first knowledgeMap
                        let updatedRevision = Map.delete first revisionMap

                        meanings .= updatedMeanings
                        knowledge .= updatedKnowledge
                        revision .= updatedRevision 
                        currentFile <- use currentFilePath 

                        let currentFile2 = "flashcardsKnowledge/" ++ (intercalate "/" (reverse $ take 2 $ reverse $ splitOn "/" currentFile ))
                        let currentFile3 = "flashcardsRevision/" ++ (intercalate "/" (reverse $ take 2 $ reverse $ splitOn "/" currentFile ))
                        liftIO $ writeCommaPairs updatedMeanings currentFile
                        liftIO $ writeCommaPairs (Map.map show updatedKnowledge) currentFile2
                        liftIO $ writeDatePairs updatedRevision currentFile3

                Nothing -> do
                        -- Handle the case where there is no selected element
                        liftIO $ putStrLn "No element selected."
            case sel of
                Nothing -> return ()
                Just i -> tabularList %= L.listRemove i

        V.EvKey V.KLeft [] -> do
            isEditing <- use isEdit
            if not isEditing 
                then do
                    colIndex %= (\i -> max 0 (i - 1))
                else do
                    zoom edit1 $ E.handleEditorEvent (VtyEvent e)

        V.EvKey V.KRight [] -> do
            isEditing <- use isEdit
            if not isEditing
                then do 
                    colIndex %= (\i -> min (length columnAlignments - 1) (i + 1))
                else do 
                    zoom edit1 $ E.handleEditorEvent (VtyEvent e)

        V.EvKey (V.KChar 'm') [] -> do
            isEditing <- use isEdit
            if not isEditing 
                then do
                    isEdit %= (\x -> True)    
                else do
                    zoom edit1 $ E.handleEditorEvent (VtyEvent e)

        -- if the user presses the key l, we start a learning session.
        V.EvKey (V.KChar 'l') [] -> do
            mean <- use meanings
            know <- use knowledge
            -- this list contains all the words with knowledge 0, i.e. words we have not learned yet.
            let zeroValues = Map.keys $ Map.filter (== 0) know 
            -- we check that there are at least 5 words we have not learned yet for the learning session
            if (length zeroValues >= 5) 
                then do
                    let outboundValues = take 5 zeroValues -- take the 5 words we'll learn
                    outboundMeanings %= (\x ->  Map.filterWithKey (\k _ -> k `elem` outboundValues) mean)
                    outboundKnowledge %= (\x ->  Map.filterWithKey (\k _ -> k `elem` outboundValues) know)
                    outboundOptions %= (\x -> Map.elems mean) 
                    outss <- use outboundOptions
                    -- we proceed with the start of the session
                    exitState .= Learn1Opt
                    M.halt
                else do
                    -- we display a message for the user, inviting him to add new words.
                    learnError .= 1

            

        V.EvKey V.KEsc [] -> do
            isEditing <- use isEdit
            if isEditing
                then do 
                    isEdit %= (\x -> False)
                    edit1 %= (\x -> (E.editor Edit1 (Just 1) "")) 
                else do 
                    exitState .= ExitOpt 
                    M.halt

        -- handle logic for when enter key is pressed
        V.EvKey V.KEnter [] -> do
            -- retrieve the boolean flag telling us whether we're currently editing or not
            isEditing <- use isEdit
            -- retrieve the current column
            column <- use colIndex
            sel <- use (tabularList.L.listSelectedL)
            -- if the current column is 0 or 1 (i.e. the key-value pair for a flashcard)
            -- then allow for modifying to occur
            if (isEditing && (column == 0 || column == 1)) 
                then do
                    case sel of 
                        Nothing -> do
                            isEdit %= (\x -> False)
                            edit1 %= (\x -> (E.editor Edit1 (Just 1) "")) 
                        Just idx -> do
                            editorContent <- use (edit1 . E.editContentsL)
                            let newValue = head $ TextZipper.getText editorContent

                            els <- use (tabularList.L.listElementsL)
                            let Row first second third fourth = els Vec.! idx  -- Get the currently selected Row

                            let updatedRow = case column of
                                    0 -> Row newValue second third fourth
                                    1 -> Row first newValue third fourth
                                    _ -> Row first second third fourth  -- No update for other columns
                            
                            currMap <- use meanings 
                            currKnow <- use knowledge
                            currRevision <- use revision
                            -- knowledge revision
                            let newMap = case column of 
                                    0 -> (changeKey first newValue currMap)
                                    1 ->  Map.adjust (const newValue) first currMap
                                    _ -> currMap
                            let newKnowledge = case column of 
                                    0 -> (changeKey first newValue currKnow)
                                    _ -> currKnow
                            let newRevision = case column of
                                    0 -> (changeKey first newValue currRevision)
                                    _ -> currRevision
                            currList <- use tabularList
                            tabularList %= (\x -> (L.listModify (\currentRow -> updatedRow) currList))
                            -- modify the files storing the flashcard data 
                            -- we are going to create IO side effects, thus we will use liftIO
                            currentFile <- use currentFilePath 

                            let currentFile2 = "flashcardsKnowledge/" ++ (intercalate "/" (reverse $ take 2 $ reverse $ splitOn "/" currentFile ))
                            let currentFile3 = "flashcardsRevision/" ++ (intercalate "/" (reverse $ take 2 $ reverse $ splitOn "/" currentFile ))
                            liftIO $ writeCommaPairs newMap currentFile
                            liftIO $ writeCommaPairs (Map.map show newKnowledge) currentFile2
                            liftIO $ writeDatePairs newRevision currentFile3
                            meanings .= newMap
                            knowledge .= newKnowledge
                            revision .= newRevision

                            -- stop editing and reset the string stored in the input field.
                            isEdit %= (\x -> False)
                            edit1 %= (\x -> (E.editor Edit1 (Just 1) "")) 

                else 
                    -- if the current column is 2 or 3 (i.e. the fields corresponding to knowledge level
                    -- or next revision date) then ignore enter, since we want to avoid letting the user
                    -- modify these values to avoid bugs (one has to be in a restricted int size and the other
                    -- is a DateTime struct wrapped in a Maybe monad) .
                    return ()

        ev -> do 
            isEditing <- use isEdit
            if isEditing 
                then do
                    zoom edit1 $ E.handleEditorEvent (VtyEvent ev)
                else do
                    T.zoom tabularList $ L.handleListEvent ev
handleEventB _ = return ()

listDrawElement :: Int -> Bool -> Row -> Widget Name
listDrawElement colIdx sel (Row a b c d) =
    let ws = [str a, str b, str c, str d]
        maybeSelect es = selectCell <$> zip [0..] es
        selectCell (i, w) = if sel && i == colIdx
                            then withDefAttr selectedCellAttr w
                            else w
    in hLimit totalWidth $
       hBox $
       maybeSelect $
       Table.alignColumns columnAlignments columnWidths ws

selectedCellAttr :: A.AttrName
selectedCellAttr = A.attrName "selectedCell"

theMapB :: A.AttrMap
theMapB = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (selectedCellAttr,      V.blue `on` V.white)
    , (E.editAttr,            V.white `on` V.blue)    
    , (E.editFocusedAttr,     V.black `on` V.yellow)
    ]

columnWidths :: [Int]
columnWidths = [20, 15, 20, 15]

totalWidth :: Int
totalWidth = sum columnWidths

mapToRows :: Map.Map String String -> Map.Map String Int -> Map.Map String (Maybe LocalTime) -> [Row]
mapToRows m m2 m3 = [Row key value (show $ fromMaybe 0 value2) (show $ fromMaybe Nothing  value3) | (key, value) <- Map.toList m , let value2 = Map.lookup key m2, let value3 = Map.lookup key m3]  -- "default" is a placeholder

headerRow :: Row
headerRow = Row "Target language:" "Translation:" "Knowledge level:" "Next revision:"

columnAlignments :: [Table.ColumnAlignment]
columnAlignments = [Table.AlignLeft, Table.AlignCenter, Table.AlignCenter,Table.AlignRight]


errorAttr :: AttrName
errorAttr = attrName "error"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listSelectedFocusedAttr, V.black `on` V.yellow)
    , (FB.fileBrowserCurrentDirectoryAttr, V.white `on` V.blue)
    , (FB.fileBrowserSelectionInfoAttr, V.white `on` V.blue)
    , (FB.fileBrowserDirectoryAttr, fg V.blue)
    , (FB.fileBrowserBlockDeviceAttr, fg V.magenta)
    , (FB.fileBrowserCharacterDeviceAttr, fg V.green)
    , (FB.fileBrowserNamedPipeAttr, fg V.yellow)
    , (FB.fileBrowserSymbolicLinkAttr, fg V.cyan)
    , (FB.fileBrowserUnixSocketAttr, fg V.red)
    , (FB.fileBrowserSelectedAttr, V.white `on` V.magenta)
    , (errorAttr, fg V.red)
    ]

theApp :: M.App (FB.FileBrowser Name) e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }



main :: IO ()
main = do
    b <- M.defaultMain theApp =<< FB.newFileBrowser FB.selectNonDirectories FileBrowser1 Nothing
    putStrLn $ "Selected entry: " <> show (FB.fileBrowserSelection b)
    let fileName = FB.fileInfoFilename (head $ FB.fileBrowserSelection b)
    let filePath = FB.fileInfoFilePath (head $ FB.fileBrowserSelection b)
    -- at this point filePath contains the path of the flashcard set we selected and fileName its name
    contentFile <- readFile filePath
    let meaningMap = parseCommaPairs contentFile
    
    let filePath2 = "flashcardsKnowledge/" ++ (intercalate "/" (reverse $ take 2 $ reverse $ splitOn "/" filePath ))
    let filePath3 = "flashcardsRevision/" ++ (intercalate "/" (reverse $ take 2 $ reverse $ splitOn "/" filePath ))
    contentFile2 <- E.try (readFile filePath2) :: IO (Either E.SomeException String)
    let knowledgeMap = case contentFile2 of
              Left ex -> createMap (Map.keys meaningMap)
              Right actualContent -> parseKnowledgePairs actualContent meaningMap

    contentFile3 <- E.try (readFile filePath3) :: IO (Either E.SomeException String)
    let revisionMap = case contentFile3 of
              Left ex -> createMapNothing (Map.keys meaningMap)
              Right actualContent -> parseDatePairs actualContent meaningMap
    
    --putStrLn $ "Selected entry: " <> show (FB.fileInfoFilename (head $ FB.fileBrowserSelection b) )
    let initialRows = mapToRows meaningMap knowledgeMap revisionMap
    let state2 = AppState filePath contentFile meaningMap knowledgeMap revisionMap (L.list List1 (Vec.fromList initialRows) 1) 0 False (E.editor Edit1 (Just 1) "") Learn1Opt 0 (Map.empty) (Map.empty) []
    b2 <- M.defaultMain appB state2 
    let firstKey = head (Map.keys (b2 ^. outboundMeanings))
    random1 <- shuffle (b2 ^. outboundOptions)
    let newRan1 = firstKey : random1 
    randomizedFirstChoice <- shuffle newRan1 
    b3 <- M.defaultMain appLearn $ St [] Nothing firstKey Presentation randomizedFirstChoice (b2 ^. outboundMeanings) (b2 ^. outboundKnowledge) [0,0,0,0] (enqueue firstKey emptyQueue) (Map.map (const 0) (b2 ^. outboundKnowledge)) (Map.map (const 0) (b2 ^. outboundKnowledge)) 0 0 (b2 ^. outboundOptions)  
    print "ciao"

