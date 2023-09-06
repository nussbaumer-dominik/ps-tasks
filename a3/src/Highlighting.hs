{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Highlighting ( highlightSyntax
                    , highlightWordOccurrences
                    , highlightMatchingBracket
                    ) where

import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import Control.Monad (forM_)
import Text.Regex.PCRE
import Syntax

highlightSyntax :: Gtk.TextBuffer -> IO ()
highlightSyntax textBuffer = do
    putStrLn "highlightSyntax"

    startIter <- #getStartIter textBuffer
    endIter <- #getEndIter textBuffer
    text <- Gtk.textBufferGetText textBuffer startIter endIter False

    let tokenPositions = getTokenPositions text syntaxRules
    putStrLn $ show tokenPositions
    tagTable <- Gtk.textBufferGetTagTable textBuffer

    forM_ tokenPositions $ \(styleName, _, (startPos, endPos)) -> do
        mTagMaybe <- getTag tagTable styleName
        case mTagMaybe of
            Just tag -> do
                sIter <- getIterAtOffset textBuffer startPos
                eIter <- getIterAtOffset textBuffer endPos
                Gtk.textBufferApplyTag textBuffer tag sIter eIter
            Nothing -> return ()
    return ()

highlightWordOccurrences :: Gtk.TextBuffer -> T.Text -> IO ()
highlightWordOccurrences textBuffer word = do
    -- Escape the word to make sure we handle special regex characters properly
    putStrLn $ "highlightWordOccurrences: " ++ T.unpack word

    let escapedWord = T.unpack $ T.concatMap escapeForRegex word
    let regexPattern = "\\b" ++ escapedWord ++ "\\b"

    startIter <- Gtk.textBufferGetStartIter textBuffer
    endIter <- Gtk.textBufferGetEndIter textBuffer
    text <- Gtk.textBufferGetText textBuffer startIter endIter False

    let matches = getAllMatches $ T.unpack text =~ regexPattern :: [(Int, Int)]
    tagTable <- Gtk.textBufferGetTagTable textBuffer
    mTagMaybe <- getTag tagTable "word-highlight"

    case mTagMaybe of
        Just tag -> do
            forM_ matches $ \(startPos, endPos) -> do
                sIter <- getIterAtOffset textBuffer startPos
                eIter <- getIterAtOffset textBuffer endPos
                Gtk.textBufferApplyTag textBuffer tag sIter eIter
        Nothing -> return ()

escapeForRegex :: Char -> T.Text
escapeForRegex c
    | c `elem` ['\\', '.', '*', '+', '?', '^', '$', '(', ')', '[', ']', '{', '}', '|'] = T.pack ['\\', c]
    | otherwise = T.singleton c

highlightMatchingBracket :: Gtk.TextBuffer -> Gtk.TextIter -> IO ()
highlightMatchingBracket textBuffer iter = do
    char <- Gtk.textIterGetChar iter
    putStrLn $ "highlightMatchingBracket: " ++ show char

    case getBracketDirection char of
        Just (targetBracket, direction) -> do
            maybeMatchingIter <- if direction == Forward
                                 then findBracketForward iter targetBracket
                                 else findBracketBackward iter targetBracket
            case maybeMatchingIter of
                Just matchingIter -> do
                    successful <- Gtk.textIterForwardChar matchingIter
                    if successful
                        then highlightIter textBuffer iter matchingIter "bracket-highlight"
                        else return ()
                Nothing -> return ()
        Nothing -> return ()


data Direction = Forward | Backward deriving (Eq)

getBracketDirection :: Char -> Maybe (Char, Direction)
getBracketDirection '[' = Just (']', Forward)
getBracketDirection ']' = Just ('[', Backward)
getBracketDirection '{' = Just ('}', Forward)
getBracketDirection '}' = Just ('{', Backward)
getBracketDirection '(' = Just (')', Forward)
getBracketDirection ')' = Just ('(', Backward)
getBracketDirection _ = Nothing

findBracketForward :: Gtk.TextIter -> Char -> IO (Maybe Gtk.TextIter)
findBracketForward iter targetBracket = do
    currentChar <- Gtk.textIterGetChar iter
    if currentChar == targetBracket
        then return $ Just iter
        else do
            moveSuccess <- Gtk.textIterForwardChar iter
            if moveSuccess
                then findBracketForward iter targetBracket
                else return Nothing

findBracketBackward :: Gtk.TextIter -> Char -> IO (Maybe Gtk.TextIter)
findBracketBackward iter targetBracket = do
    currentChar <- Gtk.textIterGetChar iter
    if currentChar == targetBracket
        then return $ Just iter
        else do
            moveSuccess <- Gtk.textIterBackwardChar iter
            if moveSuccess
                then findBracketBackward iter targetBracket
                else return Nothing

highlightIter :: Gtk.TextBuffer -> Gtk.TextIter -> Gtk.TextIter -> T.Text -> IO ()
highlightIter textBuffer start end tagName = do
    tagTable <- Gtk.textBufferGetTagTable textBuffer
    mTag <- getTag tagTable (T.unpack tagName)
    case mTag of
        Just tag -> Gtk.textBufferApplyTag textBuffer tag start end
        Nothing -> return ()

getTokenPositions :: T.Text -> [SyntaxRule] -> [(String, String, (Int, Int))]
getTokenPositions text rules =
    concatMap (\rule -> findMatches text rule) rules

findMatches :: T.Text -> SyntaxRule -> [(String, String, (Int, Int))]
findMatches t rule =
    let matches = getAllTextMatches $ (T.unpack t) =~ T.unpack (pattern rule) :: [String]
        positions = getAllMatches $ T.unpack t =~ T.unpack (pattern rule) :: [(Int, Int)]
    in zip3 (repeat $ styleTag rule) matches (map calculateEnd positions)
  where
    calculateEnd (start, len) = (start, start + len)

getTag :: Gtk.TextTagTable -> String -> IO (Maybe Gtk.TextTag)
getTag tagTable styleName = do
    let tagName = T.pack styleName
    mTag <- Gtk.textTagTableLookup tagTable tagName
    case mTag of
        Nothing -> do
            newTag <- Gtk.textTagNew (Just tagName)
            _ <- Gtk.textTagTableAdd tagTable newTag
            return $ Just newTag
        Just tag -> return $ Just tag

getIterAtOffset :: Gtk.TextBuffer -> Int -> IO Gtk.TextIter
getIterAtOffset textBuffer offset = do
    iter <- Gtk.textBufferGetIterAtOffset textBuffer (fromIntegral offset)
    return iter
