{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Highlighting
  ( highlightSyntax,
    highlightWordOccurrences,
    highlightMatchingBracket,
    isBracket,
  )
where

import Control.Monad (forM_, unless)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import Syntax
import Text.Regex.PCRE

highlightSyntax :: Gtk.TextBuffer -> IO ()
highlightSyntax textBuffer = do
  putStrLn "highlightSyntax"
  removeHighlighting textBuffer

  startIter <- #getStartIter textBuffer
  endIter <- #getEndIter textBuffer
  text <- Gtk.textBufferGetText textBuffer startIter endIter False

  let tokenPositions = getTokenPositions text syntaxRules
  putStrLn $ show tokenPositions
  tagTable <- Gtk.textBufferGetTagTable textBuffer

  forM_ tokenPositions $ \(styleName, _, (startPos, endPos)) -> do
    tag <- getTag tagTable styleName
    sIter <- getIterAtOffset textBuffer startPos
    eIter <- getIterAtOffset textBuffer endPos
    Gtk.textBufferApplyTag textBuffer tag sIter eIter

  return ()

getBufferBounds :: Gtk.TextBuffer -> IO (Gtk.TextIter, Gtk.TextIter)
getBufferBounds buffer = do
  startIter <- Gtk.textBufferGetStartIter buffer
  endIter <- Gtk.textBufferGetEndIter buffer
  return (startIter, endIter)

removeHighlighting :: Gtk.TextBuffer -> IO ()
removeHighlighting buffer = do
  (startIter, endIter) <- getBufferBounds buffer
  Gtk.textBufferRemoveAllTags buffer startIter endIter

highlightWordOccurrences :: Gtk.TextBuffer -> T.Text -> IO ()
highlightWordOccurrences textBuffer word = do
  putStrLn $ "highlightWordOccurrences: " ++ T.unpack word

  unless (T.null word) $ do
    let escapedWord = T.unpack $ T.concatMap escapeForRegex word
    let regexPattern =
          if T.length word > 1
            then "\\b" ++ escapedWord ++ "\\b"
            else escapedWord

    startIter <- Gtk.textBufferGetStartIter textBuffer
    endIter <- Gtk.textBufferGetEndIter textBuffer
    text <- Gtk.textBufferGetText textBuffer startIter endIter False

    let matches = map (\(start, len) -> (start, start + len)) $ getAllMatches $ T.unpack text =~ regexPattern :: [(Int, Int)]
    putStrLn $ show matches
    tagTable <- Gtk.textBufferGetTagTable textBuffer
    tag <- getTag tagTable "word-highlight"

    forM_ matches $ \(startPos, endPos) -> do
      sIter <- getIterAtOffset textBuffer startPos
      eIter <- getIterAtOffset textBuffer endPos
      Gtk.textBufferApplyTag textBuffer tag sIter eIter

escapeForRegex :: Char -> T.Text
escapeForRegex c
  | c `elem` ['\\', '.', '*', '+', '?', '^', '$', '(', ')', '[', ']', '{', '}', '|'] = T.pack ['\\', c]
  | otherwise = T.singleton c

highlightMatchingBracket :: Gtk.TextBuffer -> Gtk.TextIter -> IO ()
highlightMatchingBracket textBuffer iter = do
  char <- Gtk.textIterGetChar iter

  -- Save the offset of the original bracket
  startingOffset <- Gtk.textIterGetOffset iter

  case getBracketDirection char of
    Just (startingBracket, targetBracket, direction) -> do
      maybeMatchingOffset <-
        if direction == Forward
          then findBracket iter startingBracket targetBracket 1 Forward
          else findBracket iter startingBracket targetBracket 1 Backward
      case maybeMatchingOffset of
        Just matchingOffset -> do
          matchingIter <- getIterAtOffset textBuffer matchingOffset
          startingBracketIter <- getIterAtOffset textBuffer (fromIntegral startingOffset)

          -- Here, we're creating copies of the iters, and then moving them one position forward
          startingBracketEndIter <- Gtk.textIterCopy startingBracketIter
          _ <- Gtk.textIterForwardChar startingBracketEndIter

          matchingBracketEndIter <- Gtk.textIterCopy matchingIter
          _ <- Gtk.textIterForwardChar matchingBracketEndIter

          highlightIter textBuffer startingBracketIter startingBracketEndIter "bracket-highlight"
          highlightIter textBuffer matchingIter matchingBracketEndIter "bracket-highlight"
        Nothing -> return ()
    Nothing -> return ()

findBracket :: Gtk.TextIter -> Char -> Char -> Int -> Direction -> IO (Maybe Int)
findBracket iter startingBracket targetBracket nesting direction = do
  moveSuccess <- case direction of
    Forward -> Gtk.textIterForwardChar iter
    Backward -> Gtk.textIterBackwardChar iter
  if moveSuccess
    then do
      currentChar <- Gtk.textIterGetChar iter
      let adjustedNesting = adjustNesting currentChar
      if currentChar == targetBracket && adjustedNesting == 0
        then (Just . fromIntegral) <$> Gtk.textIterGetOffset iter
        else
          if adjustedNesting < 0
            then return Nothing
            else findBracket iter startingBracket targetBracket adjustedNesting direction
    else return Nothing
  where
    adjustNesting char
      | char == targetBracket = nesting - 1
      | char == startingBracket = nesting + 1
      | otherwise = nesting

-- helper
highlightIter :: Gtk.TextBuffer -> Gtk.TextIter -> Gtk.TextIter -> T.Text -> IO ()
highlightIter textBuffer start end tagName = do
  tagTable <- Gtk.textBufferGetTagTable textBuffer
  tag <- getTag tagTable (T.unpack tagName)
  Gtk.textBufferApplyTag textBuffer tag start end
  return ()

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

getTag :: Gtk.TextTagTable -> String -> IO (Gtk.TextTag)
getTag tagTable styleName = do
  let tagName = T.pack styleName
  mTag <- Gtk.textTagTableLookup tagTable tagName
  case mTag of
    Nothing -> do
      newTag <- Gtk.textTagNew (Just tagName)
      _ <- Gtk.textTagTableAdd tagTable newTag
      return $ newTag
    Just tag -> return tag

getIterAtOffset :: Gtk.TextBuffer -> Int -> IO Gtk.TextIter
getIterAtOffset textBuffer offset = do
  iter <- Gtk.textBufferGetIterAtOffset textBuffer (fromIntegral offset)
  return iter

isBracket :: Char -> Bool
isBracket ch = ch `elem` ("[]{}()" :: String)

data Direction = Forward | Backward deriving (Eq)

getBracketDirection :: Char -> Maybe (Char, Char, Direction)
getBracketDirection '[' = Just ('[', ']', Forward)
getBracketDirection ']' = Just (']', '[', Backward)
getBracketDirection '{' = Just ('{', '}', Forward)
getBracketDirection '}' = Just ('}', '{', Backward)
getBracketDirection '(' = Just ('(', ')', Forward)
getBracketDirection ')' = Just (')', '(', Backward)
getBracketDirection _ = Nothing
