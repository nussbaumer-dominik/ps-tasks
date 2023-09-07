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

-- | Highlights the syntax of the text in a given 'Gtk.TextBuffer'.
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

  highlightUnbalancedBrackets textBuffer
  return ()

-- | Fetches the start and end iterators of a given 'Gtk.TextBuffer'.
--
-- This function is useful for operations that need to span the entire content
-- of the buffer, such as extracting all text or applying/removing tags.
--
-- Returns a tuple containing the start and end iterators.
getBufferBounds :: Gtk.TextBuffer -> IO (Gtk.TextIter, Gtk.TextIter)
getBufferBounds buffer = do
  startIter <- Gtk.textBufferGetStartIter buffer
  endIter <- Gtk.textBufferGetEndIter buffer
  return (startIter, endIter)

-- | Removes all highlighting (tags) from the given 'Gtk.TextBuffer'.
--
-- This function fetches the bounds of the buffer and then removes all tags
-- (i.e., highlighting) within that range.
removeHighlighting :: Gtk.TextBuffer -> IO ()
removeHighlighting buffer = do
  (startIter, endIter) <- getBufferBounds buffer
  Gtk.textBufferRemoveAllTags buffer startIter endIter

-- | Highlights all occurrences of a specific word in a 'Gtk.TextBuffer'.
-- Given a word and a 'Gtk.TextBuffer', this function will search for and highlight
-- all occurrences of the word in the buffer. The search is case-sensitive and considers
-- word boundaries, ensuring that substrings are not accidentally highlighted.
--
-- The function uses a predefined tag named "word-highlight" to apply the highlighting.
--
-- Arguments:
-- * The text buffer to modify.
-- * The word to search for and highlight.
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

-- | Escapes a character for use in a regular expression.
-- Takes a Char as input and the escaped character as a 'T.Text'.
escapeForRegex :: Char -> T.Text
escapeForRegex c
  | c `elem` ['\\', '.', '*', '+', '?', '^', '$', '(', ')', '[', ']', '{', '}', '|'] = T.pack ['\\', c]
  | otherwise = T.singleton c

-- | Highlights a bracket and its matching counterpart in a 'Gtk.TextBuffer'.
--
-- Given a 'Gtk.TextIter' pointing to a bracket in a 'Gtk.TextBuffer', this function
-- will highlight both the bracket and its matching counterpart (if found). The search
-- for the matching bracket considers nested brackets of the same type.
--
-- The function uses a predefined tag named "bracket-highlight" to apply the highlighting.
--
-- Arguments:
-- * The text buffer to modify.
-- * The iterator pointing to the bracket to highlight.
highlightMatchingBracket :: Gtk.TextBuffer -> Gtk.TextIter -> IO ()
highlightMatchingBracket textBuffer iter = do
  char <- Gtk.textIterGetChar iter
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

          startingBracketEndIter <- Gtk.textIterCopy startingBracketIter
          _ <- Gtk.textIterForwardChar startingBracketEndIter

          matchingBracketEndIter <- Gtk.textIterCopy matchingIter
          _ <- Gtk.textIterForwardChar matchingBracketEndIter

          highlightIter textBuffer startingBracketIter startingBracketEndIter "bracket-highlight"
          highlightIter textBuffer matchingIter matchingBracketEndIter "bracket-highlight"
        Nothing -> return ()
    Nothing -> return ()

-- | Searches for a matching bracket in a given direction from a starting 'Gtk.TextIter'.
-- This function starts from the provided iterator and searches for the target bracket,
-- considering the specified nesting level and direction. It takes into account nested
-- brackets of the same type.
--
-- Arguments:
-- * The starting iterator.
-- * The bracket from which the search begins (e.g., '(').
-- * The target bracket to find (e.g., ')').
-- * The initial nesting level (usually starts at 1).
-- * The direction of the search, either 'Forward' or 'Backward'.
--
-- Returns 'Just' the offset of the found bracket or 'Nothing' if no matching bracket is found.
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

-- TODO: Refactor this by integrating it into highlightSyntax
highlightUnbalancedBrackets :: Gtk.TextBuffer -> IO ()
highlightUnbalancedBrackets textBuffer = do
  startIter <- #getStartIter textBuffer
  endIter <- #getEndIter textBuffer
  text <- Gtk.textBufferGetText textBuffer startIter endIter False

  let unbalancedPositions = getUnbalancedBrackets text
  putStrLn $ show unbalancedPositions
  tagTable <- Gtk.textBufferGetTagTable textBuffer
  tag <- getTag tagTable "unmatched-bracket"

  forM_ unbalancedPositions $ \(_, (start, end)) -> do
    sIter <- getIterAtOffset textBuffer start
    eIter <- getIterAtOffset textBuffer end
    Gtk.textBufferApplyTag textBuffer tag sIter eIter

-- | Identifies positions of unbalanced brackets in a given text.
-- Returns a list of unbalanced brackets and their positions.
getUnbalancedBrackets :: T.Text -> [(String, (Int, Int))]
getUnbalancedBrackets text = checkBalance (T.unpack text) 0 [] []

-- | Recursively checks the balance of brackets in a string.
-- Arguments:
-- * The string to process.
-- * The current index in the string.
-- * A stack of open brackets and their positions.
-- * A list of positions of unbalanced brackets found so far.
--
-- Returns a list of unbalanced brackets and their positions.
checkBalance :: String -> Int -> [(Char, Int)] -> [(String, (Int, Int))] -> [(String, (Int, Int))]
checkBalance [] _ [] positions = positions
checkBalance [] _ stack positions =
  positions ++ map (\(bracket, index) -> ([bracket], (index, index + 1))) stack
checkBalance (x : xs) index stack positions
  | x `elem` ("([{" :: String) = checkBalance xs (index + 1) ((x, index) : stack) positions
  | x `elem` (")]}" :: String) && not (null stack) && isMatching (fst $ head stack) x = checkBalance xs (index + 1) (tail stack) positions
  | x `elem` (")]}" :: String) && (null stack || not (isMatching (fst $ head stack) x)) = checkBalance xs (index + 1) stack (([x], (index, index + 1)) : positions)
  | otherwise = checkBalance xs (index + 1) stack positions

-- | Checks if two brackets are matching pairs.
-- Returns 'True' if the brackets match, 'False' otherwise.
isMatching :: Char -> Char -> Bool
isMatching '(' ')' = True
isMatching '{' '}' = True
isMatching '[' ']' = True
isMatching ')' '(' = True
isMatching '}' '{' = True
isMatching ']' '[' = True
isMatching _ _ = False

-- | Highlights a specific range in a 'Gtk.TextBuffer' using a given tag name.
-- Arguments:
-- * The text buffer to modify.
-- * The start iterator of the range to highlight.
-- * The end iterator of the range to highlight.
-- * The name of the tag to apply.
highlightIter :: Gtk.TextBuffer -> Gtk.TextIter -> Gtk.TextIter -> T.Text -> IO ()
highlightIter textBuffer start end tagName = do
  tagTable <- Gtk.textBufferGetTagTable textBuffer
  tag <- getTag tagTable (T.unpack tagName)
  Gtk.textBufferApplyTag textBuffer tag start end
  return ()

-- | Retrieves positions of tokens in a text based on a list of syntax rules.
-- This function scans the provided text and identifies tokens based on the provided
-- syntax rules. Each identified token is returned as a tuple containing:
-- * The name or type of the token (e.g., "keyword", "string").
-- * The matched token content.
-- * A tuple indicating the start and end positions of the token in the text.
--
-- Arguments:
-- * The text to scan for tokens.
-- * A list of syntax rules to apply.
--
-- Returns a list of identified tokens and their positions.
getTokenPositions :: T.Text -> [SyntaxRule] -> [(String, String, (Int, Int))]
getTokenPositions text rules =
  concatMap (\rule -> findMatches text rule) rules

-- | Finds matches in a text based on a given syntax rule.
-- This function scans the provided text and identifies tokens based on the provided
-- syntax rule. Each identified token is returned as a tuple containing:
-- * The style tag associated with the syntax rule (e.g., "keyword", "string").
-- * The matched token content.
-- * A tuple indicating the start and end positions of the token in the text.
--
-- Arguments:
-- * The text to scan for matches.
-- * The syntax rule to apply.
--
-- Returns a list of identified tokens and their positions based on the syntax rule.
findMatches :: T.Text -> SyntaxRule -> [(String, String, (Int, Int))]
findMatches t rule =
  let matches = getAllTextMatches $ (T.unpack t) =~ T.unpack (pattern rule) :: [String]
      positions = getAllMatches $ T.unpack t =~ T.unpack (pattern rule) :: [(Int, Int)]
   in zip3 (repeat $ styleTag rule) matches (map calculateEnd positions)
  where
    calculateEnd (start, len) = (start, start + len)

-- | Retrieves a 'Gtk.TextTag' from a 'Gtk.TextTagTable' based on a style name.
-- Arguments:
-- * The tag table to search or add to.
-- * The name of the style (used as the tag name).
--
-- Returns the found or newly created 'Gtk.TextTag'.
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

-- | Retrieves a 'Gtk.TextIter' at a specific offset in a 'Gtk.TextBuffer'.
-- Arguments:
-- * The text buffer to retrieve the iterator from.
-- * The character offset within the buffer.
--
-- Returns the 'Gtk.TextIter' at the specified offset.
getIterAtOffset :: Gtk.TextBuffer -> Int -> IO Gtk.TextIter
getIterAtOffset textBuffer offset = do
  iter <- Gtk.textBufferGetIterAtOffset textBuffer (fromIntegral offset)
  return iter

-- | Checks if a given character is a bracket.
-- Returns 'True' if the character is a bracket, 'False' otherwise.
isBracket :: Char -> Bool
isBracket ch = ch `elem` ("[]{}()" :: String)

-- | Represents a direction, either 'Forward' or 'Backward'.
data Direction = Forward | Backward deriving (Eq)

-- | Retrieves the matching bracket and a search direction for a given bracket character.
-- Takes a bracket character as input and returns a tuple containing:
getBracketDirection :: Char -> Maybe (Char, Char, Direction)
getBracketDirection '[' = Just ('[', ']', Forward)
getBracketDirection ']' = Just (']', '[', Backward)
getBracketDirection '{' = Just ('{', '}', Forward)
getBracketDirection '}' = Just ('}', '{', Backward)
getBracketDirection '(' = Just ('(', ')', Forward)
getBracketDirection ')' = Just (')', '(', Backward)
getBracketDirection _ = Nothing