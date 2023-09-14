{-# LANGUAGE OverloadedStrings #-}

-- | The 'SyntaxCore' module provides core functionalities for highlighting
-- different aspects of a text with a particular focus on syntax.
module SyntaxCore
  ( highlightSyntax,
    Highlight (..),
    Range (..),
    highlightWordOccurrences,
    highlightMatchingBracket,
    isBracket,
  )
where

import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Syntax (StyleTag (..), SyntaxRule (..), syntaxRules)
import Text.Regex.PCRE

-- | 'Range' represents a start and end position in a text.
data Range = Range (Int, Int)

-- | 'Highlight' represents a styled tag applied over a range.
data Highlight = Highlight StyleTag Range

-- | Represents a direction, either 'Forward' or 'Backward'.
data Direction = Forward | Backward deriving (Eq)

-- | Highlight the syntax of the provided text.
highlightSyntax
  :: T.Text       -- ^ Text to be searched through
  -> [Highlight]  -- ^ Returns a list containing all syntax highlights
highlightSyntax text = getTokenPositions text syntaxRules ++ getUnbalancedBrackets text

-- | Retrieve all occurrences of a word in the text.
highlightWordOccurrences
  :: T.Text       -- ^ Text to be searched through
  -> T.Text       -- ^ The word to be searched
  -> [Highlight]  -- ^ Returns a list containing all syntax highlights
highlightWordOccurrences text word =
  let regexPattern = "\\b" ++ T.unpack word ++ "\\b"
      matches =
        map (\(start, len) -> Range (start, start + len)) $
          getAllMatches $ T.unpack text =~ regexPattern
   in [Highlight WordHighlight match' | match' <- matches]

-- | Highlight the matching bracket in the text.
highlightMatchingBracket
  :: T.Text             -- ^ Text to be searched through
  -> Int                -- ^ offset position in the text
  -> Maybe [Highlight]  -- ^ maybe returns a list of highlights
highlightMatchingBracket text pos =
  let charAtCursor = indexSafe text pos
      charBeforeCursor = indexSafe text (pos - 1)
      (actualChar, actualPos) =
        if isJust charAtCursor && isBracket (fromJust charAtCursor)
          then (charAtCursor, pos)
          else (charBeforeCursor, pos - 1)
   in case actualChar >>= getBracketDirection of
        Just (startingBracket, targetBracket, direction) ->
          let maybeMatchingOffset = findMatchingBracket text actualPos startingBracket targetBracket direction
           in case maybeMatchingOffset of
                Just matchingOffset ->
                  Just
                    [ Highlight BracketHighlight (Range (actualPos, actualPos + 1)),
                      Highlight BracketHighlight (Range (matchingOffset, matchingOffset + 1))
                    ]
                Nothing -> Nothing
        Nothing -> Nothing

-- | Safely get the character at a given index of a text.
indexSafe
  :: T.Text     -- ^ Text to be searched
  -> Int        -- ^ index of the character in the text
  -> Maybe Char -- ^ Maybe returns a character
indexSafe text idx
  | idx < 0 || idx >= T.length text = Nothing
  | otherwise = Just (T.index text idx)

-- | Check if a character is one of the recognized brackets: [], {}, ().
isBracket
  :: Char -- ^ Character to be checked
  -> Bool -- ^ Returns true if the given character is a valid bracket, false otherwise
isBracket ch = ch `elem` ("[]{}()" :: String)

-- Helper functions:
-- | Retrieves all Highlights for a list of SyntaxRules in a given text
getTokenPositions
  :: T.Text       -- ^ Text to be searched through
  -> [SyntaxRule] -- ^ List of SyntaxRules to be checked
  -> [Highlight]  -- ^ Returns a list of Highlights
getTokenPositions text rules = concatMap (\rule -> findMatches text rule) rules

-- | Retrieves all Highlights for a given SyntaxRule
findMatches
  :: T.Text       -- ^ Text to be searched through
  -> SyntaxRule   -- ^ SyntaxRule to be checked
  -> [Highlight]  -- ^ Returns a list of Highlights
findMatches t rule =
  let positions = getAllMatches $ T.unpack t =~ T.unpack (pattern rule)
   in [Highlight (styleTag rule) position | position <- map (\(start, len) -> Range (start, start + len)) positions]

getUnbalancedBrackets :: T.Text -> [Highlight]
getUnbalancedBrackets text =
  let positions = checkBalance (T.unpack text) 0 [] []
   in [Highlight UnmatchedBracket (Range (start, start + 1)) | Range (start, _) <- positions]

-- | Recursively checks the balance of brackets in a string and returns positions of the unbalanced brackets.
--
-- The function processes the string one character at a time, keeping track of
-- the seen opening brackets and their positions. It compares each closing bracket
-- with the last seen opening bracket to check for a match. Unmatched opening
-- brackets left at the end are also considered unbalanced.
checkBalance
  :: String           -- ^ The string to be searched through for unbalanced brackets.
  -> Int              -- ^ Current index (position) being examined in the string.
  -> [(Char, Int)]    -- ^ Stack to keep track of seen opening brackets and their positions.
  -> [Range]          -- ^ Accumulator for positions of the found unbalanced brackets.
  -> [Range]          -- ^ Returns all positions of unbalanced brackets as ranges.
checkBalance [] _ [] positions = positions
checkBalance [] _ stack positions =
  positions ++ [Range (index, index + 1) | (_, index) <- stack]
checkBalance (x : xs) index stack positions
  | x `elem` ("([{" :: String) = checkBalance xs (index + 1) ((x, index) : stack) positions
  | x `elem` (")]}" :: String) && not (null stack) && isMatching (fst $ head stack) x =
    checkBalance xs (index + 1) (tail stack) positions
  | x `elem` (")]}" :: String) && (null stack || not (isMatching (fst $ head stack) x)) =
    checkBalance xs (index + 1) stack (Range (index, index + 1) : positions)
  | otherwise = checkBalance xs (index + 1) stack positions

-- | Attempts to find the matching bracket in the text, given a starting position and direction.
--
-- This function searches for the matching bracket to the one provided as input,
-- moving in the specified direction from the current index. For instance, if
-- given an opening bracket, the function will search forward for the corresponding
-- closing bracket. Conversely, if provided with a closing bracket, the function
-- will search backward for its opening counterpart.
findMatchingBracket
  :: T.Text            -- ^ The text in which to search for the matching bracket.
  -> Int               -- ^ The starting index from which to begin the search.
  -> Char              -- ^ The bracket for which a match is being sought.
  -> Char              -- ^ The expected matching bracket.
  -> Direction         -- ^ The direction in which to search ('Forward' or 'Backward').
  -> Maybe Int         -- ^ Returns the index of the matching bracket, if found.
findMatchingBracket text currentIndex startingBracket targetBracket direction =
  let nextIndex = case direction of
        Forward -> currentIndex + 1
        Backward -> currentIndex - 1
   in findMatchingBracketHelper text nextIndex startingBracket targetBracket 1 direction

-- | Helper function for 'findMatchingBracket'.
findMatchingBracketHelper
  :: T.Text     -- ^ The text in which to search for the matching bracket.
  -> Int        -- ^ The current index in the text.
  -> Char       -- ^ The bracket for which a match is being sought.
  -> Char       -- ^ The expected matching bracket.
  -> Int        -- ^ The current nesting level.
  -> Direction  -- ^ The direction in which to search ('Forward' or 'Backward').
  -> Maybe Int  -- ^ Returns the index of the matching bracket, if found.
findMatchingBracketHelper text currentIndex startingBracket targetBracket nesting direction
  | currentIndex < 0 || currentIndex >= T.length text = Nothing
  | currentChar == targetBracket && nesting == 1 = Just currentIndex
  | currentChar == targetBracket = continueSearch (nesting - 1)
  | currentChar == startingBracket = continueSearch (nesting + 1)
  | otherwise = continueSearch nesting
  where
    currentChar = T.index text currentIndex
    nextIndex = case direction of
      Forward -> currentIndex + 1
      Backward -> currentIndex - 1
    continueSearch adjustedNesting = findMatchingBracketHelper text nextIndex startingBracket targetBracket adjustedNesting direction

-- | Checks if two characters are matching brackets.
isMatching
  :: Char -- ^ First character to be checked
  -> Char -- ^ Second character to be checked
  -> Bool -- ^ Returns true if the two characters are matching brackets, false otherwise
isMatching '(' ')' = True
isMatching '{' '}' = True
isMatching '[' ']' = True
isMatching ')' '(' = True
isMatching '}' '{' = True
isMatching ']' '[' = True
isMatching _ _ = False

-- | Retrieves the matching bracket and the direction in which to search for it.
getBracketDirection
  :: Char -- ^ The bracket for which a match is being sought.
  -> Maybe (Char, Char, Direction) -- ^ Maybe returns a tuple containing the matching bracket, its counterpart, and the direction in which to search.
getBracketDirection '[' = Just ('[', ']', Forward)
getBracketDirection ']' = Just (']', '[', Backward)
getBracketDirection '{' = Just ('{', '}', Forward)
getBracketDirection '}' = Just ('}', '{', Backward)
getBracketDirection '(' = Just ('(', ')', Forward)
getBracketDirection ')' = Just (')', '(', Backward)
getBracketDirection _ = Nothing
