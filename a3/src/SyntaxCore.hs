{-# LANGUAGE OverloadedStrings #-}

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

data Range = Range (Int, Int)

data Highlight = Highlight StyleTag Range

-- | Represents a direction, either 'Forward' or 'Backward'.
data Direction = Forward | Backward deriving (Eq)

-- | Highlight the syntax of the provided text.
highlightSyntax :: T.Text -> [Highlight]
highlightSyntax text =
  let tokenPositions =
        getTokenPositions text syntaxRules
          ++ getUnbalancedBrackets text
   in tokenPositions

-- | Retrieve all occurrences of a word in the text.
highlightWordOccurrences :: T.Text -> T.Text -> [Highlight]
highlightWordOccurrences text word =
  let regexPattern = "\\b" ++ T.unpack word ++ "\\b"
      matches =
        map (\(start, len) -> Range (start, start + len)) $
          getAllMatches $ T.unpack text =~ regexPattern
   in [Highlight WordHighlight match' | match' <- matches]

-- | Highlight the matching bracket in the text.
highlightMatchingBracket :: T.Text -> Int -> Maybe [Highlight]
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

indexSafe :: T.Text -> Int -> Maybe Char
indexSafe text idx
  | idx < 0 || idx >= T.length text = Nothing
  | otherwise = Just (T.index text idx)

-- | Check if a character is a bracket.
isBracket :: Char -> Bool
isBracket ch = ch `elem` ("[]{}()" :: String)

-- Helper functions:
getTokenPositions :: T.Text -> [SyntaxRule] -> [Highlight]
getTokenPositions text rules = concatMap (\rule -> findMatches text rule) rules

findMatches :: T.Text -> SyntaxRule -> [Highlight]
findMatches t rule =
  let positions = getAllMatches $ T.unpack t =~ T.unpack (pattern rule)
   in [Highlight (styleTag rule) position | position <- map (\(start, len) -> Range (start, start + len)) positions]

getUnbalancedBrackets :: T.Text -> [Highlight]
getUnbalancedBrackets text =
  let positions = checkBalance (T.unpack text) 0 [] []
   in [Highlight UnmatchedBracket (Range (start, start + 1)) | Range (start, _) <- positions]

-- Recursively checks the balance of brackets in a string.
checkBalance :: String -> Int -> [(Char, Int)] -> [Range] -> [Range]
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

findMatchingBracket :: T.Text -> Int -> Char -> Char -> Direction -> Maybe Int
findMatchingBracket text currentIndex startingBracket targetBracket direction =
  let nextIndex = case direction of
        Forward -> currentIndex + 1
        Backward -> currentIndex - 1
   in findMatchingBracketHelper text nextIndex startingBracket targetBracket 1 direction

findMatchingBracketHelper :: T.Text -> Int -> Char -> Char -> Int -> Direction -> Maybe Int
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

isMatching :: Char -> Char -> Bool
isMatching '(' ')' = True
isMatching '{' '}' = True
isMatching '[' ']' = True
isMatching ')' '(' = True
isMatching '}' '{' = True
isMatching ']' '[' = True
isMatching _ _ = False

getBracketDirection :: Char -> Maybe (Char, Char, Direction)
getBracketDirection '[' = Just ('[', ']', Forward)
getBracketDirection ']' = Just (']', '[', Backward)
getBracketDirection '{' = Just ('{', '}', Forward)
getBracketDirection '}' = Just ('}', '{', Backward)
getBracketDirection '(' = Just ('(', ')', Forward)
getBracketDirection ')' = Just (')', '(', Backward)
getBracketDirection _ = Nothing
