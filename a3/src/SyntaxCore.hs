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
highlightMatchingBracket :: T.Text -> Int -> Maybe (Highlight, Highlight)
highlightMatchingBracket text pos =
  let char = T.index text pos
   in case getBracketDirection char of
        Just (startingBracket, targetBracket, direction) ->
          let maybeMatchingOffset = findMatchingBracket text pos startingBracket targetBracket 1 direction
           in case maybeMatchingOffset of
                Just matchingOffset -> Just (Highlight BracketHighlight (Range (pos, (pos + 1))), Highlight BracketHighlight (Range (matchingOffset, (matchingOffset + 1))))
                Nothing -> Nothing
        Nothing -> Nothing

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

findMatchingBracket :: T.Text -> Int -> Char -> Char -> Int -> Direction -> Maybe Int
findMatchingBracket text currentIndex startingBracket targetBracket nesting direction
  | currentIndex < 0 || currentIndex >= T.length text = Nothing
  | currentChar == targetBracket && adjustedNesting == 0 = Just currentIndex
  | adjustedNesting < 0 = Nothing
  | otherwise = findMatchingBracket text nextIndex startingBracket targetBracket adjustedNesting direction
  where
    currentChar = T.index text currentIndex
    nextIndex = case direction of
      Forward -> currentIndex + 1
      Backward -> currentIndex - 1
    adjustedNesting
      | currentChar == targetBracket = nesting - 1
      | currentChar == startingBracket = nesting + 1
      | otherwise = nesting

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
