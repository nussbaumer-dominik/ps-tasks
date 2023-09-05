{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import qualified GI.Gtk as Gtk
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Text (Text, unpack, pack)
import Data.Void (Void)
import Data.List (nub)
import Data.Char (isSpace, isPunctuation, isSymbol, isLetter)
import Data.GI.Base

type Parser = Parsec Void Text

data SyntaxRule = SyntaxRule
    { parser  :: Parser Text
    , styleTag :: String
    }

keywords :: [Text]
keywords = ["plus", "minus", "mult", "div", "cont", "each"]

identifierChar :: Parser Char
identifierChar = letterChar <|> char '_'

--keywordParser :: Parser Text
--keywordParser = skipMany (noneOfKeywordStarts) >> choice (map string keywords) <* notFollowedBy alphaNumChar

--keywordParser :: Parser Text
--keywordParser = choice (string <$> keywords) <* notFollowedBy alphaNumChar

keywordParser :: Parser Text
keywordParser = try $ do
    kw <- choice (string <$> keywords)
    notFollowedBy identifierChar
    return kw


--noneOfKeywordStarts :: Parser Char
--noneOfKeywordStarts = noneOf $ nub (map (head . unpack) keywords)

-- every word that isn't a keyword and isn't followed by a bracket
--variableParser :: Parser Text
--variableParser = try $ do
--    -- Parse a sequence of non-space characters
--    v <- takeWhile1P (Just "non-keyword") (\ch -> not (isSpace ch) && ch /= '(')
--
--    -- Check if the parsed text is not one of the keywords
--    if v `elem` keywords
--        then fail "Parsed text is a keyword"
--        else do
--            -- Ensure the matched text isn't followed by an opening bracket
--            notFollowedBy (char '(')
--            return v

--variableParser :: Parser Text
--variableParser = try $ do
--    v <- takeWhile1P (Just "non-keyword") (\ch -> not (isSpace ch) && ch /= '(' && not (isPunctuation ch) && not (isSymbol ch))
--    if v `elem` keywords
--        then fail "Parsed text is a keyword"
--        else notFollowedBy (char '(') >> return v

variableParser :: Parser Text
variableParser = try $ do
    v <- takeWhile1P (Just "identifier") isIdentifierChar
    if v `elem` keywords
        then fail "Parsed text is a keyword"
        else return v
  where
    isIdentifierChar ch = isLetter ch || ch == '_'


-- Integers
valueParser :: Parser Text
valueParser = fmap showt decimal
  where
    showt :: Integer -> Text
    showt = pack . show

syntaxRules :: [SyntaxRule]
syntaxRules = [SyntaxRule keywordParser  "keyword",
               SyntaxRule valueParser    "value",
               SyntaxRule variableParser "variable"]

initTagTable :: Gtk.TextTagTable -> IO ()
initTagTable tagTable = do
    keywordTag <- Gtk.new Gtk.TextTag [#name := "keyword", #foreground := "blue"]
    _ <- Gtk.textTagTableAdd tagTable keywordTag

    variableTag <- Gtk.new Gtk.TextTag [#name := "variable", #foreground := "pink"]
    _ <- Gtk.textTagTableAdd tagTable variableTag

    valueTag <- Gtk.new Gtk.TextTag [#name := "value", #foreground := "orange"]
    _ <- Gtk.textTagTableAdd tagTable valueTag
    return ()