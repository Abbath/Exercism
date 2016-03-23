{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Sgf where 

import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Data.Tree
import Data.String.Utils

type SgfTree = Tree SgfNode
type SgfNode = Map Text [Text]

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{" "}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol t =  do
    let s = T.unpack t
    ss <- L.symbol sc s
    return $ T.pack ss

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

semicolon :: Parser Text
semicolon = symbol ";"

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

key :: Parser Text
key = do
    x <- some upperChar
    lexeme (return (T.pack x))

value :: Parser Text
value = do
     x <- some (try shyza <|> try alphaNumChar <|> try tab <|> try spaceChar <|> try newline <|> char '\\') 
     lexeme (return $ T.pack . fix $ x)
  
fix :: String -> String
fix =  replace "\\]" "\\ ]" . replace "\\ " "" . replace "\n" " " . replace "\t" " "
     
shyza :: Parser Char
shyza = do
    _ <- char '\\'
    c2 <- char ']'
    return c2
--     
parser :: Parser SgfTree 
parser = sc *> tree <* eof

tree :: Parser SgfTree
tree = do 
    n <- parens nodes
    return $ n

children :: Parser [SgfTree]
children = try (count 1 nodes) <|> try (many tree)    
    
nodes :: Parser SgfTree
nodes = do
    void semicolon
    x <- optional (many keyval)
    c <- optional children
    return $ case x of 
         Nothing -> Node (M.fromList []) []
         Just r -> case c of 
                        Nothing -> Node (M.fromList r) []
                        Just ch -> Node (M.fromList r) ch

keyval :: Parser (Text,[Text])
keyval = do 
    k <- key
    v <- many (brackets value)
    return $ (k,v)
    
parseSgf :: Text -> Maybe SgfTree
parseSgf t = case parse parser "" t of 
                  Left _ -> Nothing
                  Right r -> Just r