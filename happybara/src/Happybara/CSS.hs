{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- |
-- Copyright :  (c) Charles Strahan 2014
-- License   :  MIT
-- Maintainer:  Charles Strahan <charles.c.strahan@gmail.com>
-- Stability :  experimental
module Happybara.CSS
    ( Selector(..)
    , FuncArg(..)
    , Element
    , Constraint(..)
    , Ident
    , IdentOrString(..)
    , selectors
    ) where

import           Prelude                       hiding (even, odd)

import           Control.Applicative           hiding (many, optional)

import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T

import           Text.Parsec.Prim              (ParsecT, Stream)
import           Text.ParserCombinators.Parsec hiding ((<|>))

data Selector = SimpleSelector (Maybe Element) [Constraint]
              | Descendent Selector Selector
              | ImmediateChild Selector Selector
              | PreviousSibling Selector Selector
              | AdjacentSibling Selector Selector
              deriving (Eq, Show)

data FuncArg = ANPlusBArg Int Int
             | SelectorArg Selector
             | NoArg
             deriving (Eq, Show)

type Element = Text

data Constraint = Class Text
                | ID Text
                | HasAttribute Ident
                | AttributeEquals Ident IdentOrString
                | AttributeContains Ident IdentOrString
                | AttributeDoesNotContain Ident IdentOrString
                | AttributeContainsWord Ident IdentOrString
                | AttributeContainsPrefix Ident IdentOrString
                | AttributeStartsWith Ident IdentOrString
                | AttributeEndsWith Ident IdentOrString
                | PseudoClass Ident
                | PseudoFunc Ident FuncArg
                deriving (Eq, Show)

type Ident = Text

data IdentOrString = Ident Text
                   | StringLit Text
                   deriving (Eq, Show)

selectors :: Parser [Selector]
selectors = selector `sepBy` (sp *> char ',' *> sp)

selector :: Parser Selector
selector =
    chainl1 simpleSelector parseOp
  where
    parseOp = do
        mop <- (sp *> (optionMaybe $ oneOf "+~>") <* sp)
        return $ case mop of
                     Just '+' -> AdjacentSibling
                     Just '~' -> PreviousSibling
                     Just '>' -> ImmediateChild
                     _        -> Descendent

simpleSelector :: Parser Selector
simpleSelector = SimpleSelector <$> (Just <$> element) <*> many constraint
             <|> SimpleSelector <$> (return Nothing) <*> many1 constraint

constraint :: Parser Constraint
constraint =
    klass <|> hash <|> attrib <|> pseudo
  where
    klass =
        char '.' *> (Class <$> ident)
    hash =
        char '#' *> (ID <$> ident)
    attrib = char '[' *> sp *> do
        attr <- (ident <* sp)
        end <- tryConsume $ char ']'
        if end
          then return $ HasAttribute attr
          else do
              op <- choice $ map string [ "=", "*=", "!=", "~=", "|=", "^=", "$=" ]
              val <- sp *> identOrString <* sp <* char ']'
              return $ mkAttr op attr val
      where
        identOrString = (Ident <$> ident) <|> (StringLit <$> str)
        mkAttr "="  = AttributeEquals
        mkAttr "*=" = AttributeContains
        mkAttr "!=" = AttributeDoesNotContain
        mkAttr "~=" = AttributeContainsWord
        mkAttr "|=" = AttributeContainsPrefix
        mkAttr "^=" = AttributeStartsWith
        mkAttr "$=" = AttributeEndsWith

element :: Parser Element
element =
    ident <|> (string "*" *> return "*")

pseudo :: Parser Constraint
pseudo = do
    i <- char ':' *> ident
    pseudoFunc i <|> pseudoClass i
  where
    pseudoClass i = PseudoClass <$> (return i)
    pseudoFunc i = do
        char '(' *> sp
        isEmpty <- tryConsume $ char ')'
        if isEmpty
          then return $ PseudoFunc i NoArg
          else do
              a <- args
              return $ PseudoFunc i a
      where
        sels = SelectorArg <$> selector
        args = (try $ nth <* sp <* char ')')
           <|> (try $ sels <* sp <* char ')')

-- http://dev.w3.org/csswg/selectors3/#nth-child-pseudo
nth :: Parser FuncArg
nth = even <|> odd <|> try an_anpb <|> npb <?> "nth"
  where
    even   = string "even" *> (return $ ANPlusBArg 2 0)
    odd    = string "odd" *> (return $ ANPlusBArg 2 1)
    an_anpb = do
        asign <- option '+' (oneOf "+-")
        anum <- option 1 integer <* char 'n' <* sp
        let a | asign == '-' = (-anum)
              | otherwise    =   anum
        b <- option 0 $ do
            sign <- (oneOf "+-") <* sp
            num <- integer
            return $ if sign == '-' then (-num) else num
        return $ ANPlusBArg a b
    npb = do
        bsign <- option '+' (oneOf "+-")
        bnum <- integer
        let b | bsign == '-' = (-bnum)
              | otherwise    =   bnum
        return $ ANPlusBArg 0 b
    integer = read <$> many1 digit

ident :: Parser Text
ident = do
    first <- letter <|> char '_'
    rest <- many $ alphaNum <|> char '_' <|> char '-'
    return $ T.pack (first:rest)

-- TODO: Implement propper string parsing/escaping, instead of piggybacking.
str :: Parser Text
str =
    single <|> double
  where
    singleton = (:[])
    single = do
        char '\''
        s <- many ((noneOf "\\'")
               <|> try (string "\\'" *> return '\'')
               <|> (char '\\'))
        char '\''
        return . T.pack . read $ "\"" <> s <> "\""
    double = do
        char '"'
        s <- many ((singleton <$> noneOf "\\\"")
               <|> (try $ string "\\\"")
               <|> string "\\")
        char '"'
        return . T.pack . read $ "\"" <> concat s <> "\""

tryConsume :: Stream s m t => ParsecT s u m a -> ParsecT s u m Bool
tryConsume p =
    didParse <$> optionMaybe p
  where
    didParse (Just _) = True
    didParse Nothing  = False

sp :: Parser ()
sp = spaces
