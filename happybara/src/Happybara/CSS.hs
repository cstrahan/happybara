{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Happybara.CSS
    ( Selector(..)
    , FuncArg(..)
    , Element(..)
    , Constraint(..)
    , Ident(..)
    , IdentOrString(..)
    , selectors
    ) where

import           Control.Applicative           hiding (many)
import           Control.Monad

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
             | NPlusBArg Int
             | ANArg Int
             | EvenArg
             | OddArg
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

element :: Parser Text
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
        args = (try $ anPlusB <* sp <* char ')')
           <|> (try $ sels <* sp <* char ')')

anPlusB :: Parser FuncArg
anPlusB = do
    odd <|> even <|> try negnpb <|> try anpb <|> try an <|> npb
  where
    odd    = string "odd" *> return OddArg
    even   = string "even" *> return EvenArg
    negnpb = ANPlusBArg <$> (string "-n+" *> return (-1)) <*> (posInteger)
    anpb   = ANPlusBArg <$> (integer <* char 'n') <*> (sp *> char '+' *> sp *> posInteger)
    an     = ANArg <$> (integer <* char 'n')
    npb    = NPlusBArg <$> posInteger


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
        s <- many ((singleton <$> noneOf "\\'")
               <|> try (string "\\'" *> return "'")
               <|> (string "\\"))
        char '\''
        return . T.pack . read $ "\"" <> concat s <> "\""
    double = do
        char '"'
        s <- many ((singleton <$> noneOf "\\\"")
               <|> (try $ string "\\\"")
               <|> string "\\")
        char '"'
        return . T.pack . read $ "\"" <> concat s <> "\""

integer :: Parser Int
integer = negInteger <|> posInteger

negInteger :: Parser Int
negInteger = char '-' *> (negate <$> posInteger)

posInteger :: Parser Int
posInteger = read <$> many digit

tryConsume :: Stream s m t => ParsecT s u m a -> ParsecT s u m Bool
tryConsume p =
    didParse <$> optionMaybe p
  where
    didParse (Just _) = True
    didParse Nothing  = False

sp :: Parser ()
sp = spaces
