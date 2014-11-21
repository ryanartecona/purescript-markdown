module SlamDown where

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Control.Monad.State
import Control.Monad.State.Trans
import Control.Monad.Identity
import Control.Apply
import Control.Alt
import Data.Either
import Data.Foldable

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data MDBlock = HorizontalRule
             | Header HLevel HUnderline MDInline
             | Blockquote [MDBlock]
             | OrderedList StartIndex [MDBlock]
             | UnorderedList [MDBlock]
             | CodeBlock String
             | Paragraph [MDInline]

data HLevel = H1 | H2 | H3 | H4 | H5 | H6
data HUnderline = HU0 | HU1 | HU2
type StartIndex = Number

data MDInline = Plain String
              | Code CBInfo String
              | Emphasized MDInline
              | Strong MDInline

newtype CBInfo = CBInfo String

type MDDoc = [MDBlock] -- a markdown document is a list of MDBlocks


-- Our main StateT Parser monad

data MDPState = MDPState -- will become record of parser state

type MDParser a = ParserT String (State MDPState) a

runMDParser :: forall a. MDParser a -> String -> Either ParseError a
runMDParser p s = MDPState # (evalState $ runParserT s p)

--------------------------------------------------------------------------------
-- Typeclass instances
--------------------------------------------------------------------------------

instance mdInlineOrd :: Ord MDInline where
  compare (Plain a) (Plain b) = a `compare` b
  compare (Plain _) (_      ) = GT
  compare (_      ) (Plain _) = LT
  compare (Code a x) (Code b y) = case x `compare` y of
                                       EQ -> a `compare` b
                                       other -> other
  compare (Code _ _) (_       ) = GT
  compare (_       ) (Code _ _) = LT
  compare (Emphasized a) (Emphasized b) = a `compare` b
  compare (Emphasized _) (_           ) = GT
  compare (_           ) (Emphasized _) = LT
  compare (Strong a)     (Strong b)     = a `compare` b
  compare (Strong _)     (_       )     = GT
  compare (_       )     (Strong _)     = LT

instance mdInlineEq :: Eq MDInline where
  (==) (Plain a) (Plain b) = a == b
  (==) (Code cba a) (Code cbb b) = cba == cbb && a == b
  (==) (Emphasized a) (Emphasized b) = a == b
  (==) (Strong a) (Strong b) = a == b
  (/=) a b = not (a == b)

instance mdInlineShow :: Show MDInline where
  show (Plain a) = "Plain (" ++ show a ++ ")"
  show (Code (CBInfo cb) a) = "Code {" ++ cb ++ "} (" ++ show a ++ ")"
  show (Emphasized a) = "Emphasized (" ++ show a ++ ")"
  show (Strong a) = "Strong (" ++ show a ++ ")"


instance cbInfoOrd :: Ord CBInfo where
  compare (CBInfo a) (CBInfo b) = a `compare` b

instance cbInfoEq :: Eq CBInfo where
  (==) (CBInfo a) (CBInfo b) = a == b
  (/=) (CBInfo a) (CBInfo b) = a /= b


--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

inlineWS = [" ", "\t"]

emph :: MDParser MDInline
emph = do
    delimC <- begin
    str <- fold <$> (char `manyTill` lookAhead (end delimC))
    lastC <- end delimC
    return $ Emphasized $ Plain $ str ++ lastC
  where
    begin = oneOf ["*", "_"] <* lookAhead (noneOf inlineWS)
    end c = noneOf inlineWS <* string c

strong :: MDParser MDInline
strong = do
    delimCC <- begin
    str <- fold <$> (char `manyTill` lookAhead (end delimCC))
    lastC <- end delimCC
    return $ Strong $ Plain $ str ++ lastC
  where
    begin = (string "**" <|> string "__") <* lookAhead (noneOf inlineWS)
    end cc = noneOf inlineWS <* string cc
