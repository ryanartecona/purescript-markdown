module SlamDown where

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Control.Monad.State.Trans
import Control.Monad.Identity
import Control.Apply
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

type MDParser a = ParserT String (StateT MDPState Identity) a


--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

emph :: MDParser MDInline
emph = do
    delimC <- begin
    str <- fold <$> (char `manyTill` end delimC)
    lastC <- end delimC
    return $ Emphasized $ Plain $ str ++ lastC
  where
    inlineWS = [" ", "\t"]
    begin = oneOf ["*","_"] <* lookAhead (noneOf inlineWS)
    end c = noneOf inlineWS *> string c
