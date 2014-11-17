module SlamDown where

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Control.Monad.State.Trans

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data MDBlock = HorizontalRule
             | Header HLevel HUnderline SDInline
             | Blockquote [MDBlock]
             | OrderedList StartIndex [MDBlock]
             | UnorderedList [MDBlock]
             | CodeBlock String
             | Paragraph [SDInline]

data HLevel = H1 | H2 | H3 | H4 | H5 | H6
data HUnderline = HU0 | HU1 | HU2
type StartIndex = Number

data SDInline = Plain String
              | Code CBInfo String
              | Emphasized SDInline
              | Strong SDInline

newtype CBInfo = CBInfo String

type MDDoc = [MDBlock] -- a markdown document is a list of MDBlocks

-- Our main StateT Parser monad

data MDPState = MDPState -- will become record of parser state

type MDParser a = StateT MDPState (Parser String) a

--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

-- TODO
