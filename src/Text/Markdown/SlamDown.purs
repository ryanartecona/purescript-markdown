module SlamDown where

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Control.Monad.State
import Control.Monad.State.Trans
import Control.Monad.Identity
import Control.Apply
import Control.Alt
import Control.Alternative
import Data.String hiding (replace)
import Data.String.Regex (regex, replace, parseFlags)
import Data.Maybe
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
             | CodeBlock CBInfo String
             | Paragraph [MDInline]

data HLevel = H1 | H2 | H3 | H4 | H5 | H6
data HUnderline = HU0 | HU1 | HU2
type StartIndex = Number

data MDInline = Plain String
              | Code CBInfo String
              | Emphasized MDInline
              | Strong MDInline
              | Link {title :: Maybe String, href :: String, text :: MDInline}

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
  compare (Link a)       (Link b)       =
    case a.href `compare` b.href of
         EQ -> case a.title `compare` b.title of
                    EQ -> a.text `compare` b.text
                    other -> other
         other -> other
  compare (Link a)       (_     )       = GT
  compare (_     )       (Link b)       = LT


instance mdInlineEq :: Eq MDInline where
  (==) (Plain a)      (Plain b)      = a == b
  (==) (Code cba a)   (Code cbb b)   = cba == cbb && a == b
  (==) (Emphasized a) (Emphasized b) = a == b
  (==) (Strong a)     (Strong b)     = a == b
  (==) (Link a)       (Link b)       = a.href == b.href && a.title == b.title && a.text == b.text
  (==) _              _              = false

  (/=) a b = not (a == b)


instance mdInlineShow :: Show MDInline where
  show (Plain a) = "Plain (" ++ show a ++ ")"
  show (Code (CBInfo cb) a) = "Code {" ++ cb ++ "} (" ++ show a ++ ")"
  show (Emphasized a) = "Emphasized (" ++ show a ++ ")"
  show (Strong a) = "Strong (" ++ show a ++ ")"
  show (Link a) = "Link {href: " ++ show a.href ++ ", title: " ++ show a.title ++ "} (" ++ show a.text ++ ")"


instance cbInfoOrd :: Ord CBInfo where
  compare (CBInfo a) (CBInfo b) = a `compare` b

instance cbInfoEq :: Eq CBInfo where
  (==) (CBInfo a) (CBInfo b) = a == b
  (/=) (CBInfo a) (CBInfo b) = a /= b


--------------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------------

emph :: MDParser MDInline
emph = do
    delimC <- begin
    str <- fold <$> (char `manyTill` lookAhead (end delimC))
    lastC <- end delimC
    return $ Emphasized $ Plain $ str ++ lastC
  where
    begin = oneOf ["*", "_"] <* lookAhead notAnInlineWS
    end c = notAnInlineWS <* string c

strong :: MDParser MDInline
strong = do
    delimCC <- begin
    str <- fold <$> (char `manyTill` lookAhead (end delimCC))
    lastC <- end delimCC
    return $ Strong $ Plain $ str ++ lastC
  where
    begin = (string "**" <|> string "__") <* lookAhead notAnInlineWS
    end cc = notAnInlineWS <* string cc

code :: MDParser MDInline
code = do
    bts <- backtickS
    str <- innerCode bts
    string bts
    return $ Code (CBInfo "") (replace collapseWSRegex (trim str) " ")
  where
    backtickS = fold <$> many1 (string "`")
    innerCodeNonNewline bts = fold <$> many1 ((notFollowedBy (string bts <|> newline )) *> char)
    innerCode bts = joinWith " " <$> (innerCodeNonNewline bts `sepBy` try hardwrapWS)
    collapseWSRegex = "\\W+" `regex` parseFlags "g"

autolink :: MDParser MDInline
autolink = do
    string "<"
    s <- scheme
    p <- path
    string ">"
    let href = (maybe "" (\s -> s ++ "://") s) ++ p
    return $ Link {text: Plain href, href: href, title: Nothing}
  where
    justScheme = (Just <<< fold) <$> (many1 (notFollowedBy (string ">") *> notAnInlineWS) <* string "://")
    noScheme = return Nothing
    scheme = try justScheme <|> noScheme
    path = fold <$> (many1 (notFollowedBy (string ">") *> notAnInlineWS))

--------------------------------------------------------------------------------
-- Helpers/Combinators
--------------------------------------------------------------------------------

inlineWScs = [" ", "\t"]

anInlineWS :: forall m. (Monad m) => ParserT String m String
anInlineWS = oneOf inlineWScs

notAnInlineWS :: forall m. (Monad m) => ParserT String m String
notAnInlineWS = noneOf inlineWScs

inlineWS :: forall m. (Monad m) => ParserT String m String
inlineWS = fold <$> (anInlineWS `manyTill` lookAhead notAnInlineWS)

inlineWS_ :: forall m. (Monad m) => ParserT String m Unit
inlineWS_ = void (anInlineWS `manyTill` lookAhead notAnInlineWS)

newline :: forall m. (Monad m) => ParserT String m String
newline = string "\n" <|> (string "\r" *> ("\n" `option` string "\n")) <?> "newline"

-- NOTE: currently only returns the inlineWS, not the hardwrap newlines :/
hardwrapWS :: forall m. (Monad m) => ParserT String m String
hardwrapWS = inlineWS <* optional (newline <* notFollowedBy (inlineWS_ *> newline))

-- NOTE: will be removable in next version of purescript-parsing
notFollowedBy :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit
notFollowedBy p = try $ (try p >> fail "Negated parser succeeded") <|> return unit
  where (>>) a b = a >>= const b

-- many1 :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m [a]
many1 = some
