module SlamDown where

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Control.Monad.State
import Control.Monad.State.Trans
import Control.Apply
import Control.Alt
import Control.Alternative
import Data.Identity
import Data.String hiding (replace)
import Data.String.Regex (regex, replace, test, parseFlags)
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

instance mdBlockOrd :: Ord MDBlock where
  compare (HorizontalRule) (HorizontalRule) = EQ
  compare (HorizontalRule) (_             ) = GT
  compare (_             ) (HorizontalRule) = LT
  compare (Header l1 u1 a) (Header l2 u2 b) = case l1 `compare` l2 of
                                                   EQ -> case u1 `compare` u2 of
                                                              EQ -> a `compare` b
                                                              other -> other
                                                   other -> other
  compare (Header _ _ _  ) (_             ) = GT
  compare (_             ) (Header _ _ _  ) = LT
  compare (Blockquote a)   (Blockquote b)   = a `compare` b
  compare (Blockquote _)   (_           )   = GT
  compare (_           )   (Blockquote _)   = LT
  compare (OrderedList i a) (OrderedList j b) = case i `compare` j of
                                                     EQ -> a `compare` b
                                                     other -> other
  compare (OrderedList _ _) (_              ) = GT
  compare (_              ) (OrderedList _ _) = LT
  compare (UnorderedList a) (UnorderedList b) = a `compare` b
  compare (UnorderedList _) (_              ) = GT
  compare (_              ) (UnorderedList _) = LT
  compare (CodeBlock i1 a)  (CodeBlock i2 b)  = case i1 `compare` i2 of
                                                     EQ -> a `compare` b
                                                     other -> other
  compare (CodeBlock _ _)   (_             )  = GT
  compare (_            )   (CodeBlock _  _)  = LT
  compare (Paragraph a)     (Paragraph b)     = a `compare` b
  compare (Paragraph _)     (_          )     = GT
  compare (_          )     (Paragraph _)     = LT

instance mdBlockEq :: Eq MDBlock where
  (==) (HorizontalRule)  (HorizontalRule)  = true
  (==) (Header l1 u1 a)  (Header l2 u2 b)  = l1 == l2 && u1 == u2 && a == b
  (==) (Blockquote a)    (Blockquote b)    = a == b
  (==) (OrderedList i a) (OrderedList j b) = i == j && a == b
  (==) (UnorderedList a) (UnorderedList b) = a == b
  (==) (CodeBlock i1 a)  (CodeBlock i2 b)  = i1 == i2 && a == b
  (==) (Paragraph a)     (Paragraph b)     = a == b

  (/=) a b = not (a == b)

instance mdBlockShow :: Show MDBlock where
  show (HorizontalRule)  = "HorizontalRule"
  show (Header l u h)    = "Header {level: " ++ show l ++ ", underline: " ++ show u ++ "} (" ++ show h ++ ")"
  show (Blockquote a)    = "Blockquote (" ++ show a ++ ")"
  show (OrderedList i a) = "OrderedList {startIndex: " ++ show i ++ "} (" ++ show a ++ ")"
  show (UnorderedList a) = "UnorderedList (" ++ show a ++ ")"
  show (CodeBlock i a)   = "CodeBlock {info: " ++ show i ++ "} (" ++ show a ++ ")"
  show (Paragraph a)     = "Paragraph (" ++ show a ++ ")"

hl2n :: HLevel -> Number
hl2n H1 = 1
hl2n H2 = 2
hl2n H3 = 3
hl2n H4 = 4
hl2n H5 = 5
hl2n H6 = 6

instance hLevelOrd :: Ord HLevel where
  compare a b = compare (hl2n a) (hl2n b)

instance hLevelEq :: Eq HLevel where
  (==) a b = (hl2n a) == (hl2n b)
  (/=) a b = (hl2n a) /= (hl2n b)

instance hLevelShow :: Show HLevel where
  show H1 = "H1"
  show H2 = "H2"
  show H3 = "H3"
  show H4 = "H4"
  show H5 = "H5"
  show H6 = "H6"

hu2n :: HUnderline -> Number
hu2n HU0 = 0
hu2n HU1 = 1
hu2n HU2 = 2

instance hUnderlineOrd :: Ord HUnderline where
  compare a b = compare (hu2n a) (hu2n b)

instance hUnderlineEq :: Eq HUnderline where
  (==) a b = (hu2n a) == (hu2n b)
  (/=) a b = (hu2n a) /= (hu2n b)

instance hUnderlineShow :: Show HUnderline where
  show HU0 = "HU0"
  show HU1 = "HU1"
  show HU2 = "HU2"

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

instance cbInfoShow :: Show CBInfo where
  show (CBInfo a) = show a


--------------------------------------------------------------------------------
-- Inline Parsers
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
    l <- link
    string ">"
    let text = if l.scheme == "mailto:" then l.path else l.scheme ++ l.path
        href = l.scheme ++ l.path
    return $ Link {text: Plain text, href: href, title: Nothing}
  where
    link = choice [uri, email, raw]
      where
        uri = try $ do
          s <- (fold <$> (notAnInlineWS `manyTill` (string ">" <|> string "://")) <+> string "://")
          p <- path
          return {scheme: s, path: p}
        email = try $ do
            p <- path
            if test emailRegex p
               then return {scheme: "mailto:", path: p}
               else fail "not an email autolink"
        raw = do
          p <- path
          return {scheme: "", path: p}
    path = fold <$> (many1 (notFollowedBy (string ">") *> notAnInlineWS))

inlinelink :: MDParser MDInline
inlinelink = do
    string "["
    text <- linkText
    string "]("
    dest <- linkDest
    title <- option (Nothing) (Just <$> try (inlineWS_ *> linkTitle))
    string ")"
    return $ Link {text: Plain text, href: dest, title: title}
  where
    linkText = fold <$> (many1 (notFollowedBy (string "]") *> notAnInlineWS))
    linkDest = fold <$> (notAnInlineWS `manyTill` lookAhead (string " " <|> string ")"))
    linkTitle = choice $
        [ "(" `surroundTitle` ")"
        , "\"" `surroundTitle` "\""
        , "'" `surroundTitle` "'"
        ]
      where
        inTitle term = fold <$> (notAnInlineWS `manyTill` lookAhead (string term <|> string ")"))
        surroundTitle open close = between (string open) (string close) (inTitle close)

--------------------------------------------------------------------------------
-- Block Parsers
--------------------------------------------------------------------------------

hrule :: MDParser MDBlock
hrule = do
  atMost 3 $ string " "
  choice $ (\s -> atLeast 3 (string s <* optional inlineWS_)) <$> ["-", "_", "*"]
  void (try newline) <|> eof
  return HorizontalRule

--------------------------------------------------------------------------------
-- Helpers/Combinators
--------------------------------------------------------------------------------

-- from the CommonMark spec http://spec.commonmark.org/0.12/#email-autolink
emailRegex = regex "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$" (parseFlags "g")

inlineWScs = [" ", "\t"]

anInlineWS :: forall m. (Monad m) => ParserT String m String
anInlineWS = oneOf inlineWScs

notAnInlineWS :: forall m. (Monad m) => ParserT String m String
notAnInlineWS = noneOf inlineWScs

inlineWS :: forall m. (Monad m) => ParserT String m String
inlineWS = fold <$> (anInlineWS `manyTill` lookAhead (void notAnInlineWS <|> eof))

inlineWS_ :: forall m. (Monad m) => ParserT String m Unit
inlineWS_ = void (anInlineWS `manyTill` lookAhead (void notAnInlineWS <|> eof))

newline :: forall m. (Monad m) => ParserT String m String
newline = string "\n" <|> (string "\r" *> ("\n" `option` string "\n")) <?> "newline"

-- NOTE: currently only returns the inlineWS, not the hardwrap newlines :/
hardwrapWS :: forall m. (Monad m) => ParserT String m String
hardwrapWS = inlineWS <* optional (newline <* notFollowedBy (inlineWS_ *> newline))

-- many1 :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m [a]
many1 = some

(<+>) :: forall s a m. (Monad m, Semigroup a) => ParserT s m a -> ParserT s m a -> ParserT s m a
(<+>) p1 p2 = do
  r1 <- p1
  r2 <- p2
  return (r1 <> r2)

atLeast :: forall s a m. (Monad m) => Number -> ParserT s m a -> ParserT s m [a]
atLeast n p = atLeastRec [] n
  where
    atLeastRec r 0 = (p >>= \r' -> atLeastRec (r ++ [r']) 0) <|> return r
    atLeastRec r n = do
      r' <- p
      atLeastRec (r ++ [r']) (n - 1)

atMost :: forall s a m. (Monad m) => Number -> ParserT s m a -> ParserT s m [a]
atMost n p = atMostRec [] n
  where
    atMostRec r 0 = return r
    atMostRec r n = (p >>= \r' -> atMostRec (r ++ [r']) (n - 1)) <|> return r
