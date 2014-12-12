# Module Documentation

## Module SlamDown

### Types

    newtype CBInfo where
      CBInfo :: String -> CBInfo

    data HLevel where
      H1 :: HLevel
      H2 :: HLevel
      H3 :: HLevel
      H4 :: HLevel
      H5 :: HLevel
      H6 :: HLevel

    data HUnderline where
      HU0 :: HUnderline
      HU1 :: HUnderline
      HU2 :: HUnderline

    data MDBlock where
      HorizontalRule :: MDBlock
      Header :: HLevel -> HUnderline -> MDInline -> MDBlock
      Blockquote :: [MDBlock] -> MDBlock
      OrderedList :: StartIndex -> [MDBlock] -> MDBlock
      UnorderedList :: [MDBlock] -> MDBlock
      CodeBlock :: CBInfo -> String -> MDBlock
      Paragraph :: [MDInline] -> MDBlock

    type MDDoc = [MDBlock]

    data MDInline where
      Plain :: String -> MDInline
      Code :: CBInfo -> String -> MDInline
      Emphasized :: MDInline -> MDInline
      Strong :: MDInline -> MDInline
      Link :: { text :: MDInline, href :: String, title :: Maybe String } -> MDInline

    data MDPState where
      MDPState :: MDPState

    type MDParser a = ParserT String (State MDPState) a

    type StartIndex = Number


### Type Class Instances

    instance cbInfoEq :: Eq CBInfo

    instance cbInfoOrd :: Ord CBInfo

    instance mdInlineEq :: Eq MDInline

    instance mdInlineOrd :: Ord MDInline

    instance mdInlineShow :: Show MDInline


### Values

    (<+>) :: forall s a m. (Monad m, Semigroup a) => ParserT s m a -> ParserT s m a -> ParserT s m a

    anInlineWS :: forall m. (Monad m) => ParserT String m String

    autolink :: MDParser MDInline

    code :: MDParser MDInline

    emph :: MDParser MDInline

    hardwrapWS :: forall m. (Monad m) => ParserT String m String

    inlineWS :: forall m. (Monad m) => ParserT String m String

    inlineWS_ :: forall m. (Monad m) => ParserT String m Unit

    newline :: forall m. (Monad m) => ParserT String m String

    notAnInlineWS :: forall m. (Monad m) => ParserT String m String

    runMDParser :: forall a. MDParser a -> String -> Either ParseError a

    strong :: MDParser MDInline



