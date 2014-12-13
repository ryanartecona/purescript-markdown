module Test.Main where

import SlamDown
import Test.StrongCheck
import Data.Either
import Data.Maybe
{-- import Text.Parsing.Parser.String --}

parses :: forall a. (Eq a, Show a) => MDParser a -> String -> a -> Result
parses p s t = case runMDParser p s of
                    Left e -> false
                      <?> "Failed to parse: " ++ show s ++ "\n  into: " ++ show t ++ "\n error: " ++ show e ++ "\n\n"
                    Right a -> a == t
                      <?> "Expected: " ++ show t ++ "\n\n  Actual: " ++ show a ++ "\n\n"

parseFails :: forall a. (Eq a, Show a) => MDParser a -> String -> Result
parseFails p s = case runMDParser p s of
                      Left _ -> Success
                      Right e -> false
                        <?> "Falsely parsed: " ++ show s ++ "\n  into: " ++ show e ++ "\n\n"

main = do
  assert $ emph `parses` "*test*" $ Emphasized (Plain "test")
  assert $ emph `parses` "_test_" $ Emphasized (Plain "test")
  assert $ emph `parses` "*two words*" $ Emphasized (Plain "two words")
  assert $ emph `parseFails` "*word"
  assert $ emph `parseFails` "word*"
  assert $ emph `parseFails` "* not adjacent*"
  assert $ emph `parseFails` "* notadjacent*"
  assert $ emph `parseFails` "*not adjacent *"
  assert $ emph `parseFails` "*notadjacent *"

  assert $ strong `parses` "**strong**" $ Strong (Plain "strong")
  assert $ strong `parses` "__strong__" $ Strong (Plain "strong")
  assert $ strong `parses` "**two words**" $ Strong (Plain "two words")
  assert $ strong `parseFails` "**word"
  assert $ strong `parseFails` "word**"
  assert $ strong `parseFails` "**word*"
  assert $ strong `parseFails` "*word*"
  assert $ strong `parseFails` "** nope**"
  assert $ strong `parseFails` "** not adjacent**"
  assert $ strong `parseFails` "**nope **"
  assert $ strong `parseFails` "**not adjacent **"

  {-- assert $ (notFollowedBy $ string "s") `parses` "n" $ unit --}
  {-- assert $ (notFollowedBy $ string "s") `parseFails` "s" --}
  assert $ code `parses` "`code`" $ Code (CBInfo "") "code"
  assert $ code `parses` "``code``" $ Code (CBInfo "") "code"
  assert $ code `parses` "``co`de``" $ Code (CBInfo "") "co`de"
  assert $ code `parses` "` code `" $ Code (CBInfo "") "code"
  assert $ code `parses` "`` ` ``" $ Code (CBInfo "") "`"
  assert $ code `parses` "`` hard\nwrap ``" $ Code (CBInfo "") "hard wrap"
  assert $ code `parses` "`` multi\nhard\nwraps ``" $ Code (CBInfo "") "multi hard wraps"
  assert $ code `parseFails` "``"
  assert $ code `parseFails` "```nope``"
  assert $ code `parseFails` "```\n\nnew paragraph```"

  assert $ autolink `parses` "</path>" $ Link {text: Plain "/path", href: "/path", title: Nothing}
  assert $ autolink `parses` "<http://domain.com>" $ Link {text: Plain "http://domain.com", href: "http://domain.com", title: Nothing}
  assert $ autolink `parses` "</>" $ Link {text: Plain "/", href: "/", title: Nothing}
  assert $ autolink `parses` "<ryan@test.com>" $ Link {text: Plain "ryan@test.com", href: "mailto:ryan@test.com", title: Nothing}
  assert $ autolink `parseFails` "< nope>"
  assert $ autolink `parseFails` "<nope >"
  assert $ autolink `parseFails` "<not ok>"
  assert $ autolink `parseFails` "<>"

  assert $ inlinelink `parses` "[go](http://somewhere.com)" $ Link {text: Plain "go", href: "http://somewhere.com", title: Nothing}
  assert $ inlinelink `parses` "[section](#some-section)" $ Link {text: Plain "section", href: "#some-section", title: Nothing}
  assert $ inlinelink `parses` "[go](/here)" $ Link {text: Plain "go", href: "/here", title: Nothing}
  assert $ inlinelink `parses` "[go](/there (withtitle))" $ Link {text: Plain "go", href: "/there", title: Just "withtitle"}
  assert $ inlinelink `parses` "[go](/there 'withtitle')" $ Link {text: Plain "go", href: "/there", title: Just "withtitle"}
  assert $ inlinelink `parses` "[go](/there \"withtitle\")" $ Link {text: Plain "go", href: "/there", title: Just "withtitle"}
  assert $ inlinelink `parseFails` "[](#some-section)"
  assert $ inlinelink `parseFails` "[go] (/here)"
  assert $ inlinelink `parseFails` "[unbal]anced](/path)"
