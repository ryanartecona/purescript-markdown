module Test.Main where

import SlamDown
import Test.StrongCheck
import Data.Either

parses :: forall a. (Eq a, Show a) => MDParser a -> String -> a -> Result
parses p s t = case runMDParser p s of
                    Left _ -> false
                      <?> "Failed to parse: " ++ show s ++ "\n  into: " ++ show t ++ "\n\n"
                    Right a -> a == t
                      <?> "Expected: " ++ show t ++ "\n\n  Actual: " ++ show a ++ "\n\n"

main = do
  assert $ emph `parses` "*test*" $ Emphasized (Plain "test")
