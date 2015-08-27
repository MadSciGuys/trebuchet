-- General
import Test.Tasty (defaultMain)

-- Trebuchet
import qualified Treb.Test.JSON as JSON

import qualified ExampleGen as EG

main :: IO ()
main = defaultMain JSON.tests
