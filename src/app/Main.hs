import Control.Monad ( forM_ )

import GcAlias.Contact


main :: IO ()
main = do
  ecs <- importContacts "util/resources/all-contacts.csv"
  case ecs of
    Left err -> putStrLn err
    Right cs -> forM_ cs print
