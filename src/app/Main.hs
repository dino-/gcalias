import Control.Monad ( forM_ )

import GcAlias.Alias
import GcAlias.Contact


main :: IO ()
main = do
  ecs <- importContacts "util/resources/my-contacts.csv"
  case ecs of
    Left err -> putStrLn err
    -- Right cs -> forM_ cs print
    Right cs -> forM_ (map mkAliasLine $ toAliases cs) putStrLn
