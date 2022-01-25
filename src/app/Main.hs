-- import Control.Newtype.Generics
import System.Exit ( exitFailure )
import System.IO
  ( BufferMode (NoBuffering)
  , hPutStrLn, hSetBuffering, stderr, stdout )

import GcAlias.Alias
import GcAlias.Contact
import GcAlias.Opts
import GcAlias.TarGz


main :: IO ()
main = do
  mapM_ (flip hSetBuffering $ NoBuffering) [stderr, stdout]
  opts <- parseOpts
  eCsvContents <- accessFile (optArchivePath opts) (optCsvPath opts)
  let eContacts = importContacts =<< eCsvContents
  let eAliases = (map mkAliasLine . toAliases) <$> eContacts
  case eAliases of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right as -> mapM_ putStrLn as
