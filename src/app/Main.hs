import Control.Monad ( when )
import Control.Newtype.Generics
import System.Exit ( exitFailure, exitSuccess )
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
  let eAliases = (map mkAliasLine . toAliases . snd) <$> eContacts
  when (op DumpContacts . optDumpContacts $ opts) $
    display $ map show <$> either Left (Right . snd) eContacts
  when (op DumpFields . optDumpFields $ opts) $
    display $ map show <$> either Left (Right . fst) eContacts
  display eAliases


-- Display lines of whatever we have to display, or the error message, and then
-- get out of here!
display :: Either String [String] -> IO ()
display = either
  (\err -> hPutStrLn stderr err >> exitFailure)
  (\output -> mapM_ putStrLn output >> exitSuccess)
