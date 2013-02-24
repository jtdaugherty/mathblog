module MB.Config
    ( readConfig
    , affirmative
    )
where

import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.Trans
import Data.Char (toLower)
import Data.ConfigFile.Parser
    ( parse_file
    )
import Data.Maybe
    ( fromJust
    , isNothing
    )
import Control.Monad
    ( forM_
    , when
    )

section :: String
section = "DEFAULT"

readConfig :: FilePath -> [String] -> EitherT String IO [(String, String)]
readConfig path requiredArgs = do
  result <- liftIO $ parse_file path

  case result of
    Left e -> left $ "Error parsing config file " ++ path ++ ": " ++ show e
    Right cfg ->
        do
          let pairs = fromJust $ lookup section cfg

          forM_ requiredArgs $ \k ->
            when (isNothing $ lookup k pairs) $
                 left $ "Missing required value for '" ++ k ++ "' in " ++ path

          return pairs

affirmative :: String -> Bool
affirmative s = aff $ toLower <$> s
    where
      aff "yes" = True
      aff "on" = True
      aff "1" = True
      aff _ = False