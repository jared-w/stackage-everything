{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}

module Lib where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as T
import           Data.Void
import           Network.HTTP.Req
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.Char           ( alphaNumChar
                                                , char
                                                , digitChar
                                                , space
                                                , string
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
                                                ( decimal )
import           Text.Read                      ( readMaybe )
import           Options.Generic                ( Generic
                                                , getRecord
                                                , ParseRecord(..)
                                                )
import           Data.Char                      ( isAlphaNum
                                                , isNumber
                                                )
import           System.Process
import           Data.List                      ( intersperse )

newtype Name = Name Text deriving (Eq, Ord, Show)
newtype Packages = Packages [(Name, Version)] deriving (Eq, Ord, Show)
data LtsVersion = LtsVersion Int Int deriving (Eq, Ord, Show)
data Version = Version Text | NoVersion deriving (Eq, Ord, Show)

newtype Opts = Opts { lts :: Text } deriving (Generic, Show)
instance ParseRecord Opts

main :: IO ()
main = do
  (Opts r) <- getRecord "Hackage Everything"
  case parseLtsVersion r of
    Left e -> do
      T.hPutStrLn stderr "Invalid version specified"
      T.hPutStrLn stderr (T.pack $ errorBundlePretty e)
    Right v -> do
      T.hPutStrLn stderr $ "Downloading " <> ltsTextVersion v
      generateScript v

ltsTextVersion :: LtsVersion -> Text
ltsTextVersion (LtsVersion major minor) =
  T.pack (show major) <> "." <> T.pack (show minor)

parseLtsVersion :: Text -> Either (ParseErrorBundle Text Void) LtsVersion
parseLtsVersion =
  parse (LtsVersion <$> L.decimal <* char '.' <*> L.decimal <* eof) "Wrong Version Format"

downloadCabalConstraints :: LtsVersion -> IO Text
downloadCabalConstraints ltsVersion = runReq defaultHttpConfig $ do
  response <- req
    GET
    (https "www.stackage.org" /: "lts-" <> ltsTextVersion ltsVersion /: "cabal.config")
    NoReqBody
    bsResponse
    mempty
  pure . T.decodeUtf8 $ responseBody response

generateScript :: LtsVersion -> IO ()
generateScript ltsVersion = do
  T.hPutStrLn stderr ("Downloading constraints for LTS-" <> ltsTextVersion ltsVersion)
  parse cabalConstraintFileP mempty <$> downloadCabalConstraints ltsVersion >>= \case
    Left  err      -> T.putStrLn "Parse error: " >> print err
    Right packages -> runScript ltsVersion packages

runScript :: LtsVersion -> Packages -> IO ()
runScript ltsVersion (Packages packages) =
  callCommand
    .  T.unpack
    .  T.concat
    .  intersperse " "
    $  [ "cd &&"
       , "stack build --dry-run --prefetch --resolver"
       , "lts-" <> ltsTextVersion ltsVersion
       ]
    <> [ n
       | (Name n, _) <- packages
       , n
         `notElem` [ "ghc"
                   , "rts"
                   , "ghc-boot"
                   , "ghc-boot-th"
                   , "ghc-heap"
                   , "ghci"
                   , "Win32-notify"
                   ]
       ]

-- | Ad-hoc parser for handling Stackage provided Cabal constraint files.
cabalConstraintFileP :: Parsec Void Text Packages
cabalConstraintFileP =
  manyTill anySingle (try (string "constraints:"))
    *> space
    *> packagesP
    <* manyTill anySingle eof

packagesP :: Parsec Void Text Packages
packagesP = Packages <$> sepBy packageP (char ',' *> space)

packageP :: Parsec Void Text (Name, Version)
packageP = do
  name <- Name . T.pack <$> some (alphaNumChar <|> char '-')
  space
  version <-
    let versionNumber = do
          _ <- string "=="
          v <- some (digitChar <|> char '.')
          pure . Version $ T.pack v
    in  versionNumber <|> (NoVersion <$ string "installed")
  space
  pure (name, version)
