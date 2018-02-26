#!/usr/bin/env stack
{- stack --resolver lts-10.7 --install-ghc runghc
    --package base
    --package bytestring
    --package lens
    --package megaparsec
    --package prettyprinter
    --package text
    --package wreq
    --
    -hide-all-packages
-}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where



import           Control.Applicative
import           Control.Lens
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Semigroup
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text.Encoding                    as T
import qualified Data.Text.IO                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Void
import           Network.Wreq
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Megaparsec                       as P
import           Text.Megaparsec.Char                  as P
import           Text.Read                             (readMaybe)



data LtsVersion = LtsVersion Int Int deriving (Eq, Ord, Show)
ltsTextVersion :: LtsVersion -> Text
ltsTextVersion (LtsVersion major minor) = T.pack (show major) <> "." <> T.pack (show minor)

newtype Name = Name Text deriving (Eq, Ord, Show)
data Version = Version Text | NoVersion deriving (Eq, Ord, Show)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ltsFlag : ltsVersion : [] | ltsFlag == "--lts"
            -> case parseLtsVersion (T.pack ltsVersion) of
                Left err -> do
                    T.hPutStrLn stderr "Invalid version specified"
                    T.hPutStrLn stderr err
                    exitWith (ExitFailure 1)
                Right v -> generateScript v
        _other -> do
            T.hPutStrLn stderr "Usage: ./Generate.hs --lts <version>"
            T.hPutStrLn stderr "Version: e.g. 10.5 for LTS-10.5"
            exitWith (ExitFailure 1)

parseLtsVersion :: Text -> Either Text LtsVersion
parseLtsVersion input = case parse version "<LTS version parameter>" input of
    Left err -> Left (T.pack (parseErrorPretty err))
    Right r  -> Right r
  where
    version :: Parsec Void Text LtsVersion
    version = do
        (major', minor') <- (,) <$> some digitChar <* char '.' <*> some digitChar <* eof
        case liftA2 (,) (readMaybe major') (readMaybe minor') of
            Nothing -> fail (T.unpack ("Error: " <> input <> " is not a valid LTS version number. Format: 12.3"))
            Just (major, minor) -> pure (LtsVersion major minor)

downloadCabalConstraints :: LtsVersion -> IO Text
downloadCabalConstraints ltsVersion = do
    response <- get ("https://www.stackage.org/lts-" ++ T.unpack (ltsTextVersion ltsVersion) ++ "/cabal.config")
    pure (T.decodeUtf8 (BSL.toStrict (view responseBody response)))

generateScript :: LtsVersion -> IO ()
generateScript ltsVersion = do
    T.hPutStrLn stderr ("Downloading constraints for LTS-" <> ltsTextVersion ltsVersion)
    contents <- downloadCabalConstraints ltsVersion
    T.hPutStrLn stderr "Parsing constraints file"
    case parse cabalConstraintFileP "" contents of
        Left err       -> T.putStrLn "Parse error: " >> print err
        Right packages -> renderScript ltsVersion packages

renderScript :: LtsVersion -> Packages -> IO ()
renderScript ltsVersion (Packages packages) = (render . layout . vcat)
    [ "#!/usr/bin/env bash"
    , ""
    , "set -euo pipefail"
    , ""
    , hang 4 (backslashedLineBreaks
        (pretty ("stack build --dry-run --prefetch --resolver lts-" <> ltsTextVersion ltsVersion)
        : [pretty pkgName | (Name pkgName, _) <- packages
                          , pkgName /= "ghc" -- Tends to misbehave so we exclude it here
                          , pkgName /= "rts" -- Dito
                          ]))
    , ""
    ]
  where
    render = renderIO stdout
    layout = layoutPretty defaultLayoutOptions
    backslashedLineBreaks = concatWith (\x y -> x <> " \\" <> line <> y)

-- | A list of packages and their respective versions.
newtype Packages = Packages [(Name, Version)]
    deriving (Eq, Ord, Show)

-- | Ad-hoc parser for handling Stackage provided Cabal constraint files.
cabalConstraintFileP :: Parsec Void Text Packages
cabalConstraintFileP = manyTill anyChar (try (string "constraints:"))
     *> P.space
     *> packagesP
     <* manyTill anyChar eof

  where

    packagesP :: Parsec Void Text Packages
    packagesP = fmap Packages (sepBy packageP comma')
      where
        comma' = char ',' *> P.space

    packageP :: Parsec Void Text (Name, Version)
    packageP = do
        name <- do n <- some (alphaNumChar <|> char '-')
                   (pure . Name . T.pack) n
        P.space
        version <- do
            let versionNumber = do
                    _ <- string "=="
                    v <- some (digitChar <|> char '.')
                    pure (Version (T.pack v) )
                merelyInstalled = NoVersion <$ string "installed"
            versionNumber <|> merelyInstalled
        P.space
        pure (name, version)



generateOutputFiles :: Packages -> Text -> Text -> IO ()
generateOutputFiles packages ltsVersion generatedPackageVersion = do
    putStrLn "Generating output files"
    T.writeFile "output/stackage-everything.cabal"
                (renderCabalFile packages ltsVersion generatedPackageVersion)
    T.writeFile "output/stack.yaml"
                (renderStackYaml ltsVersion)
    T.writeFile "output/README.md"
                renderReadme
    T.writeFile "output/Setup.hs"
                renderSetupHs



-- | Template to generate the .cabal file
renderCabalFile :: Packages -> Text -> Text -> Text
renderCabalFile packages ltsVersion generatedPackageVersion = T.intercalate "\n"
    [ "name:          stackage-everything"
    , "version:       " <> generatedPackageVersion
    , "synopsis:      Meta-package to depend on all of Stackage LTS " <> ltsVersion <> "."
    , "description:"
    , "    This meta-package depends on the entirety of Stackage."
    , "    ."
    , "    See README.md for further details."
    , ""
    , "license:       PublicDomain"
    , "author:        David Luposchainsky <dluposchainsky(λ)gmail.com>"
    , "maintainer:    David Luposchainsky <dluposchainsky(λ)gmail.com>"
    , "build-type:    Simple"
    , "homepage:      https://github.com/quchen/stackage-everything"
    , "bug-reports:   https://github.com/quchen/stackage-everything/issues"
    , "category:      Development"
    , "cabal-version: >=1.10"
    , "extra-source-files: README.md"
    , ""
    , "source-repository head"
    , "    type:     git"
    , "    location: https://github.com/quchen/stackage-everything"
    , ""
    , "library"
    , "    exposed-modules:  Development.Stack.Everything.Dummy"
    , "    hs-source-dirs:   src"
    , "    default-language: Haskell2010"
    , buildDepends packages ]

  where

    buildDepends :: Packages -> Text
    buildDepends (Packages p) =
        buildDependsStr <> T.intercalate separator (foldMap renderPackage p)
      where
        buildDependsStr = "    build-depends:    "
        separator = mconcat
            [ "\n"
            , T.replicate (T.length buildDependsStr - T.length ", ") " "
            , ", " ]
        renderPackage (Name n, _)
            | n == "base" = ["base < 127"] -- To make `cabal check` happy
        renderPackage (Name n, Version v) = [n <> " == " <> v ]
        renderPackage (Name n, NoVersion) = [n]



-- | Template to generate the stack.yaml file
renderStackYaml :: Text -> Text
renderStackYaml ltsVersion = T.intercalate "\n"
    [ "resolver: lts-" <> ltsVersion
    , ""
    , "packages:"
    , "    - '.'"
    , ""
    , "extra-deps: []"
    , ""
    , "flags: {}"
    , ""
    , "extra-package-dbs: []" ]



renderReadme :: Text
renderReadme = T.unlines
    [ "stackage-everything"
    , "-------------------"
    , ""
    , "This meta package depends on the entirety of Stackage."
    , ""
    , "The purpose of this is making Stackage available offline, which can be useful"
    , "if you're in an area with poor or no internet connectivity, such as airplanes"
    , "or rural areas."
    , ""
    , "Use `stack build` with appropriate parameters to make use of it."
    , "For example, to download all the source files so they can be installed"
    , "without an internet connection later, run"
    , ""
    , "```bash"
    , "stack build --prefetch --dry-run --only-dependencies"
    , "```" ]


renderSetupHs :: Text
renderSetupHs = T.unlines
    [ "import Distribution.Simple"
    , "main = defaultMain" ]
