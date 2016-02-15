import Control.Monad
import Data.List
import Data.Version (showVersion)
import NgLint.Linter
import NgLint.Matchers
import NgLint.Parser
import Paths_nglint (version)
import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.Parsec
import Text.Parsec.Error
import qualified NgLint.Output.Pretty as Pretty
import qualified NgLint.Output.Gcc as Gcc


data OutputFormat = Pretty | Gcc deriving (Show, Eq)

data Flag = Format OutputFormat deriving (Show, Eq)

data LinterConfig = LinterConfig
    { outputFormat :: OutputFormat }

defaultConfig = LinterConfig
    { outputFormat = Pretty }


printUsage :: IO ()
printUsage =
    putStrLn $ usageInfo header options
    where header = unlines [ "nglint - nginx configuration file linter"
                           , "version: " ++ showVersion version ]


lintFile :: (String -> [LintMessage] -> IO ()) -> FilePath -> IO [LintMessage]
lintFile formatMessages fileName = do
    content <- readFile fileName
    let config = parse configFile fileName content
    case config of
        Left error -> do
            print error
            return []
        Right (Config decls) -> do
            mapM_ (formatMessages content) messageGroups
            return messages
            where messages = lint decls
                  messageGroups = groupBy eq messages
                  eq (LintMessage p1 _) (LintMessage p2 _) = p1 == p2


formatp :: Maybe String -> Flag
formatp (Just str) =
    case str of
        "gcc" -> Format Gcc
        "pretty" -> Format Pretty
        _ -> error "unrecognized output format"
formatp Nothing = Format Pretty


options :: [OptDescr Flag]
options = [Option "f" ["format"] (OptArg formatp "FORMAT") "message output format"]


configFromOpts :: LinterConfig -> [Flag] -> LinterConfig
configFromOpts config (Format outputFormat : opts) =
    configFromOpts (config { outputFormat = outputFormat }) opts
configFromOpts config [] = config


getFormatter :: OutputFormat -> (String -> [LintMessage] -> IO ())
getFormatter Pretty = Pretty.printGroupedMessages
getFormatter Gcc = Gcc.printGroupedMessages


main :: IO ()
main = do
    args <- getArgs
    let (opts, nonOpts, errors) = getOpt Permute options args
        linterConfig = configFromOpts defaultConfig opts
    case length nonOpts of
        0 -> printUsage
        _ -> do
            let printMessages = getFormatter $ outputFormat linterConfig

            totalMessages <- mapM (lintFile printMessages) nonOpts
            let num = length $ concat totalMessages

            when (outputFormat linterConfig == Pretty) $
                putStrLn $ show num ++ " hints."

            if num > 0 then exitFailure else exitSuccess
