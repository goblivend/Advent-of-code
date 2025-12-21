import AOC.Cli (orDefault)
import AOC2025 (run2025)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Time.Calendar
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime)
import System.Console.GetOpt
import System.Environment (getArgs)

date :: IO (Integer, Int, Int) -- :: (year, month, day)
date = getCurrentTime >>= return . toGregorian . utctDay

years :: Map Integer (Int -> [String] -> IO ())
years = M.fromAscList [(2025, run2025)]

data Flag = Year Integer | Day Int | Help
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['y'] ["year"] (ReqArg (Year . read) "N") "Target year of puzzle (defaults to current year)",
    Option ['d'] ["day"] (ReqArg (Day . read) "N") "Target Day of puzzle (defaults to current day)",
    Option ['h'] ["help"] (NoArg Help) "display the help message"
  ]

main :: IO ()
main = do
  args <- getArgs
  (currYear, _, currDay) <- date
  let (flags, dailyFlags, _) = getOpt RequireOrder options args
      year = currYear `orDefault` [y | Year y <- flags]
      day = currDay `orDefault` [d | Day d <- flags]
      help = Help `elem` flags
  if help
    then putStrLn $ usageInfo "Usage: ./AdventOfCode [-y N | -d N | -h] -- [DAILY OPTIONS]" options
    else do
      (years M.! year) day dailyFlags
