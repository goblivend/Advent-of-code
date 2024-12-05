#!/bin/sh

is_sourced() {
   if [ -n "$ZSH_VERSION" ]; then
       case $ZSH_EVAL_CONTEXT in *:file:*) return 0;; esac
   else  # Add additional POSIX-compatible shell names here, if needed.
       case ${0##*/} in dash|-dash|bash|-bash|ksh|-ksh|sh|-sh) return 0;; esac
   fi
   return 1  # NOT sourced.
}

if ! `is_sourced`; then
    echo "use as '. $0'"
    echo exit 1
fi

LANGUAGE=$1
YEAR=`date '+%Y'`
DAY=`date '+%d'`

FOLDER=./$YEAR/Day$DAY/
mkdir -p $FOLDER

touch $FOLDER/shortinput.txt

case $LANGUAGE in
    "hs")
        cat <<EOF > $FOLDER/Day$DAY.$LANGUAGE
module Main where

import Data.Bits
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))
-- TODO: Cleanup imports after day done

type Input = [String]
type Output = Int

parseInput :: String -> Input
parseInput = lines

-- part1 :: Input -> Output
-- part1 input =

-- part2 :: Input -> Output
-- part2 input =

main :: IO ()
main = do
  args  <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print input

  -- print $ part1 input
  -- print $ part2 input
EOF

cat <<EOF > $FOLDER/Makefile

SRC=./Day$DAY.hs
TARGET=Day$DAY
all: \$(TARGET)

\$(TARGET): \$(SRC)
	ghc \$(SRC)

clean:
	\$(RM) ./Day$DAY ./Day$DAY.o ./Day$DAY.hi

.PHONY: all \$(TARGET) clean
EOF

;;

    "")
        echo "No language specified"
        ;;
esac

cd $FOLDER
