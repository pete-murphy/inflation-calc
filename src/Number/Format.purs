module Number.Format where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Number as Number
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Data.String.Regex.Unsafe as Regex.Unsafe

nonNumbers :: Regex
-- nonNumbers = Regex.Unsafe.unsafeRegex "[^0-9.]" Flags.global
nonNumbers = Regex.Unsafe.unsafeRegex "[^0-9]" Flags.global

-- trailingMoreThanTwoAfterDot :: Regex
-- trailingMoreThanTwoAfterDot = Regex.Unsafe.unsafeRegex "(\\.\\d{2}).*" Flags.noFlags

thousandGroups :: Regex
-- thousandGroups = Regex.Unsafe.unsafeRegex "\\B(?=(\\d{3})+(?=\\.|\\b))" Flags.global
thousandGroups = Regex.Unsafe.unsafeRegex "\\B(?=(\\d{3})+$)" Flags.global

leadingZeros :: Regex
leadingZeros = Regex.Unsafe.unsafeRegex "^0*" Flags.noFlags

formatString :: String -> String
formatString =
  Regex.replace nonNumbers ""
    >>> Regex.replace leadingZeros ""
    -- >>> Regex.replace trailingMoreThanTwoAfterDot "$1"
    >>> Regex.replace thousandGroups ","

foreign import formatNumber :: Number -> String

foreign import formatInt :: Int -> String

commas :: Regex
commas = Regex.Unsafe.unsafeRegex "," Flags.global

unformatNumber :: String -> Maybe Number
unformatNumber =
  Regex.replace commas ""
    >>> Number.fromString

unformatInt :: String -> Maybe Int
unformatInt =
  Regex.replace commas ""
    >>> Number.fromString
    >>> map Int.floor
