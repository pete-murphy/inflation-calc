module Format.Int where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Number as Number
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Data.String.Regex.Unsafe as Regex.Unsafe

nonNumbers :: Regex
nonNumbers = Regex.Unsafe.unsafeRegex "[^0-9]" Flags.global

thousandGroups :: Regex
thousandGroups = Regex.Unsafe.unsafeRegex "\\B(?=(\\d{3})+$)" Flags.global

leadingZeros :: Regex
leadingZeros = Regex.Unsafe.unsafeRegex "^0*" Flags.noFlags

formatString :: String -> String
formatString =
  Regex.replace nonNumbers ""
    >>> Regex.replace leadingZeros ""
    >>> Regex.replace thousandGroups ","

foreign import format :: Int -> String

commas :: Regex
commas = Regex.Unsafe.unsafeRegex "," Flags.global

unformat :: String -> Maybe Int
unformat =
  Regex.replace commas ""
    >>> Number.fromString
    >>> map Int.floor
