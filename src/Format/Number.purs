module Format.Number where

import Prelude

import Data.Maybe (Maybe)
import Data.Number as Number
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Data.String.Regex.Unsafe as Regex.Unsafe

nonNumbers :: Regex
nonNumbers = Regex.Unsafe.unsafeRegex "[^0-9.]" Flags.global

trailingMoreThanTwoAfterDot :: Regex
trailingMoreThanTwoAfterDot = Regex.Unsafe.unsafeRegex "(\\.\\d{2}).*" Flags.noFlags

thousandGroups :: Regex
thousandGroups = Regex.Unsafe.unsafeRegex "\\B(?=(\\d{3})+(?=\\.|\\b))" Flags.global

leadingZeros :: Regex
leadingZeros = Regex.Unsafe.unsafeRegex "^0*" Flags.noFlags

formatString :: String -> String
formatString =
  Regex.replace nonNumbers ""
    >>> Regex.replace leadingZeros ""
    >>> Regex.replace trailingMoreThanTwoAfterDot "$1"
    >>> Regex.replace thousandGroups ","

foreign import format :: Number -> String

commas :: Regex
commas = Regex.Unsafe.unsafeRegex "," Flags.global

unformat :: String -> Maybe Number
unformat =
  Regex.replace commas ""
    >>> Number.fromString

