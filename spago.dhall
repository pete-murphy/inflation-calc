{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "react-starter"
, dependencies =
  [ "aff"
  , "arrays"
  , "checked-exceptions"
  , "codec-argonaut"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "fetch-core"
  , "foldable-traversable"
  , "integers"
  , "js-date"
  , "js-timers"
  , "maybe"
  , "newtype"
  , "nullable"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "strings"
  , "tuples"
  , "validation"
  , "web-dom"
  , "web-html"
  , "yoga-fetch"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
