{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "arrays"
    , "canvas"
    , "console"
    , "effect"
    , "gen"
    , "js-timers"
    , "lists"
    , "ordered-collections"
    , "psci-support"
    , "random"
    , "refs"
    , "transformers"
    , "tuples"
    , "unfoldable"
    , "web-html"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
