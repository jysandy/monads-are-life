{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "react-basic-starter"
, dependencies =
  [ "affjax"
  , "argonaut-generic"
  , "console"
  , "effect"
  , "generics-rep"
  , "psci-support"
  , "react-basic"
  , "react-basic-hooks"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
