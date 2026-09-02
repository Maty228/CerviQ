{-|
Module      : Main
Description : Cabal compatibility entry point for the CerviQ package.

Cabal invokes this conventional setup program when building the package with
the @Simple@ build type. Application startup lives in @app/Main.hs@.
-}

import Distribution.Simple


-- | Delegates package setup to Cabal's default implementation.
main :: IO ()
main = defaultMain
