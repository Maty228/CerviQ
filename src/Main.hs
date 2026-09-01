{-|
Module      : Main
Description : Executable entry point of the CerviQ application.

This module intentionally contains no application logic. It delegates startup
to 'App.runCerviQ', where the graphical application, available agents, scenarios,
event handling, rendering, and updates are initialized.
-}

module Main (main) where

import App (runCerviQ)


-- | Starts the CerviQ application.
main :: IO ()
main = runCerviQ