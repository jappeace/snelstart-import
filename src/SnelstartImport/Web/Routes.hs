{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-splices #-}

-- | routes for the app, seperated due to stage restriction
module SnelstartImport.Web.Routes
  (
    resourcesApp,
    App(..),
    Handler,
    Route(..)
  )
where

import Data.Text
import           Options.Applicative
import SnelstartImport.Options
import Yesod.Core.Dispatch(parseRoutes, mkYesodData)
import Yesod.Core(RenderRoute(..), Yesod(..), RenderMessage(..), toWaiApp)
import Text.Blaze.Html(Html)
import Yesod.Form.Fields(FormMessage(..), defaultFormMessage)
import Network.Wai.Handler.Warp(run)

data App = App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage


mkYesodData "App" [parseRoutes|
/ RootR GET
|]
