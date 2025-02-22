{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- | routes for the app, seperated due to stage restriction
module SnelstartImport.Web.Routes
  (
    resourcesApp,
    App(..),
    Handler,
    Route(..),
    Widget
  )
where

import Yesod.Core.Dispatch(parseRoutes, mkYesodData)
import Yesod.Core(RenderRoute(..), RenderMessage(..))
import Yesod.Form.Fields(FormMessage(..), defaultFormMessage)

data App = App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage


mkYesodData "App" [parseRoutes|
/ RootR GET
|]
