{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Yesod.Form.Fields(FormMessage(..), defaultFormMessage)
import Yesod.Core

data App = App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance RenderRoute App => Yesod App

mkYesodData "App" [parseRoutes|
/ RootR GET POST
|]
