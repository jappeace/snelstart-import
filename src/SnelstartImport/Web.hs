{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- | small web frontend
module SnelstartImport.Web
  ( webMain
  )
where

import Data.Text
import           Options.Applicative
import SnelstartImport.Options
import Yesod.Core.Dispatch(parseRoutes, mkYesodData, mkYesodDispatch)
import Yesod.Core(RenderRoute(..), Yesod(..), RenderMessage(..), toWaiApp)
import Text.Blaze.Html(Html)
import Yesod.Form.Fields(FormMessage(..), defaultFormMessage)
import Network.Wai.Handler.Warp(run)
import SnelstartImport.Web.Routes

getRootR :: Handler Html
getRootR =
  pure "hello world"

mkYesodDispatch "App" resourcesApp

instance Yesod App

webMain :: WebOptions -> IO ()
webMain options = do
  waiApp <- toWaiApp App
  run (webPort options) waiApp
