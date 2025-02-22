{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}


-- | small web frontend
module SnelstartImport.Web
  ( webMain
  )
where

import SnelstartImport.Options
import Yesod.Core.Dispatch(mkYesodDispatch)
import Yesod.Core(Yesod(..), toWaiApp)
import Text.Blaze.Html(Html)
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
