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
import Yesod.Core(toWaiApp)
import Network.Wai.Handler.Warp(run)
import SnelstartImport.Web.Routes
import SnelstartImport.Web.Handler

mkYesodDispatch "App" resourcesApp


webMain :: WebOptions -> IO ()
webMain options = do
  putStrLn $ "listening on port " <> show (webPort options)
  waiApp <- toWaiApp App
  run (webPort options) waiApp
