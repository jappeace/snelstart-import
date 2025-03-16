{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- | layout, deal with styling
module SnelstartImport.Web.Layout
  ( layout
  )
where

import SnelstartImport.Web.Routes
import Text.Blaze.Html(Html)
import Yesod.Core.Widget
import Yesod.Core(defaultLayout, lucius)

layout :: Widget -> Handler Html
layout widget = do
  defaultLayout $ do
    toWidget [lucius|
      html {
        text-align: center;
      }
      body {
        text-align: left;
        margin: 0 auto;
        width: 50%;
        max-width: 25em;
        padding: 1em;
      }
      |]
    widget
