{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- | handlers
module SnelstartImport.Web.Handler
  ( getRootR
  , postRootR
  )
where

import SnelstartImport.Web.Routes
import Text.Blaze.Html(Html)
import Yesod.Form
import Yesod.Core.Widget
import Yesod.Core.Handler
import Data.Text
import Yesod.Core(defaultLayout)
import Data.Text.Encoding


type Form a = Html -> MForm Handler (FormResult a, Widget)

data InputFileForm = InputFileForm {
  ifBank :: Text ,
  ifFileInfo :: FileInfo
  }

inputFileForm :: Form InputFileForm
inputFileForm csrf = do
  (bankRes, bankView) <- mreq textField "bank" Nothing
  (inputRes, inputView) <- mreq fileField "file" Nothing

  let view =
       [whamlet|
        ^{csrf}
        <div>
          ^{fvInput bankView}
        <div>
          ^{fvInput inputView}
        <div>
          <button type=submit >convert
  |]
  pure $ (InputFileForm <$> bankRes <*> inputRes, view)

getRootR :: Handler Html
getRootR = do
  ((res, form), enctype) <- runFormPost inputFileForm
  defaultLayout $ inputForm [] enctype form

inputForm :: [Text] -> Enctype -> Widget -> Widget
inputForm issues enctype form =
  let
    issuesWidget = if Prelude.null issues then "" else
      [whamlet|
        <ul>
          $forall issue <- issues
            <li> #{issue}
      |]
  in

  [whamlet|
^{issuesWidget}
<form method=post enctype=#{enctype}>
    ^{form}
|]

postRootR :: Handler Html
postRootR = do
  ((res, form), enctype) <- runFormPost inputFileForm

  case res of
    FormMissing     -> defaultLayout $ inputForm ["error - missing data"] enctype form
    FormFailure x   -> defaultLayout $ inputForm x enctype form
    FormSuccess suc -> do
      contents <- fileSourceByteString $ ifFileInfo suc



      let contentText = decodeUtf8 contents

      defaultLayout $ [whamlet|
              <table>
                <tr>
                  <th>bank
                  <td>#{ifBank suc}
                <tr>
                  <th>filename
                  <td>#{fileName $ ifFileInfo suc}

              <h2>contents
              <pre>
                #{contentText}
              |]
