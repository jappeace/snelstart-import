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

import Data.Vector(toList)
import qualified Data.ByteString.Lazy as LBS
import SnelstartImport.Convert
import SnelstartImport.ING
import SnelstartImport.Web.Routes
import SnelstartImport.N26
import Text.Blaze.Html(Html)
import Yesod.Form
import Yesod.Core.Widget
import Yesod.Core.Handler
import Data.Text
import Yesod.Core(defaultLayout)
import Data.Text.Encoding
import Data.ByteString.Base64
import Data.Base64.Types(extractBase64)


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

      case readN26BS $ LBS.fromStrict contents of
        Left err -> defaultLayout $ inputForm [pack err] enctype form
        Right n26 -> let
            csvOut :: LBS.ByteString
            csvOut = writeCsv (toING (ifBank suc) <$> toList n26)
            contentText = decodeUtf8 $ LBS.toStrict csvOut
            downloadText = "data:text/plain;base64," <> (extractBase64 $ encodeBase64 $ LBS.toStrict csvOut)
          in
            defaultLayout $ [whamlet|
                <table>
                  <tr>
                    <th>bank
                    <td>#{ifBank suc}
                  <tr>
                    <th>filename
                    <td>#{fileName $ ifFileInfo suc}

                <h2>contents
                <a href=#{downloadText} download="hello.txt"> Download
                <pre>
                  <code>
                    #{contentText}
                |]
