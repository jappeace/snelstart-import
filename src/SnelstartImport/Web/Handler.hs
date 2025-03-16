{-# LANGUAGE RecordWildCards #-}
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
import Data.Text(Text, pack)
import Data.Text.Encoding
import Data.ByteString.Base64
import Data.Base64.Types(extractBase64)
import qualified Data.Text as Text
import SnelstartImport.SepaDirectCoreScheme(readSepaDirectCoreScheme)
import SnelstartImport.Web.Layout(layout)
import Yesod.Core(lucius)
import SnelstartImport.Web.Message
import Data.Time
import Control.Monad.IO.Class(liftIO)


type Form a = Html -> MForm Handler (FormResult a, Widget)


data InputFileForm = InputFileForm {
  ifBank :: Text ,
  ifFileInfo :: FileInfo
  }

inputFileForm :: Form InputFileForm
inputFileForm csrf = do
  (bankRes, bankView) <- mreq textField "own bank account" Nothing
  (inputRes, inputView) <- mreq fileField "xml file" Nothing

  let view = do
       toWidget [lucius|
          form label {
            width: 100%;
            display: inline-block;
          }
          form input{
            margin-bottom: 1em;
          }
       |]
       [whamlet|
        ^{csrf}
        <div>
          <label for=#{fvId bankView}>_{MsgOwnBank}
          ^{fvInput bankView}
        <div>
          <label for=#{fvId bankView}>_{MsgXmlFile}
          ^{fvInput inputView}
        <div>
          <button type=submit >_{MsgConvert}
  |]
  pure $ (InputFileForm <$> bankRes <*> inputRes, view)

getRootR :: Handler Html
getRootR = do
  ((_res, form), enctype) <- runFormPost inputFileForm
  layout $ inputForm [] enctype form

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
<h1>_{MsgTitle}
^{issuesWidget}
<form method=post enctype=#{enctype}>
    ^{form}
|]

postRootR :: Handler Html
postRootR = do
  ((res, form), enctype) <- runFormPost inputFileForm

  case res of
    FormMissing     -> layout $ inputForm ["error - missing data"] enctype form
    FormFailure x   -> layout $ inputForm x enctype form
    FormSuccess formRes -> do
      contents <- fileSourceByteString $ ifFileInfo formRes
      let filename = fileName $ ifFileInfo formRes
      if Text.isSuffixOf "xml" filename then
        case readSepaDirectCoreScheme contents of
          Left err -> layout $ inputForm [pack $ show err] enctype form
          Right res' -> renderDownload formRes (sepaDirectCoreSchemeToING (ifBank formRes) <$> res')
      else case readN26BS $ LBS.fromStrict contents of
        Left err -> layout $ inputForm [pack err] enctype form
        Right n26 -> renderDownload formRes (n26ToING (ifBank formRes) <$> toList n26)

renderDownload :: InputFileForm -> [ING] -> Handler Html
renderDownload form ings =
          let
            csvOut  = (writeCsv ings)
            contentText = decodeUtf8 $ LBS.toStrict csvOut
            downloadText = "data:text/plain;base64," <> (extractBase64 $ encodeBase64 $ LBS.toStrict csvOut)
          in
          layout $ do
            curTime <- liftIO getCurrentTime
            let
                timeStr :: String
                timeStr = formatTime defaultTimeLocale "%F_%H-%M" curTime
                downloadFileName = "snelstart-import-" <> timeStr <> ".csv"
            toWidget [lucius|
                pre {
                  overflow: scroll;
                  width: 100%;
                  position: absolute;
                  left: 0;
                  background-color: lightgray;
                  padding: 1em;
                }
            |]
            [whamlet|
                <table>
                  <tr>
                    <th>_{MsgBank}
                    <td>#{ifBank form }
                  <tr>
                    <th>_{MsgFilename}
                    <td>#{fileName $ ifFileInfo form }

                <h2>_{MsgContents}
                <a href=#{downloadText} download=#{downloadFileName}>_{MsgDownload}
                <pre>
                  <code>
                    #{contentText}
                |]
