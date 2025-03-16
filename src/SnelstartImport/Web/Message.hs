{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- | translations
module SnelstartImport.Web.Message where


import Text.Shakespeare.I18N
import SnelstartImport.Web.Routes

mkMessage "App" "i18n" "en"
