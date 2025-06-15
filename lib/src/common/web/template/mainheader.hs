{-# LANGUAGE OverloadedStrings #-}

module Common.Web.Template.MainHeader where

import Common.Web.Attributes
import Data.Text (Text)
import Lucid

mainHeaderTemplate :: Text -> Html ()
mainHeaderTemplate current = do
  header_ [class_ "nav"] $ do
    h1_  [ class_ "logo"
         , hxGet_ "/ui/projects/vw"
         , hxPushUrl_ True
         , hxSwap_ "innerHTML"
         , hxTarget_ "#container"
         ] "textio"
    h2_  []              (toHtml current)
    div_ [class_ "dot"]  mempty
    
