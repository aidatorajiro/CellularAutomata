{-# LANGUAGE RecursiveDo #-}

module Lib
    ( startApp ) where

import Data.Map
import Reflex.Dom

button_attrsDyn :: MonadWidget t m => Dynamic t (Map String String) -> m (El t, Event t ())
button_attrsDyn attrsDyn = do
    (btn, _) <- elDynAttr' "div" attrsDyn $ text ""
    return (btn, domEvent Click btn)

startApp :: IO ()
startApp = mainWidget $ do
    el "style" $ do
        text ".button { width: 10px; height:10px; border: 1px gray solid; }"
    elClass "div" "wrapper" $ mdo
        bool <- toggle False events
        attrsDyn <- forDyn bool $ \b -> fromList [ ("style", if b == True then "background: black;" else "background: white;"), ("class", "button") ]
        (btn, events) <- button_attrsDyn attrsDyn
        return ()