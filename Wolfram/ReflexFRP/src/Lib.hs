{-# LANGUAGE RecursiveDo #-}

module Lib
    ( startApp ) where

import qualified Data.Map as M
import Reflex.Dom
import Data.IORef
import Data.Foldable
import Data.Bits
import Safe (readMay)

width :: Int
width = 100

height :: Int
height = 50

getCellValue :: Int -> Int -> Int -> Int -> Int
getCellValue rule a b c = 
    let x = 2^(a * 4 + b * 2 + c)
     in if rule .&. x == x then 1 else 0

wolfram :: Int -> [Int] -> [[Int]]
wolfram rule initial_state = 
    let arr = wolfram rule initial_state
     in map (\x -> map (\y ->
        let ax = x - 1
            bx = x
            cx = x + 1
            abcy = y - 1
         in if ax < 0 || cx >= width -- 範囲外だったらアウト
            then -1
            else
                if abcy == -1 -- 上端に来ていたら、initial_stateから出す
                then getCellValue rule (initial_state !! ax) (initial_state !! bx) (initial_state !! cx)
                else 
                    if arr !! ax !! abcy == -1 || arr !! cx !! abcy == -1 --それ以外の場合で、両端がアウトでなければ、arrから出す
                    then -1
                    else getCellValue rule (arr !! ax !! abcy) (arr !! bx !! abcy) (arr !! cx !! abcy)) $ take height [0..]) $ take width [0..]

button_attrsDyn :: MonadWidget t m => Dynamic t (M.Map String String) -> m (El t, Event t ())
button_attrsDyn attrsDyn = do
    (btn, _) <- elDynAttr' "div" attrsDyn $ text ""
    return (btn, domEvent Click btn)

normal_button :: MonadWidget t m => String -> m (El t, Event t ())
normal_button value = do
    (btn, _) <- el' "button" $ text value
    return (btn, domEvent Click btn)

makeStyle = el "style" $ text "\
\.cell {\
\    width: 12px;\
\    height:12px;\
\    position: absolute;\
\}\
\.button {\
\    width: 10px;\
\    height:10px;\
\    border: 1px gray solid;\
\}\
\.releWrapper {\
\    opacity: 0.8;\
\    z-index: 999;\
\    position: absolute;\
\    background: white;\
\    padding: 10px;\
\    top: 16px;\
\    left: 12px;\
\}"

makeCells = elClass "div" "wrapper" $ do
    inputs_arr <- mapM (\x -> mdo
        bool <- toggle False events
        num <- forDyn bool $ \b -> if b == True then 1 else 0
        attrsDyn <- forDyn num $ \n -> M.fromList [
            ("style", "background: " ++ (if n == 1 then "black" else "white") ++ "; top: 0px; left: " ++ (show $ x * 12) ++ "px"),
            ("class", "button cell")]
        (btn, events) <- button_attrsDyn attrsDyn
        return num) $ take width [0..]
    
    inputsDyn <- foldrM (\x y -> combineDyn (:) x y) (constDyn []) inputs_arr
    
    ruleDyn <- elClass "div" "releWrapper" $ do
        text "rulecode: "
        ruleInput <- textInput def
        ruleInputDyn <- mapDyn (maybe 90 id . readMay) (_textInput_value ruleInput)
        (button, click) <- normal_button "Set"
        ruleDyn <- foldDyn (\e p -> e) 90 $ tagDyn ruleInputDyn click
        el "br" $ do
            return ()
        text "current: "
        display ruleDyn
        return ruleDyn
    
    cell_state <- combineDyn (\rule inputs -> wolfram rule inputs) ruleDyn inputsDyn 
    
    cells <- mapM (\x -> mapM (\y -> do
        num <- forDyn cell_state $ \s -> s !! x !! y
        attrsDyn <- forDyn num $ \n -> M.fromList [
            ("style", "background: " ++ (if n == -1 then "orange" else if n == 0 then "white" else "black") ++ "; top: " ++ (show $ (y + 1) * 12) ++ "px; left: " ++ (show $ x * 12) ++ "px"),
            ("class", "cell")]
        elDynAttr "div" attrsDyn $ text ""
        return ()) $ take height [0..]) $ take width [0..]
    return ()

startApp :: IO ()
startApp = mainWidget $ do
    makeStyle
    makeCells