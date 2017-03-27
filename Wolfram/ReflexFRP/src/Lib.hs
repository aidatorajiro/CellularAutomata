{-# LANGUAGE RecursiveDo #-}

module Lib
    ( startApp ) where

import qualified Data.Map as M
import Reflex.Dom
import Data.IORef
import Data.Foldable
import Data.Bits

width :: Int
width = 100

height :: Int
height = 50

rule :: Int
rule = 90

getCellValue :: Int -> Int -> Int -> Int
getCellValue a b c = 
    let x = 2^(a * 4 + b * 2 + c)
     in if rule .&. x == x then 1 else 0

wolfram :: [Int] -> [[Int]]
wolfram initial_state = 
    let arr = wolfram initial_state
     in map (\x -> map (\y ->
        let ax = x - 1
            bx = x
            cx = x + 1
            abcy = y - 1
         in if ax < 0 || cx >= width -- 範囲外だったらアウト
            then -1
            else
                if abcy == -1 -- 上端に来ていたら、initial_stateから出す
                then getCellValue (initial_state !! ax) (initial_state !! bx) (initial_state !! cx)
                else 
                    if arr !! ax !! abcy == -1 || arr !! cx !! abcy == -1 --それ以外の場合で、両端がアウトでなければ、arrから出す
                    then -1
                    else getCellValue (arr !! ax !! abcy) (arr !! bx !! abcy) (arr !! cx !! abcy)) $ take height [0..]) $ take width [0..]

button_attrsDyn :: MonadWidget t m => Dynamic t (M.Map String String) -> m (El t, Event t ())
button_attrsDyn attrsDyn = do
    (btn, _) <- elDynAttr' "div" attrsDyn $ text ""
    return (btn, domEvent Click btn)

makeStyle = el "style" $ text ".cell { width: 12px; height:12px; position: absolute; } .button { width: 10px; height:10px; border: 1px gray solid; }"

makeCells = elClass "div" "wrapper" $ do
    inputs <- mapM (\x -> mdo
        bool <- toggle False events
        num <- forDyn bool $ \b -> if b == True then 1 else 0
        attrsDyn <- forDyn num $ \n -> M.fromList [
            ("style", "background: " ++ (if n == 1 then "black" else "white") ++ "; top: 0px; left: " ++ (show $ x * 12) ++ "px"),
            ("class", "button cell")]
        (btn, events) <- button_attrsDyn attrsDyn
        return num) $ take width [0..]
    
    inputs_dynarr <- foldrM (\x y -> combineDyn (:) x y) (constDyn []) inputs
    
    cell_state <- forDyn inputs_dynarr $ \a -> wolfram a
    
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