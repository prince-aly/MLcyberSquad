module Main where

--------------------------------------------------------------------------------

import ANN.Simple

--------------------------------------------------------------------------------

main :: IO ()
main = do
    nn <- newRandomNetwork [3, 4, 5, 2]
    let testInput = [1, -1, 3]
    putStrLn $ "evaluating with " ++ show testInput
    putStrLn $ show (evaluateNetwork testInput nn)
