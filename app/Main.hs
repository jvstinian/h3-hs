module Main where

import H3.Miscellaneous (degsToRads)

main :: IO ()
main = do
  putStrLn $ "90 degress in radians: " ++ show (degsToRads 90)

