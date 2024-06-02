module Main where

import Miscellaneous (degsToRads)
import H3ErrorCodes
import Foreign.ForeignPtr (withForeignPtr)

main :: IO ()
main = do
  putStrLn $ "90 degress in radians: " ++ show (degsToRads 90)
  exampleGeoPolygon

exampleGeoPolygon :: IO ()
exampleGeoPolygon = do
  let coords = LatLng (degsToRads 45) (degsToRads (-72.5))
      (status, h3index) = c2hs_latLngToCell coords 9
      (_, fptr) = c2hs_cellsToLinkedMultiPolygon [h3index]
  gps <- withForeignPtr fptr extractGeoPolygons
  putStrLn $ show gps
  
  putStrLn $ "H3 indices: " ++ show [h3index]
  (h3error2, gps2) <- hs_cellsToLinkedMultiPolygon [h3index]
  putStrLn $ "Second attempt: " ++ show gps2
  putStrLn $ "Second attempt status: " ++ show h3error2
  
  putStrLn $ "Results are the same: " ++ show (gps == gps2)

