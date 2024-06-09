module H3.Hierarchy
  ( cellToParent 
  , cellToCenterChild 
  , cellToChildPos
  , childPosToCell
  , cellToChildren
  , compactCells
  , uncompactCells
  , uncompactCellsUsingSize
  ) where

import Data.Int (Int64)
import H3.Internal.H3Api 
  ( H3ErrorCodes
  , H3Index
  , c2hs_cellToParent
  , c2hs_cellToCenterChild
  , c2hs_cellToChildPos
  , c2hs_childPosToCell 
  )
import H3.Internal.FFI 
  ( hsCellToChildren
  , hsCompactCells 
  , hsUncompactCells 
  , hsUncompactCellsUsingSize 
  )
import H3.Internal.Utils (toEither)


cellToParent :: H3Index -> Int -> Either H3ErrorCodes H3Index
cellToParent cell = toEither . c2hs_cellToParent cell

cellToCenterChild :: H3Index -> Int -> Either H3ErrorCodes H3Index
cellToCenterChild cell = toEither . c2hs_cellToCenterChild cell

cellToChildPos :: H3Index -> Int -> Either H3ErrorCodes Int64
cellToChildPos child = toEither . c2hs_cellToChildPos child

childPosToCell :: Int64 -> H3Index -> Int -> Either H3ErrorCodes H3Index
childPosToCell childPos parent = toEither . c2hs_childPosToCell childPos parent 

cellToChildren :: H3Index -> Int -> Either H3ErrorCodes [H3Index]
cellToChildren cell = toEither . hsCellToChildren cell

compactCells :: [H3Index] -> Either H3ErrorCodes [H3Index]
compactCells  = toEither . hsCompactCells 

-- TODO: Should we keep the following?
uncompactCells :: [H3Index] -> Int64 -> Int -> Either H3ErrorCodes [H3Index]
uncompactCells compactedSet maxCells = toEither . hsUncompactCells compactedSet maxCells

uncompactCellsUsingSize :: [H3Index] -> Int -> Either H3ErrorCodes [H3Index]
uncompactCellsUsingSize compactedSet = toEither . hsUncompactCellsUsingSize compactedSet

