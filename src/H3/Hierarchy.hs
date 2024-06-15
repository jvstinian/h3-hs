module H3.Hierarchy
  ( cellToParent 
  , cellToCenterChild 
  , cellToChildPos
  , childPosToCell
  , cellToChildren
  , compactCells
  , uncompactCells
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
  )
import H3.Internal.Utils (toEither)


-- | Provides the parent index containing @cell@
cellToParent :: H3Index -- ^ cell
             -> Int     -- ^ parentRes 
             -> Either H3ErrorCodes H3Index
cellToParent cell = toEither . c2hs_cellToParent cell

-- | Provides the center child index contained by @cell@ at resolution @childRes@.
cellToCenterChild :: H3Index -- ^ cell
                  -> Int     -- ^ childRes
                  -> Either H3ErrorCodes H3Index
cellToCenterChild cell = toEither . c2hs_cellToCenterChild cell

-- | Returns the position of the child cell within an ordered list of all children of the 
--   cell's parent at the specified resolution @parentRes@. 
--   The order of the ordered list is the same as that returned by 'cellToChildren'. 
--   This is the complement of 'childPosToCell'.
cellToChildPos :: H3Index -- ^ child
               -> Int     -- ^ parentRes
               -> Either H3ErrorCodes Int64
cellToChildPos child = toEither . c2hs_cellToChildPos child

-- | Returns the child cell at a given position within an ordered list of all children of @parent@
--   at the specified resolution @childRes@. 
--   The order of the ordered list is the same as that returned by 'cellToChildren'. 
--   This is the complement of 'cellToChildPos'.
childPosToCell :: Int64   -- ^ childPos
               -> H3Index -- ^ parent
               -> Int     -- ^ childRes
               -> Either H3ErrorCodes H3Index
childPosToCell childPos parent = toEither . c2hs_childPosToCell childPos parent 

-- | Returns children with the indexes contained by @cell@ at resolution @childRes@.
cellToChildren :: H3Index -- ^ cell
               -> Int     -- ^ childRes
               -> Either H3ErrorCodes [H3Index]
cellToChildren cell = toEither . hsCellToChildren cell

-- | Compacts the set @cellSet@ of indexes as best as possible.  
--   Cells in @cellSet@ must all share the same resolution.
compactCells :: [H3Index] -- ^ cellSet
             -> Either H3ErrorCodes [H3Index]
compactCells  = toEither . hsCompactCells 

-- | Uncompacts the set @compactedSet@ of indexes to the resolution @res@
uncompactCells :: [H3Index] -- ^ compactedSet
               -> Int       -- ^ res
               -> Either H3ErrorCodes [H3Index]
uncompactCells compactedSet = toEither . hsUncompactCells compactedSet

