module Language.Grin.AST.BasicOperation (
  BasicOperation(..)
  ) where

import Language.Grin.AST.Type

-- ---------------------------------------------------------------------------

data BasicOperation primtypes
  = Demote                -- turn a node into an inode, always okay
  | Promote               -- turn an inode into a node, the inode _must_ already be a valid node
  | Eval -- evaluate an inode, returns a node representing the evaluated value. Bool is whether to update the inode
  | Apply ![Typ primtypes] -- apply a partial application to a value, returning the given type
  | StoreNode !Bool -- create a new node, Bool is true if it should be a direct node, the second val is the region
  | Redirect  -- write an indirection over its first argument to point to its second one
  | Overwrite -- overwrite an existing node with new data (the tag must match what was used for the initial Store)
  | PeekVal   -- read a value from a pointed to location
  | PokeVal   -- write a value to a pointed to location
  | Consume   -- consume a value, depending on the back end this may be used to free memory
  | GcTouch   -- touch a value, forcing the GC to hold onto it.
  | Coerce !(Typ primtypes) -- coerce one type to another, danger zone. This is for reflection/rts and not for integral conversions.
  | GcPush    -- push some pointers onto the GC stack, returning registers representing the values on the stack
  | NewRegister   -- create a new register
  | ReadRegister  -- read a register
  | WriteRegister -- write to a register
  deriving (Show, Eq, Ord)


-- ---------------------------------------------------------------------------




