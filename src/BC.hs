{-# LANGUAGE DataKinds #-}
module BC where

import qualified Data.Map as Map
import Control.Lens.Plated
import AST (ModuleName, Block(..), NameStage(R), Line, Decl(..), _Import)

data Instruction
 = PushInt Int
 | PushString String
 | StaticCall String Int

getImports :: [Decl 'R] -> [ModuleName]
getImports = undefined --toListOf (traverse . cosmos . _Import)

-- TODO Control.Lens.Plated
-- TODO which part should move all top-level declarations to a MAIN-like fn
compile :: [Decl 'R] -> Module
compile decls = let (imports, vars, fns) = undefined decls -- split decls
                 in undefined

-- TODO global strings (function names etc)

data Module = Module -- TODO version?
  { name :: String
  , dependencies :: [[String]] -- this should include lower-level `Import`s (with a `uniq` at the end)
  , functions :: Map.Map String [Instruction]
  }
