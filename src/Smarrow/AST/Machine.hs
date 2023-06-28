module Smarrow.AST.Machine where

import Smarrow.AST.Names
import Smarrow.AST.Surface

------------------------------------------------------------------------

data Machine = Machine
  { machineName     :: MachineName
  , machineRefines  :: Maybe MachineName
  , machineState    :: StateDecl
  , machineLanguage :: LangDecl
  , machineFunction :: Expr
  }

data LangDecl = LangDecl
  { ldTypes :: [(Type, Type)]
  }
  deriving (Show, Read)

data StateDecl = StateDecl
  { sdType :: Type
  }
  deriving Show
