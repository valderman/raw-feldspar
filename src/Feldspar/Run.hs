-- | Monad for running Feldspar programs and C code back ends

module Feldspar.Run
  ( -- * Front end
    module Feldspar
  , module Feldspar.Run.Frontend
    -- * Back ends
  , runIO
  , compile
  , icompile
    {-
  , compileAndCheck'
  , compileAndCheck
  , runCompiled'
  , runCompiled
  , captureCompiled'
  , captureCompiled
  , compareCompiled'
  , compareCompiled
-}
  ) where

import Feldspar
import Feldspar.Run.Representation
import Feldspar.Run.Compile
import Feldspar.Run.Frontend

