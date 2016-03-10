module Haste.Aplite.Compile where
import Feldspar.Run.Compile
import Haste.Aplite.Export
import Language.JS.Monad

compile :: Export a => CodeTuning -> a -> JSString
compile ct f =
    wrapped ct $ f' {funParams = params}
  where
    Fun startid params prog = mkFun 0 [] f
    f' = generate startid prog

generate :: (ReturnValue a, Interp instr JSGen, HFunctor instr)
        => Int -> Program instr a -> Func
generate startid = runJSGen startid . interpret . lowerTop . liftRun
