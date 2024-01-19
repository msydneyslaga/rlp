{-# LANGUAGE LambdaCase #-}
module Rlp2Core
    ( rlp2core
    )
    where
--------------------------------------------------------------------------------
import Core.Syntax                  as Core
import Rlp.Syntax                   as Rlp
import Data.Foldable
import Data.HashMap.Strict          qualified as H
import Control.Monad.State
import Lens.Micro.Platform
--------------------------------------------------------------------------------

rlp2core :: RlpProgram' -> Program'
rlp2core (RlpProgram ds) = execState (decl2core `traverse_` ds) init
    where
        init = Program
            { _programScDefs    = mempty
            , _programTypeSigs  = mempty
            }

type GenCoreProg b = State (Program b)

type GenCoreProg' = GenCoreProg Name

emitTypeSig :: Name -> Type -> GenCoreProg' ()
emitTypeSig b t = do
    let tl :: Lens' Program' (Maybe Type)
        tl = programTypeSigs . at b
    tl <~ (use tl >>= \case
        -- TODO: non-fatal error
        Just o  -> error "(TODO: non-fatal) duplicate type sigs"
        Nothing -> pure (Just t)
        )

decl2core :: Decl' RlpExpr -> GenCoreProg' ()

decl2core (DataD n as cs) = undefined

decl2core (TySigD vs t) = mkSig `traverse_` vs where
    mkSig :: VarId -> GenCoreProg' ()
    mkSig (NameVar n) = emitTypeSig n t

