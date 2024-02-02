module Rlp2Core
    ( rlpProgToCore
    )
    where
--------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.Writer.CPS
import Lens.Micro
import Lens.Micro.Internal
import Data.Text                        (Text)
import Data.Text                        qualified as T
import Data.HashMap.Strict              qualified as H

import Core.Syntax                      as Core
import Rlp.Syntax                       as Rlp
import Rlp.Parse.Types                  (RlpcPs)
--------------------------------------------------------------------------------

rlpProgToCore :: RlpProgram RlpcPs -> Program'
rlpProgToCore = foldMapOf (progDecls . each) declToCore

declToCore :: Decl' RlpcPs -> Program'

declToCore = undefined

-- declToCore (TySigD ns t) =
--     mempty & programTypeSigs .~ H.fromList [ (n, typeToCore t) | n <- ns ]

typeToCore :: RlpType RlpcPs -> Type
typeToCore = undefined
-- typeToCore FunConT = TyFun
-- typeToCore (FunT s t) = typeToCore s :-> typeToCore t
-- typeToCore (AppT s t) = TyApp (typeToCore s) (typeToCore t)
-- typeToCore (ConT n) = TyCon (dsNameToName n)
-- typeToCore (VarT x) = TyVar (dsNameToName x)

-- | Forwards-compatiblity if IdP RlpDs is changed
dsNameToName :: IdP RlpcPs -> Name
dsNameToName = id

