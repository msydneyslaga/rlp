{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module GM.Visual
    ( renderGmState
    )
    where
--------------------------------------------------------------------------------
import Text.Printf
import Data.Function            ((&), on)
import Text.PrettyPrint         qualified as P

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import GM.Types
import GM.Print
--------------------------------------------------------------------------------

renderGmState :: GmState -> IO ()
renderGmState st = renderCairo path size (drawState st)
    where
        size = mkSizeSpec2D (Just 1000) (Just 1000)
        path = printf "/tmp/render/%04d.png" n
        n = st ^. gmStats . stsReductions

drawState :: GmState -> Diagram B
drawState = drawStack

drawStack :: GmState -> Diagram B
drawStack st = st & vcatOf (gmStack . each . to cell)
    where
        cell a = rect 10 5
              <> text (printf "%04x: %s" a (P.render . showNodeAt st $ a))

vcatOf :: (InSpace V2 n a, Floating n, Juxtaposable a, HasOrigin a, Monoid' a)
       => Getting (Endo [a]) s a -> s -> a
vcatOf l = vcat . (^.. l)

newtype Vap a = Vap { getVap :: a }

instance (InSpace V2 n a, Juxtaposable a, Semigroup a)
         => Semigroup (Vap a) where (<>) = (Vap .) . ((===) `on` getVap)
instance (InSpace V2 n a, Juxtaposable a, Monoid a)
         => Monoid (Vap a) where mempty = Vap mempty

newtype Hap a = Hap { getHap :: a }

instance (InSpace V2 n a, Juxtaposable a, Semigroup a)
         => Semigroup (Hap a) where (<>) = (Hap .) . ((|||) `on` getHap)
instance (InSpace V2 n a, Juxtaposable a, Monoid a)
         => Monoid (Hap a) where mempty = Hap mempty

