module Control.DFA
    ( DFA(..)
    , evalDFA
    )
    where
----------------------------------------------------------------------------------
import Data.Maybe       (isJust, catMaybes)
----------------------------------------------------------------------------------

newtype DFA s = DFA { stepDFA :: s -> Maybe s }

evalDFA :: DFA s -> s -> [s]
evalDFA dfa s = catMaybes $ iterateM (stepDFA dfa) s
    where
        iterateM :: (Monad m) => (a -> m a) -> a -> [m a]
        iterateM k z = iterate (>>=k) (pure z)

