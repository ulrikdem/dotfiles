\begin{code}

module IconQuery (iconQuery) where

import Data.List
import Data.Monoid
import XMonad.ManageHook

iconQuery = getFirst <$> composeAll
    [ className =? "Luakit" --> icon "\xf484" -- 
    , className =? "Termite" <&&> title $? " - nvim" --> icon "\xe62b" -- 
    , className =? "Termite" --> icon "\xe7a2" -- 
    ]

icon = return . First . Just

q ^? s = fmap (s `isPrefixOf`) q
q $? s = fmap (s `isSuffixOf`) q

\end{code}
