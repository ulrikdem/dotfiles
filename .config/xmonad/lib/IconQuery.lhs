\begin{code}

module IconQuery (iconQuery) where

import Data.List
import Data.Monoid
import XMonad.ManageHook

iconQuery = getFirst <$> composeAll
    [ className =? "Alacritty" <&&> title $? " - nvim" --> icon "\xe62b" -- 
    , className =? "Alacritty" --> icon "\xf489" -- 
    , className =? "Luakit" --> icon "\xf484" -- 
    ]

icon = return . First . Just

q ^? s = fmap (s `isPrefixOf`) q
q $? s = fmap (s `isSuffixOf`) q

\end{code}
