\begin{code}

module IconQuery (iconQuery) where

import Data.List
import Data.Monoid
import XMonad.ManageHook

iconQuery = getLast <$> composeAll
    [ icon "\xfaae" -- 类
    , className =? "Luakit" --> icon "\xf484" -- 
    , className =? "Termite" --> icon "\xe7a2" -- 
    , className =? "Termite" <&&> title $? " - nvim" --> icon "\xe62b" -- 
    ]

icon = return . Last . Just

q ^? s = fmap (s `isPrefixOf`) q
q $? s = fmap (s `isSuffixOf`) q

\end{code}
