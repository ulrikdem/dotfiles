\begin{code}

module IconRules (iconRules) where

import Data.List
import XMonad.ManageHook

iconRules =
    [ (return True, "\xfaae") -- 类
    , (className =? "Luakit", "\xf484") -- 
    , (className =? "Termite", "\xe7a2") -- 
    , (className =? "Termite" <&&> title $? " - nvim", "\xe62b") -- 
    ]

q ^? s = fmap (s `isPrefixOf`) q
q $? s = fmap (s `isSuffixOf`) q

\end{code}
