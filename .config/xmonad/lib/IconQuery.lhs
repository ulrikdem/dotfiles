\begin{code}

module IconQuery (iconQuery) where

import Data.List
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers hiding ((^?), ($?))

iconQuery = composeOne
    [ className =? "Alacritty" <&&> title $? " - nvim" -?> return "\xe62b" -- 
    , className =? "Alacritty" -?> return "\xf489" -- 
    , className =? "Luakit" -?> return "\xf484" -- 
    , return $ Just "\xfaae" -- 类
    ]

q ^? s = fmap (s `isPrefixOf`) q
q $? s = fmap (s `isSuffixOf`) q

\end{code}
