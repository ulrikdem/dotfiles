\begin{code}

module IconRules where

import Data.List
import XMonad.ManageHook

iconRules =
    [ (return True, "\xfaae")
    , (className =? "Luakit", "\xf484")
    , (className =? "Termite", "\xe7a2")
    , (className =? "Termite" <&&> fmap (" - nvim" `isSuffixOf`) title, "\xe62b")
    ]

\end{code}
