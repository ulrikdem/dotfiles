\begin{code}

module IconQuery (iconQuery) where

import Data.List
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers hiding ((^?), ($?))

iconQuery = composeOne
    [ className =? "Alacritty" <&&> title $? " - nvim" -?> return "\xe62b" -- 
    , className =? "Alacritty" -?> return "\xf489" -- 
    , className =? "Evince" <||> className =? "Zathura" -?> return "\xf4a5" -- 
    , className =? "firefox" -?> return "\xf269" -- 
    , className =? "Gimp" -?> return "\xf1fc" -- 
    , className =? "Luakit" -?> return "\xf484" -- 
    , className =? "mpv" -?> return "\xf144" -- 
    , className =? "Sxiv" -?> return "\xf7e8" -- 
    , className $? ".exe" -?> return "\xf000" -- 
    , appName =? "libreoffice" -?> return "\xf713" -- 
    , return $ Just "\xfaae" -- 类
    ]

q ^? s = fmap (s `isPrefixOf`) q
q $? s = fmap (s `isSuffixOf`) q

\end{code}
