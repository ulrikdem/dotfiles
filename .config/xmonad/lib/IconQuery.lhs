\begin{code}

module IconQuery (iconQuery) where

import Data.List
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

iconQuery = composeOne
    [ className =? "Alacritty" <&&> title $? " - nvim" -?> return "\xe62b" -- 
    , className =? "Alacritty" -?> return "\xf489" -- 
    , className =? "Evince" <||> className =? "Zathura" -?> return "\xf4a5" -- 
    , className =? "firefox" -?> return "\xf269" -- 
    , className =? "Gimp" -?> return "\xf1fc" -- 
    , className =? "mpv" -?> return "\xf144" -- 
    , className =? "Sxiv" -?> return "\xf7e8" -- 
    , className =? "thunderbird" -?> return "\xf6ef" -- 
    , className $? ".exe" -?> return "\xf000" -- 
    , appName =? "libreoffice" -?> return "\xf713" -- 
    , return $ Just "\xfaae" -- 类
    ]

\end{code}
