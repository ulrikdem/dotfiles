\begin{code}

module IconQuery (iconQuery) where

import Data.List
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

iconQuery = composeOne
    [ className =? "Alacritty" <&&> title $? " - nvim" -?> return "\xe62b" -- 
    , className =? "Alacritty" -?> return "\xe795" -- 
    , className =? "Evince" <||> className =? "Zathura" -?> return "\xf0219" -- 󰈙
    , className =? "firefox" -?> return "\xf269" -- 
    , className =? "Gimp" -?> return "\xf1fc" -- 
    , className =? "mpv" -?> return "\xf144" -- 
    , className =? "Nm-connection-editor" -?> return "\xf06f3" -- 󰛳
    , className =? "Sxiv" -?> return "\xf02e9" -- 󰋩
    , className =? "thunderbird" -?> return "\xf01f0" -- 󰇰
    , className =? "webview" <&&> title $? "JupyterLab" -?> return "\xf0320" -- 󰌠
    , className $? ".exe" -?> return "\xf000" -- 
    , appName =? "libreoffice" -?> return "\xf0214" -- 󰈔
    , return $ Just "\xf05af" -- 󰖯
    ]

\end{code}
