\begin{code}

module LocalConfig (overrideConfig, leaderMap, iconQuery) where

import Data.List
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

overrideConfig = id

leaderMap terminal =
    [ ("f", spawn "firefox")
    , ("h", spawn $ terminal ++ " -e htop")
    , ("m", spawn "mpv --player-operation-mode=pseudo-gui")
    , ("n", spawn $ terminal ++ " -e newsboat")
    , ("p", spawn $ terminal ++ " -e ipython")
    , ("t", spawn "thunderbird")
    , ("v", spawn $ terminal ++ " -e nvim")
    ]

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
