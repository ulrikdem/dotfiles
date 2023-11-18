\begin{code}

module LocalConfig (overrideConfig, leaderMap, iconQuery) where

import Data.List
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

overrideConfig = id

leaderMap spawnInTerminal =
    [ ("f", spawn "firefox")
    , ("S-f", spawn "firefox --private-window")
    , ("h", spawnInTerminal "htop")
    , ("m", spawn "mpv --player-operation-mode=pseudo-gui")
    , ("n", spawnInTerminal "newsboat")
    , ("p", spawnInTerminal "ipython")
    , ("t", spawn "thunderbird")
    , ("v", spawnInTerminal "nvim")
    ]

iconQuery = composeOne
    [ className =? "Alacritty" -?> composeOne
        [ title =? "htop" -?> return "\xf0128" -- 󰄨
        , title =? "ipython" -?> return "\xf0320" -- 󰌠
        , title =? "newsboat" -?> return "\xf09e" -- 
        , title ^? "ranger:" -?> return "\xe5fe" -- 
        , title $? " - nvim" -?> return "\xe62b" -- 
        , return $ Just "\xe795" -- 
        ]
    , className =? "Evince" <||> className =? "Zathura" -?> return "\xf0219" -- 󰈙
    , className =? "firefox" -?> return "\xf269" -- 
    , className =? "Gimp" -?> return "\xf1fc" -- 
    , className =? "mpv" -?> return "\xf144" -- 
    , className =? "Nm-connection-editor" -?> return "\xf06f3" -- 󰛳
    , className =? "Sxiv" -?> return "\xf02e9" -- 󰋩
    , className =? "thunderbird" -?> return "\xf01ee" -- 󰇮
    , className =? "webview" <&&> title $? "JupyterLab" -?> return "\xf0320" -- 󰌠
    , className $? ".exe" -?> return "\xf000" -- 
    , appName =? "libreoffice" -?> return "\xf0214" -- 󰈔
    , return $ Just "\xf05af" -- 󰖯
    ]

\end{code}
