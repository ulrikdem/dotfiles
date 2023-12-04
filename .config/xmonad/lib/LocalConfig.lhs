\begin{code}

module LocalConfig (overrideConfig, leaderMap, iconQuery) where

import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

overrideConfig = id

leaderMap =
    [ ("f", "firefox", False)
    , ("S-f", "firefox --private-window", False)
    , ("h", "htop", True)
    , ("i", "ipython", True)
    , ("m", "mpv --player-operation-mode=pseudo-gui", False)
    , ("n", "newsboat", True)
    , ("p", "alacritty -o window.dimensions.columns=120 -o window.dimensions.lines=15 --class Alacritty,xmonad-float -T pulsemixer -e pulsemixer", False)
    , ("t", "thunderbird", False)
    , ("v", "nvim", True)
    ]

iconQuery = composeOne
    [ className =? "Alacritty" -?> composeOne
        [ title =? "htop" -?> return "\xf0128" -- 󰄨
        , title =? "ipython" -?> return "\xf0320" -- 󰌠
        , title =? "newsboat" -?> return "\xf09e" -- 
        , title =? "pulsemixer" -?> return "\xf057e" -- 󰕾
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
