\begin{code}

module LocalConfig (overrideConfig, leaderMap, iconQuery) where

import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

overrideConfig = id

leaderMap =
    [ ("c", "nm-rofi", False)
    , ("S-c", "nm-connection-editor", False)
    , ("f", "firefox", False)
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
        , title =? "ipython" <||> title =? "ipy" -?> return "\xf0320" -- 󰌠
        , title =? "newsboat" -?> return "\xf046b" -- 󰑫
        , title =? "pulsemixer" -?> return "\xf057e" -- 󰕾
        , title ^? "ranger:" -?> return "\xf07c" -- 
        , title $? " - nvim" -?> return "\xf36f" -- 
        , return $ Just "\xe795" -- 
        ]
    , className =? "Evince" <||> className =? "Zathura" -?> return "\xf0219" -- 󰈙
    , className =? "firefox" -?> return "\xf0239" -- 󰈹
    , className =? "Gimp" -?> return "\xf1fc" -- 
    , className ^? "libreoffice" <||> className =? "Soffice" -?> return "\xf0214" -- 󰈔
    , className =? "mpv" -?> return "\xf144" -- 
    , className =? "Nm-connection-editor" -?> return "\xf06f3" -- 󰛳
    , className =? "Sxiv" -?> return "\xf03e" -- 
    , className =? "thunderbird" -?> return "\xf01ee" -- 󰇮
    , className =? "webview" <&&> title $? "JupyterLab" -?> return "\xf0320" -- 󰌠
    , className $? ".exe" -?> return "\xedae" -- 
    , return $ Just "\xf05af" -- 󰖯
    ]

\end{code}
