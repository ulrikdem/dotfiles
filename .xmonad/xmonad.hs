import qualified Data.Map.Strict as M
import Graphics.X11.ExtraTypes.XF86
import XMonad hiding ((|||))
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Promote
import XMonad.Actions.SwapWorkspaces
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.StateFull
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.CustomKeys
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad

main = xmonad . ewmh . docks $ def
    { startupHook = composeAll
        [ setDefaultCursor xC_left_ptr
        , dynStatusBarStartup bar $ return ()
        ]
    , handleEventHook = dynStatusBarEventHook bar $ return ()
    , logHook = multiPP
        pp {ppCurrent = xmobarColor "green" ""}
        pp {ppCurrent = xmobarColor "yellow" ""}
    , normalBorderColor = "black"
    , focusedBorderColor = "gray"
    , layoutHook = layout
    , modMask = mod4Mask
    , keys = customKeys delkeys inskeys
    , terminal = "termite"
    , manageHook = let r = W.RationalRect 0.25 0.25 0.5 0.5 in composeAll
        [ scratchpadManageHook r
        , appName =? "download-prompt" --> customFloating r
        , placeHook $ fixed (0.5, 0.5)
        , className =? "Gxmessage" --> doFloat
        ]
    }

bar (S i) = spawnPipe $ "xmobar -x " ++ show i

pp = namedScratchpadFilterOutWorkspacePP def
    { ppVisible = id
    , ppVisibleNoWindows = Just $ xmobarColor "gray32" ""
    , ppHiddenNoWindows = xmobarColor "gray32" ""
    , ppTitle = id
    , ppOrder = \[w, l, t] -> [w, t]
    , ppSep = " <fc=gray32>│</fc> "
    }

layout = avoidStruts (tall ||| Mirror tall ||| full) ||| renamed [PrependWords "NoBar"] full where
    tall = let b = Border 6 6 6 6 in spacingRaw False b True b True $ Tall 1 0.1 0.5
    full = noBorders StateFull

delkeys XConfig {modMask = mod} =
    [ (mod, xK_question)
    , (mod .|. shiftMask, xK_slash)
    ]

inskeys XConfig {modMask = mod, terminal = term} =
    [ ((mod, xK_Return), promote)
    , ((mod, xK_space), sendMessage $ JumpToLayout "Spacing Tall")
    , ((mod .|. shiftMask, xK_m), sendMessage $ JumpToLayout "Mirror Spacing Tall")
    , ((mod, xK_f), sendMessage $ JumpToLayout "StateFull")
    , ((mod .|. shiftMask, xK_f), sendMessage $ JumpToLayout "NoBar StateFull")
    , ((mod, xK_c), placeFocused $ fixed (0.5, 0.5))
    , ((mod, xK_p), shellPrompt . promptconf "" =<< initMatches)
    , ((mod .|. shiftMask, xK_p), shellPrompt . promptconf (term ++ " -e ") =<< initMatches)
    , ((mod, xK_s), scratchpadSpawnActionCustom $ term ++ " --name scratchpad")
    , ((mod, xK_b), spawn "luakit")
    , ((mod, xK_z), spawn "lock")
    , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+")
    , ((0, xF86XK_MonBrightnessDown), spawn "light -U 25")
    , ((0, xF86XK_MonBrightnessUp), spawn "light -A 25")
    ] ++
    [ ((mod .|. shiftMask, k), moveTo d . WSIs . return $ (/= "NSP") . W.tag)
    | (k, d) <- [(xK_h, Prev), (xK_l, Next)]
    ] ++
    [ ((mod .|. controlMask, k), windows $ swapWithCurrent w)
    | (k, w) <- zip [xK_1..] $ workspaces def
    ] ++
    [ ((mod .|. m, k), f def i)
    | (m, f) <- [(0, viewScreen), (shiftMask, sendToScreen)]
    , (k, i) <- zip [xK_w, xK_e, xK_r] [0..]
    ]

promptconf text matches = def
    { promptBorderWidth = 0
    , font = "xft:monospace:pixelsize=14"
    , defaultText = text
    , historyFilter = deleteAllDuplicates
    , promptKeymap = M.union (M.fromList
        [ ((controlMask, xK_p), historyUpMatching matches)
        , ((controlMask, xK_n), historyDownMatching matches)
        , ((controlMask, xK_b), moveCursor Prev)
        , ((controlMask, xK_f), moveCursor Next)
        , ((mod1Mask, xK_b), moveWord Prev)
        , ((mod1Mask, xK_f), moveWord Next)
        ]) defaultXPKeymap
    }
