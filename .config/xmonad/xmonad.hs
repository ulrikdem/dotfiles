-- Imports {{{1

-- vim: foldmethod=marker

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import Control.Monad

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

import Graphics.X11.Xft

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.TagWindows

import XMonad.Hooks.DynamicBars
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Decoration
import XMonad.Layout.GridVariants hiding (Orientation(..))
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.StateFull
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Stack

import ConfigOverride
import IconQuery

-- Main {{{1

main = do
    textHeight <- getTextHeight
    xmonad $ overrideConfig $ withUrgencyHook NoUrgencyHook $ setEwmhActivateHook doAskUrgent $ ewmh $ docks $ def
        { startupHook = barStartupHook textHeight <> setDefaultCursor xC_left_ptr
        , handleEventHook = barEventHook textHeight <> workspaceEventHook
        , logHook = do
            barLogHook
            let tagFloating set win = tagIff (win `M.member` W.floating set) "floating" win
            withWindowSet $ mapM_ <$> tagFloating <*> W.allWindows
        , manageHook = composeAll
            [ appName =? "xmonad-scratchpad" --> ask >>= liftX . addTag "scratchpad" >> manageCustomFloat textHeight
            , appName =? "xmonad-custom-float" --> manageCustomFloat textHeight
            , placeHook $ fixed (0.5, 0.5)
            , appName =? "xmonad-float" --> doFloat
            ]
        , layoutHook = lessBorders Screen $ resetEmpty
            $ named "tiled" (avoidStruts $ layout textHeight)
            ||| named "full" (avoidStruts StateFull)
            ||| named "fullscreen" StateFull
        , borderWidth = 2
        , normalBorderColor = "black"
        , focusedBorderColor = "gray50"
        , terminal = terminalName
        , modMask = modm
        } `removeKeysP` removedKeys `additionalKeysP` extraKeys textHeight `additionalMouseBindings` extraMouseBindings

terminalName = "alacritty"

tagIff = bool delTag addTag

-- Theme {{{1

theme = def
    { inactiveColor = "black"
    , inactiveBorderWidth = 0
    , fontName = "xft:monospace-" ++ show fontSize
    }

fontSize = 9

getTextHeight = do
    display <- openDisplay ""
    font <- xftFontOpen display (defaultScreenOfDisplay display) $ drop 4 $ fontName theme
    height <- xftfont_height font
    xftFontClose display font
    closeDisplay display
    return $ fi height

-- Bar {{{1

barStartupHook textHeight = dynStatusBarStartup (spawnBar textHeight) $ return ()
barEventHook textHeight = dynStatusBarEventHook (spawnBar textHeight) $ return ()

spawnBar textHeight (S i) = spawnPipe $ "xmobar -p 'TopH " ++ show (barHeight textHeight) ++ "' -x " ++ show i
    ++ " -f 'Monospace " ++ show fontSize ++ "' -N 'Symbols Nerd Font " ++ show (fontSize + 2) ++ "' -D \"$(xrdb -get Xft.dpi)\""

barHeight textHeight = h + h `mod` 2 - 1 where
    h = textHeight * 3 `div` 2

barLogHook = do
    let getIcon w win = xmobarAction ("xdotool set_desktop_viewport \n " ++ w ++ " windowactivate " ++ show win) "1"
            <$> runQuery iconQuery win
        getIcons w = fmap ((W.tag w,) . concat) . onFocusedZ (xmobarColor "gray50" "" . xmobarFont 1)
            <$> mapZM_ (getIcon $ W.tag w) (W.stack w)
    icons <- withWindowSet $ fmap (M.fromList . catMaybes) . mapM getIcons . W.workspaces
    let rename w _ = xmobarAction ("xdotool set_desktop_viewport \n " ++ w) "1"
            $ xmobarAction ("xdotool getactivewindow set_desktop_for_window " ++ show (read w - 1)) "3"
            $ pad $ (w ++) $ xmobarColor "gray25" "" $ M.findWithDefault "" w icons
        getScreen = do
            S i <- withWindowSet $ return . W.screen . W.current
            return $ Just $ show i
        showTag tag = do
            hasTag' <- withWindowSet $ mapM (hasTag tag) . W.peek
            return $ Just $ if hasTag' == Just True then " <fc=gray25>[" ++ tag ++ "]</fc>" else ""
        pp color = filterOutWsPP [scratchpadWorkspaceTag] def
            { ppCurrent = xmobarBorder "Top" color 2
            , ppVisible = xmobarBorder "Top" "#404040" 2 -- gray25
            , ppUrgent = xmobarBorder "Top" "#cd8500" 2 -- orange3
            , ppHiddenNoWindows = id
            , ppRename = rename
            , ppTitle = xmobarRaw
            , ppTitleSanitize = id
            , ppExtras = [getScreen, showTag "scratchpad"]
            , ppOrder = \(workspaces : layout : title : screen : tags)
                -> [intercalate screen $ lines workspaces, title ++ concat tags]
            , ppSep = "<fc=gray25>│</fc> "
            , ppWsSep = ""
            }
    multiPP (pp "#008b00" {- green4 -}) (pp "#808080" {- gray50 -})

workspaceEventHook event@ClientMessageEvent {ev_data = screen : workspace : _} = do
    atom <- getAtom "_NET_DESKTOP_VIEWPORT"
    when (ev_message_type event == atom) $ screenWorkspace (fi screen)
        >>= flip whenJust (\w -> windows $ W.greedyView (show workspace) . W.view w)
    mempty
workspaceEventHook _ = mempty

manageCustomFloat textHeight = do
    height <- liftX $ withWindowSet $ return . rect_height . screenRect . W.screenDetail . W.current
    let y = fi (barHeight textHeight) / fi height
    customFloating $ W.RationalRect (2 / 3) y (1 / 3) (1 - y)

-- Keys {{{1

modm = mod4Mask

removedKeys = ["M-?", "M-S-/", "M-S-<Tab>"]

extraKeys textHeight =
    [ ("M-<Return>", promote)
    , ("M-h", windows W.focusDown)
    , ("M-S-h", windows W.swapDown)
    , ("M-C-h", rotAllDown)
    , ("M-C-j", rotAllDown)
    , ("M-C-k", rotAllUp)

    , ("M-<Up>", sendMessage $ Go U)
    , ("M-<Down>", sendMessage $ Go D)
    , ("M-<Left>", sendMessage $ Go L)
    , ("M-<Right>", sendMessage $ Go R)
    , ("M-S-<Up>", sendMessage $ Swap U)
    , ("M-S-<Down>", sendMessage $ Swap D)
    , ("M-S-<Left>", sendMessage $ Apply (windows . W.modify' . moveLeft) L)
    , ("M-S-<Right>", sendMessage $ Apply (windows . W.modify' . moveRight) R)

    , ("M-S-m", placeFocused $ fixed (0.5, 0.5))

    , ("M-s", allNamedScratchpadAction
        [ NS "" (terminalName ++ " --class Alacritty,xmonad-scratchpad") (liftX . hasTag "scratchpad" =<< ask) idHook
        ] "")
    , ("M-S-s", toggleTag "scratchpad")

    , ("M-C-<Left>", sendMessage $ IncMasterCols (-1))
    , ("M-C-<Right>", sendMessage $ IncMasterCols 1)
    , ("M-C-<Up>", sendMessage $ ModifyLimit pred)
    , ("M-C-<Down>", sendMessage $ ModifyLimit succ)
    , ("M-a", withWindowSet $ flip whenJust (sendMessage . ModifyLimit . const . length) . W.stack . W.workspace . W.current)

    , ("M-<Space>", sendMessage $ JumpToLayout "tiled")
    , ("M-f", sendMessage $ JumpToLayout "full")
    , ("M-S-f", sendMessage $ JumpToLayout "fullscreen")

    , ("M-g", windowPrompt (windowPromptConfig textHeight) Goto allWindows)
    , ("M-S-g", windowPrompt (windowPromptConfig textHeight) Bring allWindows)
    , ("M-p", commandPrompt textHeight Shell spawn)
    , ("M-S-p", commandPrompt textHeight Terminal $ runInTerm "")

    , ("M-b", spawn "firefox")
    , ("M-S-t", spawn "thunderbird")
    , ("M-v", spawn "mpv --player-operation-mode=pseudo-gui")
    , ("M-u", spawn "unicode-input")
    , ("M-l", spawn "lock")
    , ("M-S-l", spawn "systemctl suspend")

    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")
    , ("<XF86MonBrightnessDown>", spawn "light -U 10")
    , ("<XF86MonBrightnessUp>", spawn "light -A 10")

    , ("M-S-u", focusUrgent)
    , ("M-C-u", clearUrgents)

    , ("M-<Tab>", toggleWS' [scratchpadWorkspaceTag])
    , ("M-S-,", moveTo Prev cycleWSType)
    , ("M-S-.", moveTo Next cycleWSType)
    ] ++
    [ ("M-C-" ++ [key], windows $ swapWithCurrent workspace)
    | (key, workspace) <- zip ['1'..] $ workspaces def
    ] ++

    [ (mod ++ [key], action index)
    | (mod, action) <-
        [ ("M-", viewScreen def)
        , ("M-S-", sendToScreen def)
        , ("M-C-", getScreen def >=> flip whenJust (screenWorkspace >=> flip whenJust (windows . W.greedyView)))
        ]
    , (keys, index) <- zip ["wn", "e", "ri"] [0..]
    , key <- keys
    ]

extraMouseBindings =
    [ ((modm, button3), \w -> focus w >> mouseResizeEdgeWindow (1 / 3) w >> windows W.shiftMaster)
    ]

moveLeft win stack = stack {W.up = b, W.down = reverse a ++ W.down stack} where
    (a, b) = splitAt (succ $ fromJust $ elemIndex win $ W.up stack) $ W.up stack
moveRight win = reverseS . moveLeft win . reverseS

toggleTag tag = withFocused $ \win -> do
    hasTag' <- hasTag tag win
    tagIff (not hasTag') tag win
    barLogHook

weightFactor = 1.26

cycleWSType = hiddenWS :&: ignoringWSs [scratchpadWorkspaceTag]

-- Prompt {{{1

windowPromptConfig textHeight = def
    { promptBorderWidth = 0
    , height = textHeight
    , font = fontName theme
    , maxComplColumns = Just 1
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    , historySize = 0
    , promptKeymap = M.union (M.fromList
        [ ((controlMask, xK_u), killBefore)
        , ((controlMask, xK_BackSpace), killWord' (\c -> isSpace c || c == '/') Prev)
        , ((controlMask, xK_Left), moveCursor Prev >> moveWord Prev)
        , ((controlMask, xK_Right), moveWord Next >> moveCursor Next)
        ]) emacsLikeXPKeymap
    }

commandPromptConfig textHeight matches = def
    { promptBorderWidth = 0
    , height = textHeight
    , font = fontName theme
    , historyFilter = deleteAllDuplicates
    , promptKeymap = M.union (M.fromList
        [ ((0, xK_Up), historyUpMatching matches)
        , ((0, xK_Down), historyDownMatching matches)
        ]) $ promptKeymap $ windowPromptConfig textHeight
    }

commandPrompt textHeight prompt action = do
    matches <- initMatches
    let config = commandPromptConfig textHeight matches
    cmds <- io getCommands
    mkXPrompt prompt config (getShellCompl' CaseInSensitive cmds $ searchPredicate config) action

data Terminal = Terminal

instance XPrompt Terminal where
    showXPrompt _ = "Run in terminal: "
    completionToCommand _ = completionToCommand Shell

-- Layout {{{1

layout textHeight = spacing $ navigation $ focusTracking $ limit grid where
    spacing = smartSpacingWithEdge gapWidth
    navigation = configurableNavigation noNavigateBorders
    limit = ModifiedLayout $ SlidingLimit 0 3
    grid = TallGrid 1000000000 2 1 1 0 :: TallGrid Window
    gapWidth = round $ fi textHeight / 4

data SlidingLimit a = SlidingLimit Int Int
    deriving (Read, Show)

instance LayoutModifier SlidingLimit a where
    modifyLayoutWithUpdate (SlidingLimit start limit) workspace rect = case W.stack workspace of
        Just stack@(W.Stack focus up down) -> do
            let i = length up
                start' = max 0 $ min (length stack - limit) $ max (i - limit + 1) $ min i start
                stack' = W.Stack focus (take (i - start') up) (take (start' + limit - 1 - i) down)
            result <- runLayout workspace {W.stack = Just stack'} rect
            return (result, Just $ SlidingLimit start' limit)
        Nothing -> (, Nothing) <$> runLayout workspace rect
    pureMess (SlidingLimit start limit) = fmap apply . fromMessage where
        apply (ModifyLimit f) = SlidingLimit start $ max 1 $ f limit

data ModifyLimit = ModifyLimit (Int -> Int)

instance Message ModifyLimit

resetEmpty layout = ResetEmpty layout layout

data ResetEmpty l a = ResetEmpty (l a) (l a)
    deriving (Read, Show)

instance (LayoutClass l a) => LayoutClass (ResetEmpty l) a where
    runLayout (W.Workspace tag (ResetEmpty reset _) Nothing) rect = do
        (rects, layout') <- runLayout (W.Workspace tag reset Nothing) rect
        return (rects, Just $ ResetEmpty reset $ fromMaybe reset layout')
    runLayout (W.Workspace tag (ResetEmpty reset layout) stack) rect = do
        (rects, layout') <- runLayout (W.Workspace tag layout stack) rect
        return (rects, fmap (ResetEmpty reset) layout')
    handleMessage (ResetEmpty reset layout) message = fmap (ResetEmpty reset) <$> handleMessage layout message
    description (ResetEmpty _ layout) = description layout
