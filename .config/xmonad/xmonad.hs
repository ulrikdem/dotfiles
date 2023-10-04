-- Imports {{{1

-- vim: foldmethod=marker

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import Control.Monad

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

import Graphics.X11.Xft

import System.Exit

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

import XMonad.Layout.BorderResize
import XMonad.Layout.Decoration
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.StateFull
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
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

main = xmonad . conf =<< getTextHeight

conf textHeight = overrideConfig $ withUrgencyHook NoUrgencyHook $ setEwmhActivateHook doAskUrgent $ ewmh $ docks def
    { startupHook = barStartupHook textHeight <> setDefaultCursor xC_left_ptr
        <> checkKeymap (conf textHeight) (keymap textHeight)
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
    , layoutHook = layout textHeight
    , borderWidth = 2
    , normalBorderColor = inactiveColor theme
    , focusedBorderColor = activeColor theme
    , terminal = "alacritty"
    , modMask = mod4Mask
    , keys = flip mkKeymap $ keymap textHeight
    , mouseBindings = mouse
    }

tagIff = bool delTag addTag

-- Theme {{{1

theme = def
    { activeColor = "gray50"
    , activeTextColor = "black"
    , activeBorderWidth = 0
    , inactiveColor = "black"
    , inactiveTextColor = "gray50"
    , inactiveBorderWidth = 0
    , urgentColor = "black"
    , urgentTextColor = "orange3"
    , urgentBorderWidth = 0
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
            . xmobarAction ("xdotool set_desktop_for_window " ++ show win ++ " $(xdotool get_desktop)") "3"
            <$> runQuery iconQuery win
        getIcons w = fmap ((W.tag w,) . concat) . onFocusedZ (xmobarColor "gray50" "" . xmobarFont 1)
            <$> mapZM_ (getIcon $ W.tag w) (W.stack w)
    icons <- withWindowSet $ fmap (M.fromList . catMaybes) . mapM getIcons . W.workspaces
    let rename w _ = xmobarAction ("xdotool set_desktop_viewport \n " ++ w) "1"
            $ pad $ (w ++) $ xmobarColor "gray25" "" $ M.findWithDefault "" w icons
        getScreen = do
            S i <- gets $ W.screen . W.current . windowset
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

workspaceEventHook event@ClientMessageEvent{ev_data = screen : workspace : _} = do
    atom <- getAtom "_NET_DESKTOP_VIEWPORT"
    when (ev_message_type event == atom) $ screenWorkspace (fi screen)
        >>= flip whenJust (\w -> windows $ W.greedyView (show workspace) . W.view w)
    mempty
workspaceEventHook _ = mempty

manageCustomFloat textHeight = do
    height <- liftX $ gets $ rect_height . screenRect . W.screenDetail . W.current . windowset
    let y = fi (barHeight textHeight) / fi height
    customFloating $ W.RationalRect (2 / 3) y (1 / 3) (1 - y)

-- Keys {{{1

keymap textHeight = let XConfig{terminal = terminal, layoutHook = layout} = conf textHeight in
    [ ("M-q", asks directories >>= flip recompile False >>= flip when (restart "xmonad" True))
    , ("M-S-q", io exitSuccess)
    , ("M-x", kill)

    , ("M-S-m", promote)
    , ("M-m", windows W.focusMaster)
    , ("M-n", windows W.focusUp)
    , ("M-e", windows W.focusDown)
    , ("M-S-n", windows W.swapUp)
    , ("M-S-e", windows W.swapDown)
    , ("M-C-n", rotAllUp)
    , ("M-C-e", rotAllDown)

    , ("M-<Up>", sendMessage $ Go U)
    , ("M-<Down>", sendMessage $ Go D)
    , ("M-<Left>", sendMessage $ Go L)
    , ("M-<Right>", sendMessage $ Go R)
    , ("M-S-<Up>", sendMessage $ Swap U)
    , ("M-S-<Down>", sendMessage $ Swap D)
    , ("M-S-<Left>", sendMessage $ Apply (windows . W.modify' . moveLeft) L)
    , ("M-S-<Right>", sendMessage $ Apply (windows . W.modify' . moveRight) R)

    , ("M-c", placeFocused $ fixed (0.5, 0.5))
    , ("M-S-t", withFocused $ windows . W.sink)

    , ("M-s", allNamedScratchpadAction
        [ NS "" (terminal ++ " --class Alacritty,xmonad-scratchpad") (liftX . hasTag "scratchpad" =<< ask) idHook
        ] "")
    , ("M-S-s", toggleTag "scratchpad")

    , ("M--", modifyColumns (-))
    , ("M-=", modifyColumns (+))
    , ("M-C--", sendMessage $ ModifyLimit pred)
    , ("M-C-=", sendMessage $ ModifyLimit succ)
    , ("M-S-=", withWindowSet $ flip whenJust (sendMessage . ModifyLimit . const . length) . W.stack . W.workspace . W.current)
    , ("M-f", withFocused $ \w -> windows (W.sink w) >> sendMessage (ToggleFullscreen w))
    , ("M-<Backspace>", setLayout $ Layout layout)

    , ("M-g", windowPrompt (windowPromptConfig textHeight) Goto allWindows)
    , ("M-S-g", windowPrompt (windowPromptConfig textHeight) Bring allWindows)
    , ("M-r", commandPrompt textHeight Shell spawn)
    , ("M-S-r", commandPrompt textHeight Terminal $ runInTerm "")

    , ("M-<Return>", spawn terminal)
    , ("M-w", spawn "firefox")
    , ("M-t", spawn "thunderbird")
    , ("M-v", spawn "mpv --player-operation-mode=pseudo-gui")
    , ("M-i", spawn "unicode-input")
    , ("M-l", spawn "lock")
    , ("M-S-l", spawn "systemctl suspend")

    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")
    , ("<XF86MonBrightnessDown>", spawn "light -U 10")
    , ("<XF86MonBrightnessUp>", spawn "light -A 10")

    , ("M-u", focusUrgent)
    , ("M-S-u", clearUrgents)

    , ("M-<Space>", toggleWS' [scratchpadWorkspaceTag])
    , ("M-[", moveTo Prev cycleWSType)
    , ("M-]", moveTo Next cycleWSType)
    ] ++

    [ (mod ++ w, windows $ f w)
    | (mod, f) <- [("M-", W.greedyView), ("M-S-", W.shift), ("M-C-", swapWithCurrent)]
    , w <- workspaces $ conf textHeight
    ] ++

    [ (mod ++ [key], f i)
    | (mod, f) <- [("M-", viewScreen def), ("M-S-", sendToScreen def),
        ("M-C-", getScreen def >=> flip whenJust (screenWorkspace >=> flip whenJust (windows . W.greedyView)))]
    , (key, i) <- zip "h,." [0..]
    ]

mouse XConfig{modMask = mod} = M.fromList
    [ ((mod, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((mod, button3), \w -> focus w >> mouseResizeEdgeWindow (1 / 3) w >> windows W.shiftMaster)
    ]

moveLeft win stack = stack{W.up = b, W.down = reverse a ++ W.down stack} where
    (a, b) = splitAt (succ $ fromJust $ elemIndex win $ W.up stack) $ W.up stack
moveRight win = reverseS . moveLeft win . reverseS

modifyColumns op = do
    send <- gets $ flip sendMessageWithNoRefresh . W.workspace . W.current . windowset
    send $ WithColumns $ \c -> send $ ModifyLimit $ \l -> l `op` ceiling (fi l / fi c)
    sendMessage $ ModifyColumns (`op` 1)

toggleTag tag = withFocused $ \win -> do
    hasTag' <- hasTag tag win
    tagIff (not hasTag') tag win
    barLogHook

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

layout textHeight =
    lessBorders Screen $
    resetEmpty $
    ModifiedLayout (Fullscreen Nothing) $
    ModifiedLayout (Limit 3 (0, 0) $ tabbedBottom EllipsisShrinker theme{decoHeight = textHeight * 5 `div` 4}) $
    avoidStruts $
    smartSpacingWithEdge (fi textHeight `div` 4) $
    configurableNavigation noNavigateBorders $
    borderResize $
    (Grid [1, 1] (textHeight * 8) Nothing :: Grid Window)

data EllipsisShrinker = EllipsisShrinker
    deriving (Read, Show)

instance Shrinker EllipsisShrinker where
    shrinkIt _ s = s : map (++ "…") (reverse $ inits $ init s)

data LayoutMessage
    = ModifyColumns (Int -> Int)
    | WithColumns (Int -> X ())
    | ModifyLimit (Int -> Int)
    | ToggleFullscreen Window

instance Message LayoutMessage

data Grid a = Grid
    { colWeights :: [Rational]
    , minWidth :: Dimension
    , focused :: Maybe (Int, Rectangle)
    }
    deriving (Read, Show)

instance (Eq a) => LayoutClass Grid a where
    doLayout grid@Grid{colWeights = weights} rect stack@W.Stack{W.focus = focus} = return (rects, grid') where
        colWins = split (length weights) $ W.integrate stack
        rects = layout (mirrorRect rect) (zip colWins weights) >>= \(wins, r) -> layout (mirrorRect r) $ zip wins $ repeat 1
        grid' = Just grid{focused = (,) <$> findIndex (focus `elem`) colWins <*> fmap snd (find ((focus ==) . fst) rects)}
        split _ [] = []
        split cols wins = let (a, b) = splitAt (max 1 $ length wins `div` cols) wins in a : split (cols - 1) b
        layout rect as = layout' rect as $ sum $ snd <$> as
        layout' (Rectangle x y w h) ((a, weight) : as) total = let h' = round $ fi h * weight / total
            in (a, Rectangle x y w h') : layout' (Rectangle x (y + fi h') w (h - h')) as (total - weight)
        layout' _ [] _ = []
    emptyLayout grid _ = return ([], Just grid{focused = Nothing})
    handleMessage grid@Grid{colWeights = weights} m
        | Just (ModifyColumns f) <- fromMessage m = return $ Just grid{colWeights = replicate (max 1 $ f $ length weights) 1}
        | Just (WithColumns f) <- fromMessage m = f (length weights) >> return Nothing
        | Just (SetGeometry rect@(Rectangle x' _ w' _)) <- fromMessage m,
          Just (col, Rectangle x _ w _) <- focused grid,
          w' /= w && (x' == x || col > 0),
          (before, l : r : after) <- splitAt (if x' == x then col else col - 1) weights,
          let scale = weights !! col / fi w
              delta = (fi w' - fi w) * scale
              (l', r') = if x' == x then (l + delta, r - delta) else (l - delta, r + delta)
              min = fi (minWidth grid) * scale,
          l' >= min && r' >= min
            = return $ Just grid{colWeights = before ++ l' : r' : after, focused = Just (col, rect)}
        | otherwise = return Nothing
    description _ = "Grid"

data Limit l a = Limit Int (Int, Int) (l a)
    deriving (Read, Show)

instance (LayoutClass l a, Read (l a), Eq a) => LayoutModifier (Limit l) a where
    modifyLayoutWithUpdate (Limit n state extraLayout) (W.Workspace tag mainLayout stack) rect = do
        let focus = fromMaybe 0 $ length . W.up <$> stack
            (mainWins, extraWins) = splitAt (n - 1) $ W.integrate' stack
            state' = (length extraWins, if focus >= n - 1 then focus - n + 1
                                        else if length extraWins > fst state then 0
                                        else max 0 $ snd state + length extraWins - fst state)
            extraStack = toStack (snd state') extraWins
            extraFocus = fmap W.focus extraStack
            mainStack = toStack (min focus $ n - 1) $ mainWins ++ maybeToList extraFocus
        (rects, mainLayout') <- runLayout (W.Workspace tag mainLayout mainStack) rect
        (rects, extraLayout') <- case break ((== extraFocus) . Just . fst) rects of
            (before, (_, rect) : after) -> do
                (rects, extraLayout') <- runLayout (W.Workspace tag extraLayout extraStack) rect
                return (before ++ rects ++ after, extraLayout')
            (_, []) -> (rects,) . snd <$> runLayout (W.Workspace tag extraLayout Nothing) rect
        return ((rects, mainLayout'), Just $ Limit n state' $ fromMaybe extraLayout extraLayout')
    handleMess (Limit n state extraLayout) m = case fromMessage m of
        Just (ModifyLimit f) -> return $ Just $ Limit (max 1 $ f n) state extraLayout
        _ -> fmap (Limit n state) <$> handleMessage extraLayout m

toStack i list = case splitAt i list of
    (up, focus : down) -> Just $ W.Stack focus (reverse up) down
    _ -> Nothing

data Fullscreen a = Fullscreen (Maybe a)
    deriving (Read, Show)

instance LayoutModifier Fullscreen Window where
    modifyLayoutWithUpdate (Fullscreen state) workspace rect
        | Just win <- state, Just stack <- W.stack workspace, W.focus stack == win = do
            (_, update) <- runLayout workspace{W.stack = Nothing} rect
            return (([(win, rect)], update), Nothing)
        | otherwise = (, Just $ Fullscreen Nothing) <$> runLayout workspace rect
    pureMess (Fullscreen state) m = case fromMessage m of
        Just (ToggleFullscreen win) -> Just $ Fullscreen $ if state == Just win then Nothing else Just win
        _ -> Nothing

resetEmpty layout = ResetEmpty layout layout

data ResetEmpty l a = ResetEmpty (l a) (l a)
    deriving (Read, Show)

instance (LayoutClass l a) => LayoutClass (ResetEmpty l) a where
    runLayout (W.Workspace tag (ResetEmpty reset _) Nothing) rect = do
        (rects, layout') <- runLayout (W.Workspace tag reset Nothing) rect
        return (rects, Just $ ResetEmpty reset $ fromMaybe reset layout')
    runLayout (W.Workspace tag (ResetEmpty reset layout) stack) rect = do
        (rects, layout') <- runLayout (W.Workspace tag layout stack) rect
        return (rects, ResetEmpty reset <$> layout')
    handleMessage (ResetEmpty reset layout) m = fmap (ResetEmpty reset) <$> handleMessage layout m
    description (ResetEmpty _ layout) = description layout
