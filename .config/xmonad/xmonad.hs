-- Imports {{{1

-- vim: foldmethod=marker

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

import Control.Monad

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

import Graphics.X11.Xft

import Numeric

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

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.BorderResize
import XMonad.Layout.Decoration
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Stack
import XMonad.Util.WorkspaceCompare

import LocalConfig

-- Main {{{1

main = xmonad . conf =<< getTextHeight

conf textHeight = overrideConfig
    $ withUrgencyHook NoUrgencyHook $ setEwmhActivateHook doAskUrgent $ ewmh
    $ dynamicSBs (statusBar textHeight) $ docks def
        { startupHook = setDefaultCursor xC_left_ptr
        , handleEventHook = workspaceEventHook
        , manageHook = floatManageHook textHeight
        , logHook = floatLogHook
        , layoutHook = layout textHeight
        , borderWidth = 2
        , normalBorderColor = inactiveColor theme
        , focusedBorderColor = activeColor theme
        , terminal = "alacritty"
        , modMask = mod4Mask
        , keys = flip mkKeymap $ keymap textHeight
        , mouseBindings = mouse
        }

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

statusBar textHeight screen@(S i) = return $ statusBarGeneric cmd $ barLogHook screen prop where
    prop = "_XMONAD_LOG_" ++ show i
    cmd = "xmobar -p 'TopH " ++ show (barHeight textHeight) ++ "' -x " ++ show i
        ++ " -C '[Run UnsafeNamedXPropertyLog \"" ++ prop ++ "\" \"xmonad\"]'"
        ++ " -f 'Monospace " ++ show fontSize ++ "' -N 'Symbols Nerd Font " ++ show (fontSize + 2) ++ "' -N 'Monospace " ++ show (fontSize - 1) ++ "'"
        ++ " -D \"$(xrdb -get Xft.dpi)\""

barHeight textHeight = h + h `mod` 2 - 1 where
    h = textHeight * 3 `div` 2

barLogHook screen@(S sid) prop = do
    workspaceIndex <- getWorkspaceIndex
    index <- workspaceIndex . fromJust <$> screenWorkspace screen
    let getIcon w win = xmobarAction ("xdotool set_desktop_viewport " ++ show sid ++ " " ++ w ++ " windowactivate " ++ show win) "1"
            . xmobarAction ("xdotool set_desktop " ++ show index ++ " set_desktop_for_window " ++ show win ++ " " ++ show index) "3"
            <$> runQuery iconQuery win
        getIcons w = fmap ((W.tag w,) . concat) . onFocusedZ (xmobarColor "gray50" "\n")
            <$> mapZM_ (getIcon $ W.tag w) (W.stack w)
    icons <- withWindowSet $ fmap (M.fromList . catMaybes) . mapM getIcons . W.workspaces
    let rename w _ = xmobarAction ("xdotool set_desktop_viewport " ++ show sid ++ " " ++ w) "1"
            $ pad $ xmobarColor "gray33" "\n" $ (++ xmobarFont 1 (M.findWithDefault "" w icons))
            $ xmobarAction ("xdotool set_desktop " ++ show index ++ "; xdotool getactivewindow set_desktop_for_window " ++ show (workspaceIndex w)) "3" w
        showTag tag = do
            hasTag' <- withWindowSet $ mapM (hasTag tag) . W.peek
            return $ Just $ if hasTag' == Just True then " <fc=gray33>[" ++ tag ++ "]</fc>" else ""
    current <- gets $ W.current . windowset
    let color = if screen == W.screen current then [0xCD, 0x00, 0x00] {- red3 -} else [0x80, 0x80, 0x80] {- gray50 -}
        showColor [r, g, b] = let hex i = showHex (i `div` 16) . showHex (i `mod` 16) in ('#' :) . hex r . hex g . hex b
        background color = intercalate (showColor color ":0") . lines . xmobarColor "gray50" "\n"
        highlight color = xmobarBorder "Bottom" (showColor color "") 2 . background (map (`div` 4) color)
        pp = filterOutWsPP [scratchpadWorkspaceTag] def
            { ppCurrent = highlight color
            , ppVisible = highlight [0x54, 0x54, 0x54] -- gray33
            , ppUrgent = highlight [0xCD, 0x85, 0x00] -- orange3
            , ppHidden = background [0, 0, 0]
            , ppHiddenNoWindows = background [0, 0, 0]
            , ppRename = rename
            , ppTitle = xmobarRaw
            , ppTitleSanitize = id
            , ppExtras = [showTag "scratchpad"]
            , ppOrder = \(workspaces : layout : title : tags) -> [workspaces, title ++ concat tags]
            , ppSep = "<fc=gray17>│</fc> "
            , ppWsSep = ""
            }
    screenWorkspace screen >>= flip whenJust (modifyWindowSet . W.view)
    dynamicLogString pp >>= xmonadPropLog' prop
    modifyWindowSet $ W.view $ W.tag $ W.workspace current

getWorkspaceIndex = do
    sort <- getSortByIndex
    workspaces <- gets $ map W.tag . sort . W.workspaces . windowset
    return $ fromJust . (`elemIndex` workspaces)

-- Hooks {{{1

workspaceEventHook event@ClientMessageEvent{ev_data = screen : workspace : _} = do
    atom <- getAtom "_NET_DESKTOP_VIEWPORT"
    when (ev_message_type event == atom) $ screenWorkspace (fi screen)
        >>= flip whenJust (\w -> windows $ W.greedyView (show workspace) . W.view w)
    mempty
workspaceEventHook _ = mempty

floatManageHook textHeight = composeAll
    [ appName =? "xmonad-scratchpad" --> ask >>= liftX . addTag "scratchpad" >> manageCustomFloat textHeight
    , appName =? "xmonad-custom-float" --> manageCustomFloat textHeight
    , placeHook $ fixed (0.5, 0.5)
    , appName =? "xmonad-float" --> doFloat
    ]

manageCustomFloat textHeight = do
    height <- liftX $ gets $ rect_height . screenRect . W.screenDetail . W.current . windowset
    let y = fi (barHeight textHeight) / fi height
    customFloating $ W.RationalRect (2 / 3) y (1 / 3) (1 - y)

floatLogHook = withWindowSet $ mapM_ <$> tagFloating <*> W.allWindows where
    tagFloating set win = tagIff (win `M.member` W.floating set) "floating" win

tagIff = bool delTag addTag

-- Keys {{{1

keymap textHeight = let XConfig{terminal = terminal, layoutHook = layout, logHook = logHook} = conf textHeight in
    [ ("M-q", asks directories >>= flip recompile False >>= flip when (restart "xmonad" True))
    , ("M-S-q", io exitSuccess)
    , ("M-x", kill)
    , ("M-a", sortWindows)

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
    , ("M-S-<Up>", sendMessage $ Apply (windows . W.modify' . moveUp) U)
    , ("M-S-<Down>", sendMessage $ Apply (windows . W.modify' . moveDown) D)
    , ("M-S-<Left>", sendMessage $ Apply (windows . W.modify' . moveUp) L)
    , ("M-S-<Right>", sendMessage $ Apply (windows . W.modify' . moveDown) R)

    , ("M--", modifyColumns (-))
    , ("M-=", modifyColumns (+))
    , ("M-C--", sendMessage $ ModifyLimit pred)
    , ("M-C-=", sendMessage $ ModifyLimit succ)
    , ("M-\\", fmap W.stack getWorkspace >>= flip whenJust (sendMessage . ModifyLimit . const . length))
    , ("M-f", withFocused $ \w -> windows (W.sink w) >> sendMessage (ToggleFullscreen w))
    , ("M-<Backspace>", setLayout $ Layout layout)

    , ("M-c", placeFocused $ fixed (0.5, 0.5))
    , ("M-t", withFocused $ windows . W.sink)

    , ("M-s", allNamedScratchpadAction
        [ NS "" (terminal ++ " --class xmonad-scratchpad,Alacritty") (liftX . hasTag "scratchpad" =<< ask) idHook
        ] "")
    , ("M-S-s", toggleTag "scratchpad")

    , ("M-<Return>", spawn terminal)
    , ("M-i", spawn "unicode-input")
    , ("M-l", spawn "lock")
    , ("M-S-l", spawn "systemctl suspend")

    , ("<XF86AudioMute>", spawn "volume toggle")
    , ("<XF86AudioLowerVolume>", spawn "volume 2%-")
    , ("<XF86AudioRaiseVolume>", spawn "volume 2%+")
    , ("<XF86MonBrightnessDown>", spawn "brightness -10")
    , ("<XF86MonBrightnessUp>", spawn "brightness +10")

    , ("M-r", commandPrompt textHeight Shell (completionToCommand Shell) spawn =<< io getCommands)
    , ("M-S-r", commandPrompt textHeight Terminal (completionToCommand Shell) (spawnIn terminal []) =<< io getCommands)
    , ("M-o", commandPrompt textHeight Open (shellQuote . shellQuote) (safeSpawn "xdg-open" . pure) [])

    , ("M-w", do
        workspaceIndex <- getWorkspaceIndex
        i <- workspaceIndex . W.tag <$> getWorkspace
        safeSpawn "rofi" ["-show", "window", "-window-command", "xdotool set_desktop_for_window {window} " ++ show i])

    , ("M-u", focusUrgent)
    , ("M-S-u", clearUrgents >> logHook)

    , ("M-p", toggleWS' [scratchpadWorkspaceTag])
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
    ] ++

    [ ("M-<Space> " ++ mod ++ mod' ++ key, f command)
    | mod <- ["", "M-"]
    , (key, command, term) <- leaderMap
    , (mod', f) <- if term then [("", spawnIn terminal []), ("S-", spawnIn terminal ["--class=xmonad-scratchpad,Alacritty"])]
                           else [("", spawn)]
    ]

mouse XConfig{modMask = mod} = M.fromList
    [ ((mod, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((mod, button3), \w -> focus w >> mouseResizeEdgeWindow (1 / 3) w >> windows W.shiftMaster)
    ]

sortWindows = do
    wins <- W.integrate' . W.stack <$> getWorkspace
    titles <- forM wins $ runQuery title
    windows $ W.modify Nothing $ const $ W.differentiate $ map fst $ sortOn snd $ zip wins titles

moveUp win stack = stack{W.up = b, W.down = reverse a ++ W.down stack} where
    (a, b) = splitAt (succ $ fromJust $ elemIndex win $ W.up stack) $ W.up stack
moveDown win = reverseS . moveUp win . reverseS

modifyColumns op = do
    send <- flip sendMessageWithNoRefresh <$> getWorkspace
    send $ WithColumns $ \c -> send $ ModifyLimit $ \l -> l `op` ceiling (fi l / fi c)
    sendMessage $ ModifyColumns (`op` 1)

getWorkspace = gets $ W.workspace . W.current . windowset

toggleTag tag = withFocused $ \win -> do
    hasTag' <- hasTag tag win
    tagIff (not hasTag') tag win

cycleWSType = hiddenWS :&: ignoringWSs [scratchpadWorkspaceTag]

spawnIn terminal args command = safeSpawn terminal
    $ args ++ ["-e" , "sh", "-c", "printf '\\e]2;%s\\a' " ++ shellQuote command ++ "; exec " ++ command]

shellQuote = wrap "'" "'" . escape where
    escape s = case break (== '\'') s of
        (a, []) -> a
        (a, _ : b) -> a ++ "'\\''" ++ escape b

-- Prompt {{{1

promptConfig textHeight matches = def
    { promptBorderWidth = 0
    , height = textHeight
    , font = fontName theme
    , historyFilter = deleteAllDuplicates
    , promptKeymap = let isSeparator c = isSpace c || c == '/' in M.union (M.fromList
        [ ((0, xK_Up), historyUpMatching matches)
        , ((0, xK_Down), historyDownMatching matches)
        , ((mod1Mask, xK_BackSpace), killWord Prev)
        , ((mod1Mask, xK_d), killWord Next >> deleteString Next)
        , ((controlMask, xK_Left), moveCursor Prev >> moveWord Prev)
        , ((controlMask, xK_Right), moveCursor Prev >> moveWord Next >> moveCursor Next >> moveCursor Next)
        , ((shiftMask, xK_Left), moveCursor Prev >> moveWord' isSeparator Prev)
        , ((shiftMask, xK_Right), moveCursor Prev >> moveWord' isSeparator Next >> moveCursor Next >> moveCursor Next)
        , ((shiftMask, xK_Insert), pasteString)
        ]) $ defaultXPKeymap' isSeparator
    }

commandPrompt textHeight prompt escape action cmds = do
    matches <- initMatches
    let config = promptConfig textHeight matches
        strip cs = if length cs > 1 then deleteConsecutive $ dropWhileEnd (== '/') <$> cs else cs
        compl = fmap strip . getShellCompl' CaseInSensitive cmds (searchPredicate config) . escape
    mkXPrompt prompt config compl action

data Terminal = Terminal

instance XPrompt Terminal where
    showXPrompt _ = "Run in terminal: "
    completionToCommand _ = completionToCommand Shell

data Open = Open

instance XPrompt Open where
    showXPrompt _ = "Open: "
    commandToComplete _ = id
    nextCompletion _ = getNextCompletion

-- Layout {{{1

layout textHeight =
    lessBorders Screen $
    resetEmpty $
    ModifiedLayout (Fullscreen Nothing) $
    ModifiedLayout (Limit 3 (0, 0) tabbed) $
    avoidStruts $
    smartSpacingWithEdge (fi gapWidth) $
    configurableNavigation noNavigateBorders $
    borderResizeNear gapWidth $
    Columns [1, 1] (textHeight * 8) Nothing where
        tabbed = FocusFromStack Nothing $ tabbedBottom EllipsisShrinker theme{decoHeight = textHeight * 5 `div` 4}
        gapWidth = textHeight `div` 4

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

data Columns a = Columns
    { colWeights :: [Rational]
    , minWidth :: Dimension
    , focused :: Maybe (Int, Rectangle)
    }
    deriving (Read, Show)

instance (Eq a) => LayoutClass Columns a where
    doLayout layout@Columns{colWeights = weights} rect stack@W.Stack{W.focus = focus} = return (rects, layout') where
        colWins = splitList (length weights) $ W.integrate stack
        rects = splitRect (mirrorRect rect) (zip colWins weights) >>= \(wins, r) -> splitRect (mirrorRect r) $ zip wins $ repeat 1
        layout' = Just layout{focused = (,) <$> findIndex (focus `elem`) colWins <*> fmap snd (find ((focus ==) . fst) rects)}
        splitList _ [] = []
        splitList cols wins = let (a, b) = splitAt (max 1 $ length wins `div` cols) wins in a : splitList (cols - 1) b
        splitRect rect as = splitRect' rect as $ sum $ snd <$> as
        splitRect' (Rectangle x y w h) ((a, weight) : as) total = let h' = round $ fi h * weight / total
            in (a, Rectangle x y w h') : splitRect' (Rectangle x (y + fi h') w (h - h')) as (total - weight)
        splitRect' _ [] _ = []
    emptyLayout layout _ = return ([], Just layout{focused = Nothing})
    handleMessage layout@Columns{colWeights = weights} m
        | Just (ModifyColumns f) <- fromMessage m = return $ Just layout{colWeights = replicate (max 1 $ f $ length weights) 1}
        | Just (WithColumns f) <- fromMessage m = f (length weights) >> return Nothing
        | Just (SetGeometry rect@(Rectangle x' _ w' _)) <- fromMessage m,
          Just (col, Rectangle x _ w _) <- focused layout,
          w' /= w && (x' == x || col > 0),
          (before, l : r : after) <- splitAt (if x' == x then col else col - 1) weights,
          let scale = weights !! col / fi w
              delta = (fi w' - fi w) * scale
              (l', r') = if x' == x then (l + delta, r - delta) else (l - delta, r + delta)
              min = fi (minWidth layout) * scale,
          l' >= min && r' >= min
            = return $ Just layout{colWeights = before ++ l' : r' : after, focused = Just (col, rect)}
        | otherwise = return Nothing
    description _ = "Columns"

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
            (_, []) -> (rects,) <$> handleMessage extraLayout (SomeMessage Hide)
        return ((rects, mainLayout'), Just $ Limit n state' $ fromMaybe extraLayout extraLayout')
    handleMess (Limit n state extraLayout) m = case fromMessage m of
        Just (ModifyLimit f) -> return $ Just $ Limit (max 1 $ f n) state extraLayout
        _ -> fmap (Limit n state) <$> handleMessage extraLayout m

toStack i list = case splitAt i list of
    (up, focus : down) -> Just $ W.Stack focus (reverse up) down
    _ -> Nothing

data FocusFromStack l a = FocusFromStack (Maybe a) (l a)
    deriving (Read, Show)

instance (LayoutClass l Window) => LayoutClass (FocusFromStack l) Window where
    runLayout (W.Workspace tag (FocusFromStack _ layout) stack) rect = do
        let focus = fmap W.focus stack
        (rects, layout') <- tempFocus focus $ runLayout (W.Workspace tag layout stack) rect
        return (rects, Just $ FocusFromStack focus $ fromMaybe layout layout')
    handleMessage (FocusFromStack focus layout) m = case fromMessage m of
        Just PropertyEvent{} -> tempFocus focus handle
        Just ExposeEvent{} -> tempFocus focus handle
        _ -> handle
        where handle = fmap (FocusFromStack focus) <$> handleMessage layout m

tempFocus focus action = do
    ws <- gets windowset
    modifyWindowSet $ maybe id W.focusWindow focus
    r <- action
    modifyWindowSet $ const ws
    return r

data Fullscreen a = Fullscreen (Maybe a)
    deriving (Read, Show)

instance LayoutModifier Fullscreen Window where
    modifyLayoutWithUpdate (Fullscreen state) workspace@(W.Workspace _ layout stack) rect
        | Just win <- state, Just stack' <- stack, W.focus stack' == win = do
            layout' <- handleMessage layout $ SomeMessage Hide
            return (([(win, rect)], layout'), Nothing)
        | otherwise = (, Just $ Fullscreen Nothing) <$> runLayout workspace rect
    pureMess (Fullscreen state) m = case fromMessage m of
        Just (ToggleFullscreen win) -> Just $ Fullscreen $ if state == Just win then Nothing else Just win
        _ -> Nothing

resetEmpty layout = ResetEmpty layout layout

data ResetEmpty l a = ResetEmpty (l a) (l a)
    deriving (Read, Show)

instance (LayoutClass l a) => LayoutClass (ResetEmpty l) a where
    runLayout (W.Workspace tag (ResetEmpty reset layout) Nothing) rect = do
        handleMessage layout $ SomeMessage ReleaseResources
        (rects, layout') <- runLayout (W.Workspace tag reset Nothing) rect
        return (rects, Just $ ResetEmpty reset $ fromMaybe reset layout')
    runLayout (W.Workspace tag (ResetEmpty reset layout) stack) rect = do
        (rects, layout') <- runLayout (W.Workspace tag layout stack) rect
        return (rects, ResetEmpty reset <$> layout')
    handleMessage (ResetEmpty reset layout) m = fmap (ResetEmpty reset) <$> handleMessage layout m
    description (ResetEmpty _ layout) = description layout
