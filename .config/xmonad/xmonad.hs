-- Imports {{{1

-- vim: foldmethod=marker
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Control.Monad
import Data.Bool
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xft
import XMonad hiding ((|||))
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.TagWindows
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Layout.Decoration
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.StateFull
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.CustomKeys
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad

-- Main {{{1

main = getFontHeight >>= \fontHeight -> xmonad $ ewmh $ docks def
    { startupHook = barStartupHook <+> setDefaultCursor xC_left_ptr
    , handleEventHook = barEventHook
    , logHook = barLogHook <+> withWindowSet (\s -> mapM_ (\w ->
        (if M.member w $ W.floating s then addTag else delTag) "floating" w) $ W.allWindows s)
    , normalBorderColor = "black"
    , focusedBorderColor = "gray50"
    , modMask = mod4Mask
    , keys = customKeys delKeys $ insKeys fontHeight
    , terminal = "termite"
    , manageHook = let r = W.RationalRect 0 (2 / 3) 1 (1 / 3) in composeAll
        [ scratchpadManageHook r
        , appName =? "xmonad-custom-float" --> customFloating r
        , placeHook $ fixed (0.5, 0.5)
        , appName =? "xmonad-float" --> doFloat
        ]
    , layoutHook = named "tiled" (avoidStruts $ layout fontHeight)
        ||| named "full" (avoidStruts $ noBorders StateFull)
        ||| named "fullscreen" (noBorders StateFull)
    }

-- Theme {{{1

theme = def
    { inactiveColor = "black"
    , inactiveBorderWidth = 0
    , fontName = "xft:monospace:size=" ++ show fontSize
    }

fontSize = 9

getFontHeight = do
    display <- openDisplay ""
    font <- xftFontOpen display (defaultScreenOfDisplay display) $ fontName theme
    ascent <- xftfont_ascent font
    descent <- xftfont_descent font
    xftFontClose display font
    closeDisplay display
    return $ fromIntegral $ ascent + descent + 2

-- Bar {{{1

barStartupHook = dynStatusBarStartup spawnBar $ return ()
barEventHook = dynStatusBarEventHook spawnBar $ return ()

spawnBar (S i) = spawnPipe $ "xmobar -x " ++ show i
    ++ " -f '" ++ fontName theme ++ ",Symbols Nerd Font:size=" ++ show (fontSize + 4) ++ "'"

barLogHook = multiPP pp {ppCurrent = xmobarColor "black" "darkgreen" . wrapWorkspace}
    pp {ppCurrent = xmobarColor "black" "gray50" . wrapWorkspace}

pp = namedScratchpadFilterOutWorkspacePP def
    { ppVisible = wrapWorkspace
    , ppHidden = wrapWorkspace
    , ppVisibleNoWindows = Just $ xmobarColor "gray25" "" . wrapWorkspace
    , ppHiddenNoWindows = xmobarColor "gray25" "" . wrapWorkspace
    , ppTitle = xmobarRaw . shorten 120
    , ppTitleSanitize = id
    , ppOrder = \[w, l, t] -> [w, t]
    , ppSep = "<fc=gray25>│</fc> "
    , ppWsSep = ""
    }

wrapWorkspace s = xmobarAction ("xdotool key super+" ++ s) "1" $ pad s

-- Keys {{{1

delKeys XConfig {modMask = mod} =
    [ (mod, xK_question)
    , (mod .|. shiftMask, xK_slash)
    , (mod .|. shiftMask, xK_Tab)
    ]

insKeys fontHeight XConfig {modMask = mod, terminal = term} =
    [ ((mod, xK_g), windowPrompt (windowPromptConf fontHeight) Goto allWindows)
    , ((mod .|. shiftMask, xK_g), windowPrompt (windowPromptConf fontHeight) Bring allWindows)

    , ((mod, xK_p), shellPrompt . promptConf fontHeight =<< initMatches)
    , ((mod .|. shiftMask, xK_p), termPrompt term . promptConf fontHeight =<< initMatches)

    , ((mod, xK_s), scratchpadSpawnActionCustom $ term ++ " --name scratchpad")
    , ((mod, xK_b), spawn "luakit")
    , ((mod .|. shiftMask, xK_b), spawn "luakit --private")
    , ((mod, xK_z), spawn "lock")

    , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+")
    , ((0, xF86XK_MonBrightnessDown), spawn "light -U 25")
    , ((0, xF86XK_MonBrightnessUp), spawn "light -A 25")

    , ((mod .|. shiftMask, xK_m), placeFocused $ fixed (0.5, 0.5))
    , ((mod, xK_Return), promote)
    , ((mod .|. controlMask, xK_j), rotAllDown)
    , ((mod .|. controlMask, xK_k), rotAllUp)
    , ((mod, xK_h), sendMessage $ Go L)
    , ((mod, xK_l), sendMessage $ Go R)
    , ((mod .|. shiftMask, xK_h), sendMessage $ Apply (windows . W.modify' . moveLeft) L)
    , ((mod .|. shiftMask, xK_l), sendMessage $ Apply (windows . W.modify' . moveRight) R)

    , ((mod, xK_a), sendMessage AddColumn)
    , ((mod, xK_d), sendMessage DeleteColumn)
    , ((mod, xK_comma), sendMessage $ ModifyLimit succ)
    , ((mod, xK_period), sendMessage $ ModifyLimit pred)
    , ((mod, xK_minus), sendMessage $ ModifyColWeight (/ weightFactor))
    , ((mod, xK_equal), sendMessage $ ModifyColWeight (* weightFactor))
    , ((mod, xK_BackSpace), sendMessage $ ModifyColWeight $ const 1)
    , ((mod .|. shiftMask, xK_minus), sendMessage $ ModifyWinWeight (/ weightFactor))
    , ((mod .|. shiftMask, xK_equal), sendMessage $ ModifyWinWeight (* weightFactor))
    , ((mod .|. shiftMask, xK_BackSpace), sendMessage $ ModifyWinWeight $ const 1)
    , ((mod, xK_c), withFocused (\w -> ($ w) . ($ "collapsible") . bool addTag delTag =<< hasTag "collapsible" w))

    , ((mod, xK_space), sendMessage $ JumpToLayout "tiled")
    , ((mod, xK_f), sendMessage $ JumpToLayout "full")
    , ((mod .|. shiftMask, xK_f), sendMessage $ JumpToLayout "fullscreen")

    , ((mod, xK_Tab), toggleWS)
    , ((mod .|. shiftMask, xK_comma), moveTo Prev cycleWSType)
    , ((mod .|. shiftMask, xK_period), moveTo Next cycleWSType)
    ] ++
    [ ((mod .|. controlMask, k), windows $ swapWithCurrent w)
    | (k, w) <- zip [xK_1..] $ workspaces def
    ] ++

    [ ((mod .|. m, k), f i)
    | (m, f) <-
        [ (0, viewScreen def)
        , (shiftMask, sendToScreen def)
        , (controlMask,
            (flip whenJust ((flip whenJust (windows . W.greedyView) =<<) . screenWorkspace) =<<) . getScreen def)
        ]
    , (k, i) <- zip [xK_w, xK_e, xK_r] [0..]
    ]

moveLeft win stack = stack {W.up = b, W.down = reverse a ++ W.down stack} where
    (a, b) = splitAt (succ $ fromJust $ elemIndex win $ W.up stack) $ W.up stack
moveRight win stack = stack {W.down = b, W.up = reverse a ++ W.up stack} where
    (a, b) = splitAt (succ $ fromJust $ elemIndex win $ W.down stack) $ W.down stack

weightFactor = 1.26

cycleWSType = WSIs $ do
    hidden <- gets $ map W.tag . W.hidden . windowset
    return $ (\w -> w `elem` hidden && w /= "NSP") . W.tag

-- Prompt {{{1

windowPromptConf fontHeight = def
    { promptBorderWidth = 0
    , height = fontHeight
    , font = fontName theme
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    , historySize = 0
    , promptKeymap = M.union (M.fromList
        [ ((controlMask, xK_u), killBefore)
        , ((controlMask, xK_w), killWord Prev)
        , ((mod1Mask, xK_BackSpace), killWord' (\c -> isSpace c || c == '/') Prev)
        ]) emacsLikeXPKeymap
    }

promptConf fontHeight matches = def
    { promptBorderWidth = 0
    , height = fontHeight
    , font = fontName theme
    , historyFilter = deleteAllDuplicates
    , promptKeymap = M.union (M.fromList
        [ ((controlMask, xK_p), historyUpMatching matches)
        , ((controlMask, xK_n), historyDownMatching matches)
        ]) $ promptKeymap $ windowPromptConf fontHeight
    }

termPrompt term conf = do
    cmds <- io getCommands
    mkXPrompt TermPrompt conf (getShellCompl cmds $ searchPredicate conf) $ \c -> safeSpawn term ["-e", c]

data TermPrompt = TermPrompt

instance XPrompt TermPrompt where
    showXPrompt _ = "Run in terminal: "
    completionToCommand _ = completionToCommand Shell

-- Layout {{{1

layout :: Dimension -> ModifiedLayout (Decoration CollapseDeco DefaultShrinker)
    (ModifiedLayout Spacing (ModifiedLayout WindowNavigation CustomLayout)) Window
layout fontHeight = decoration shrinkText theme {decoHeight = fontHeight} CollapseDeco
    $ spacingWithEdge gapWidth $ configurableNavigation noNavigateBorders
    $ EmptyLayout [def, def {limit = maxBound}] $ fontHeight + fromIntegral gapWidth * 2
    where
        gapWidth = round $ fromIntegral fontHeight / 3

data CollapseDeco a = CollapseDeco
    deriving (Read, Show)

instance DecorationStyle CollapseDeco Window where
    decorate _ _ height _ stack _ (win, rect) =
        fmap (bool Nothing (Just rect) . (&& rect_height rect <= height)) $ isCollapsed stack win
    shrink _ _ = id

isCollapsed stack win = fmap (&& win /= W.focus stack) $ hasTag "collapsible" win

data Column = Column
    { limit :: Int
    , colWeight :: Rational
    , winWeights :: M.Map Int Rational
    }
    deriving (Read, Show)

instance Default Column where
    def = Column {limit = 1, colWeight = 1, winWeights = M.empty}

data CustomLayout a = CustomLayout (W.Stack Column) Int Dimension | EmptyLayout [Column] Dimension
    deriving (Read, Show)

instance LayoutClass CustomLayout Window where
    doLayout layout rect stack = do
        let wins = W.integrate stack
        collapsed <- mapM (isCollapsed stack) wins

        let (cols, collapsedHeight) = case layout of
                CustomLayout c _ h -> (W.integrate c, h)
                EmptyLayout c h -> (c, h)
            cumLimits = scanl (+) 0 $ map limit $ init cols

            split rect weights = split' rect weights where
                split' rect@Rectangle {rect_height = height} (Just weight : weights) = setHeight rect weights $ round
                    $ fromIntegral (height - collapsedHeight' * fromIntegral (length $ filter isNothing weights))
                        * weight / (weight + sum (catMaybes weights))
                split' rect (Nothing : weights) = setHeight rect weights collapsedHeight
                split' _ [] = []
                setHeight (Rectangle x y w h) weights h' =
                    Rectangle x y w h' : split' (Rectangle x (y + fromIntegral h') w (h - h')) weights
                collapsedHeight' = min collapsedHeight $ rect_height rect `div` fromIntegral (length weights)

            collapsedWins = M.fromList $ zip (map fst $ filter snd $ zip wins collapsed) $ repeat Nothing
            layoutCol col rect wins = zip wins $ split rect
                $ map (flip (M.findWithDefault $ Just 1) $ M.union collapsedWins' winWeights') wins
                where
                    collapsedWins' = if all (flip M.member collapsedWins) wins then M.empty else collapsedWins
                    winWeights' = M.map Just $ M.mapKeys (wins !!) $ M.takeWhileAntitone (< length wins) $ winWeights col

            colWins = takeWhile (not . null) $ zipWith take (map limit cols) $ zipWith drop cumLimits $ repeat wins
            colRects = map mirrorRect $ split (mirrorRect rect) $ map (Just . colWeight) $ take (length colWins) cols
            winRects = concat $ zipWith3 layoutCol cols colRects colWins

            colIndex = pred $ fromMaybe (length cols) $ findIndex (> length (W.up stack)) cumLimits
            winIndex = length (W.up stack) - cumLimits !! colIndex
            layout' = CustomLayout W.Stack
                { W.up = reverse $ take colIndex cols
                , W.focus = cols !! colIndex
                , W.down = drop (colIndex + 1) cols
                } winIndex collapsedHeight

        return (winRects, Just layout')

    emptyLayout (CustomLayout cols _ height) _ = return ([], Just $ EmptyLayout (W.integrate cols) height)
    emptyLayout (EmptyLayout _ _) _ = return ([], Nothing)

    handleMessage (CustomLayout cols@W.Stack {W.focus = col} focus height) m =
        case fromMessage m of
            Just AddColumn -> fixFocus $ CustomLayout cols
                { W.focus = def
                , W.down = col : W.down cols
                } focus height
            Just DeleteColumn -> skipLastCol $ fixFocus $ CustomLayout cols
                { W.focus = head $ W.down cols
                , W.down = tail $ W.down cols
                } focus height
            Just (ModifyLimit f) -> skipLastCol $ fixFocus $ CustomLayout cols
                { W.focus = col {limit = max 1 $ f $ limit col}
                } focus height
            Just (ModifyColWeight f) -> return $ Just $ CustomLayout cols
                { W.focus = col {colWeight = f $ colWeight col}
                } focus height
            Just (ModifyWinWeight f) -> return $ Just $ CustomLayout cols
                { W.focus = col {winWeights = M.alter (mfilter (/= 1) . Just . f . fromMaybe 1) focus $ winWeights col}
                } focus height
            Nothing -> return Nothing
        where
            skipLastCol r = if null (W.down cols) then return Nothing else r
            fixFocus (CustomLayout cols focus height) = do
                let focus' = min focus $ pred $ limit $ W.focus cols
                modifyWindowSet $ foldr (.) id $ replicate (focus - focus') W.focusUp
                return $ Just $ CustomLayout cols focus' height
    handleMessage (EmptyLayout _ _) _ = return Nothing

data LayoutMessage
    = AddColumn
    | DeleteColumn
    | ModifyLimit (Int -> Int)
    | ModifyColWeight (Rational -> Rational)
    | ModifyWinWeight (Rational -> Rational)
    deriving (Typeable)

instance Message LayoutMessage
