-- Imports {{{1

-- vim: foldmethod=marker
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Control.Monad
import Data.Bool
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Graphics.X11.Xft
import IconRules
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
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad

-- Main {{{1

main = getFontHeight >>= \fontHeight -> xmonad $ ewmh $ docks $ def
    { startupHook = setDefaultCursor xC_left_ptr >> barStartupHook
    , handleEventHook = barEventHook
    , logHook = barLogHook >> withWindowSet (\s -> mapM_ (\w ->
        (if M.member w $ W.floating s then addTag else delTag) "floating" w) $ W.allWindows s)
    , manageHook = let r = W.RationalRect 0 (2 / 3) 1 (1 / 3) in composeAll
        [ scratchpadManageHook r
        , appName =? "xmonad-custom-float" --> customFloating r
        , placeHook $ fixed (0.5, 0.5)
        , appName =? "xmonad-float" --> doFloat
        ]
    , layoutHook = named "tiled" (avoidStruts $ layout fontHeight)
        ||| named "full" (avoidStruts $ noBorders StateFull)
        ||| named "fullscreen" (noBorders StateFull)
    , normalBorderColor = "black"
    , focusedBorderColor = "gray50"
    , terminal = term
    , modMask = mod4Mask
    } `additionalKeysP` extraKeys fontHeight `removeKeysP` removedKeys

term = "termite"

-- Theme {{{1

theme = def
    { inactiveColor = "black"
    , inactiveBorderWidth = 0
    , fontName = "xft:monospace:size=9"
    }

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

spawnBar (S i) = spawnPipe $ "xmobar -x " ++ show i ++ " -f '" ++ fontName theme ++ ",Symbols Nerd Font:size=11'"

barLogHook = withWindowSet $ \s -> do
    icons <- fmap M.fromList $ mapM workspaceIcon $ W.workspaces s
    multiPP (pp icons "darkgreen") (pp icons "gray50")

pp icons color = namedScratchpadFilterOutWorkspacePP def
    { ppCurrent = xmobarColor "black" color . wrapWorkspace
    , ppVisible = wrapWorkspace
    , ppHidden = wrapWorkspace
    , ppVisibleNoWindows = Just $ xmobarColor "gray25" "" . wrapWorkspace
    , ppHiddenNoWindows = xmobarColor "gray25" "" . wrapWorkspace
    , ppTitle = xmobarRaw . shorten 120
    , ppTitleSanitize = id
    , ppOrder = \[w, l, t] -> [w, t]
    , ppSep = "<fc=gray25>│</fc> "
    , ppWsSep = ""
    }
    where
        wrapWorkspace w = xmobarAction ("xdotool key super+" ++ w) "1" $ pad
            $ fromMaybe w $ wrap "<fn=1>" "</fn>" <$> M.findWithDefault Nothing w icons

workspaceIcon w = do
    icons <- case W.stack w of
        Just s -> mapM (\(q, i) -> fmap (bool Nothing $ Just i) $ runQuery q $ W.focus s) iconRules
        Nothing -> return []
    return (W.tag w, foldr (flip maybe Just) Nothing icons)

-- Keys {{{1

removedKeys = ["M-?", "M-S-/", "M-S-<Tab>"]

extraKeys fontHeight =
    [ ("M-g", windowPrompt (windowPromptConf fontHeight) Goto allWindows)
    , ("M-S-g", windowPrompt (windowPromptConf fontHeight) Bring allWindows)

    , ("M-p", shellPrompt . promptConf fontHeight =<< initMatches)
    , ("M-S-p", termPrompt term . promptConf fontHeight =<< initMatches)

    , ("M-s", scratchpadSpawnActionCustom $ term ++ " --name scratchpad")
    , ("M-b", spawn "luakit")
    , ("M-S-b", spawn "luakit --private")
    , ("M-z", spawn "lock")

    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
    , ("<XF86MonBrightnessDown>", spawn "light -U 25")
    , ("<XF86MonBrightnessUp>", spawn "light -A 25")

    , ("M-S-m", placeFocused $ fixed (0.5, 0.5))
    , ("M-<Return>", promote)
    , ("M-C-j", rotAllDown)
    , ("M-C-k", rotAllUp)
    , ("M-h", sendMessage $ Go L)
    , ("M-l", sendMessage $ Go R)
    , ("M-S-h", sendMessage $ Apply (windows . W.modify' . moveLeft) L)
    , ("M-S-l", sendMessage $ Apply (windows . W.modify' . moveRight) R)

    , ("M-a", sendMessage AddColumn)
    , ("M-d", sendMessage DeleteColumn)
    , ("M-,", sendMessage $ ModifyLimit succ)
    , ("M-.", sendMessage $ ModifyLimit pred)
    , ("M--", sendMessage $ ModifyColWeight (/ weightFactor))
    , ("M-=", sendMessage $ ModifyColWeight (* weightFactor))
    , ("M-<Backspace>", sendMessage $ ModifyColWeight $ const 1)
    , ("M-S--", sendMessage $ ModifyWinWeight (/ weightFactor))
    , ("M-S-=", sendMessage $ ModifyWinWeight (* weightFactor))
    , ("M-S-<Backspace>", sendMessage $ ModifyWinWeight $ const 1)
    , ("M-c", withFocused (\w -> ($ w) . ($ "collapsible") . bool addTag delTag =<< hasTag "collapsible" w))

    , ("M-<Space>", sendMessage $ JumpToLayout "tiled")
    , ("M-f", sendMessage $ JumpToLayout "full")
    , ("M-S-f", sendMessage $ JumpToLayout "fullscreen")

    , ("M-<Tab>", toggleWS)
    , ("M-S-,", moveTo Prev cycleWSType)
    , ("M-S-.", moveTo Next cycleWSType)
    ] ++
    [ ("M-C-" ++ [k], windows $ swapWithCurrent w)
    | (k, w) <- zip ['1'..] $ workspaces def
    ] ++

    [ ("M-" ++ m ++ [k], f i)
    | (m, f) <-
        [ ("", viewScreen def)
        , ("S-", sendToScreen def)
        , ("C-", (flip whenJust ((flip whenJust (windows . W.greedyView) =<<) . screenWorkspace) =<<) . getScreen def)
        ]
    , (k, i) <- zip "wer" [0..]
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
        bool Nothing (Just rect) . (&& rect_height rect <= height) <$> isCollapsed stack win
    shrink _ _ = id

isCollapsed stack win = (&& win /= W.focus stack) <$> hasTag "collapsible" win

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
            cumLimits = scanl (+) 0 $ limit <$> init cols

            split rect weights = split' rect weights where
                split' rect@Rectangle {rect_height = height} (Just weight : weights) = setHeight rect weights $ round
                    $ fromIntegral (height - collapsedHeight' * fromIntegral (length $ filter isNothing weights))
                        * weight / (weight + sum (catMaybes weights))
                split' rect (Nothing : weights) = setHeight rect weights collapsedHeight'
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
            colRects = map mirrorRect $ split (mirrorRect rect) $ Just . colWeight <$> take (length colWins) cols
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
