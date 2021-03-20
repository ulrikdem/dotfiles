-- Imports {{{1

-- vim: foldmethod=marker

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

import Control.Monad

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

import Graphics.X11.Xft

import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

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

import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Stack

import IconQuery

-- Main {{{1

main = do
    textHeight <- getTextHeight
    xmonad $ ewmh $ docks $ def
        { startupHook = barStartupHook <> setDefaultCursor xC_left_ptr
        , handleEventHook = barEventHook <> workspaceEventHook
        , logHook = do
            barLogHook
            let tagFloating set win = tagIff (win `M.member` W.floating set) "floating" win
            withWindowSet $ mapM_ <$> tagFloating <*> W.allWindows
        , manageHook = let rect = W.RationalRect 0 (2 / 3) 1 (1 / 3) in composeAll
            [ appName =? "xmonad-scratchpad" --> ask >>= liftX . addTag "scratchpad" >> customFloating rect
            , appName =? "xmonad-custom-float" --> customFloating rect
            , placeHook $ fixed (0.5, 0.5)
            , appName =? "xmonad-float" --> doFloat
            ]
        , layoutHook = resetEmpty
            $ named "tiled" (avoidStruts $ layout textHeight)
            ||| named "full" (avoidStruts $ noBorders StateFull)
            ||| named "fullscreen" (noBorders StateFull)
        , normalBorderColor = "black"
        , focusedBorderColor = "gray50"
        , terminal = terminalName
        , modMask = mod4Mask
        } `additionalKeysP` extraKeys textHeight `removeKeysP` removedKeys

terminalName = "termite"

tagIff = bool delTag addTag

-- Theme {{{1

theme = def
    { inactiveColor = "black"
    , inactiveBorderWidth = 0
    , fontName = "xft:monospace:size=9"
    }

getTextHeight = do
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

barLogHook = do
    let getIcon w = fmap (W.tag w,) $ maybe mempty (runQuery iconQuery . W.focus) $ W.stack w
    icons <- withWindowSet $ fmap M.fromList . mapM getIcon . W.workspaces
    let wrapWorkspace w = xmobarAction ("xdotool set_desktop_viewport \n " ++ w) "1"
            $ pad $ fromMaybe w $ M.findWithDefault Nothing w icons
        getScreen = do
            S i <- withWindowSet $ return . W.screen . W.current
            return $ Just $ show i
        showTag tag = do
            hasTag' <- withWindowSet $ mapM (hasTag tag) . W.peek
            return $ Just $ if hasTag' == Just True then " <fc=gray25>[" ++ tag ++ "]</fc>" else ""
        pp color = namedScratchpadFilterOutWorkspacePP def
            { ppCurrent = xmobarColor "black" color . wrapWorkspace
            , ppVisible = xmobarColor "black" "gray25" . wrapWorkspace
            , ppHidden = wrapWorkspace
            , ppHiddenNoWindows = xmobarColor "gray25" "" . wrapWorkspace
            , ppTitle = xmobarRaw . shorten 100
            , ppTitleSanitize = id
            , ppExtras = [getScreen, showTag "collapsible", showTag "scratchpad"]
            , ppOrder = \(workspaces : layout : title : screen : tags)
                -> [intercalate screen $ lines workspaces, title ++ concat tags]
            , ppSep = "<fc=gray25>│</fc> "
            , ppWsSep = ""
            }
    multiPP (pp "green4") (pp "gray50")

workspaceEventHook event@ClientMessageEvent {ev_data = screen : workspace : _} = do
    atom <- getAtom "_NET_DESKTOP_VIEWPORT"
    when (ev_message_type event == atom) $ screenWorkspace (fromIntegral screen)
        >>= flip whenJust (\w -> windows $ W.greedyView (show workspace) . W.view w)
    mempty
workspaceEventHook _ = mempty

-- Keys {{{1

removedKeys = ["M-?", "M-S-/", "M-S-<Tab>"]

extraKeys textHeight =
    [ ("M-<Return>", promote)
    , ("M-C-j", rotAllDown)
    , ("M-C-k", rotAllUp)

    , ("M-h", sendMessage $ Go L)
    , ("M-l", sendMessage $ Go R)
    , ("M-S-h", sendMessage $ Apply (windows . W.modify' . moveLeft) L)
    , ("M-S-l", sendMessage $ Apply (windows . W.modify' . moveRight) R)

    , ("M-S-m", placeFocused $ fixed (0.5, 0.5))

    , ("M-s", allNamedScratchpadAction
        [ NS "" (terminalName ++ " --name xmonad-scratchpad") (liftX . hasTag "scratchpad" =<< ask) idHook
        ] "")
    , ("M-S-s", toggleTag "scratchpad")
    , ("M-c", toggleTag "collapsible")

    , ("M-a", sendMessage AddColumn)
    , ("M-d", sendMessage DeleteColumn)
    , ("M-,", sendMessage $ ModifyLimit succ)
    , ("M-.", sendMessage $ ModifyLimit pred)
    , ("M--", sendMessage $ ModifyColWeight (/ weightFactor))
    , ("M-=", sendMessage $ ModifyColWeight (* weightFactor))
    , ("M-<Backspace>", sendMessage ResetColWeights)
    , ("M-S--", sendMessage $ ModifyWinWeight (/ weightFactor))
    , ("M-S-=", sendMessage $ ModifyWinWeight (* weightFactor))
    , ("M-S-<Backspace>", sendMessage ResetWinWeights)

    , ("M-<Space>", sendMessage $ JumpToLayout "tiled")
    , ("M-f", sendMessage $ JumpToLayout "full")
    , ("M-S-f", sendMessage $ JumpToLayout "fullscreen")

    , ("M-g", windowPrompt (windowPromptConfig textHeight) Goto allWindows)
    , ("M-S-g", windowPrompt (windowPromptConfig textHeight) Bring allWindows)
    , ("M-p", shellPrompt . commandPromptConfig textHeight =<< initMatches)
    , ("M-S-p", terminalPrompt . commandPromptConfig textHeight =<< initMatches)

    , ("M-b", spawn "luakit")
    , ("M-S-b", spawn "luakit --private")
    , ("M-S-a", spawn "spacer")
    , ("M-u", spawn "unicode-input")
    , ("M-z", spawn "lock")

    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")
    , ("<XF86MonBrightnessDown>", spawn "light -U 10")
    , ("<XF86MonBrightnessUp>", spawn "light -A 10")

    , ("M-<Tab>", toggleWS' ["NSP"])
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
    , (key, index) <- zip "wer" [0..]
    ]

moveLeft win stack = stack {W.up = b, W.down = reverse a ++ W.down stack} where
    (a, b) = splitAt (succ $ fromJust $ elemIndex win $ W.up stack) $ W.up stack
moveRight win = reverseS . moveLeft win . reverseS

toggleTag tag = withFocused $ \win -> do
    hasTag' <- hasTag tag win
    tagIff (not hasTag') tag win
    barLogHook

weightFactor = 1.26

cycleWSType = WSIs $ do
    hidden <- gets $ map W.tag . W.hidden . windowset
    return $ (\w -> w `elem` hidden && w /= "NSP") . W.tag

-- Prompt {{{1

windowPromptConfig textHeight = def
    { promptBorderWidth = 0
    , height = textHeight
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

commandPromptConfig textHeight matches = def
    { promptBorderWidth = 0
    , height = textHeight
    , font = fontName theme
    , historyFilter = deleteAllDuplicates
    , promptKeymap = M.union (M.fromList
        [ ((controlMask, xK_p), historyUpMatching matches)
        , ((controlMask, xK_n), historyDownMatching matches)
        ]) $ promptKeymap $ windowPromptConfig textHeight
    }

terminalPrompt config = do
    cmds <- io getCommands
    mkXPrompt TerminalPrompt config (getShellCompl cmds $ searchPredicate config)
        $ \c -> safeSpawn terminalName ["-e", c]

data TerminalPrompt = TerminalPrompt

instance XPrompt TerminalPrompt where
    showXPrompt _ = "Run in terminal: "
    completionToCommand _ = completionToCommand Shell

-- Layout {{{1

layout textHeight = addDecoration $ addGaps $ addNavigation customLayout where
    addDecoration = decoration EllipsisShrinker theme CollapseDecoration
    addGaps = spacingWithEdge gapWidth
    addNavigation = configurableNavigation noNavigateBorders
    customLayout = EmptyLayout [def, def {limit = maxBound}]
        $ textHeight + fromIntegral gapWidth * 2 :: CustomLayout Window
    gapWidth = round $ fromIntegral textHeight / 4

data CollapseDecoration a = CollapseDecoration
    deriving (Read, Show)

instance DecorationStyle CollapseDecoration Window where
    decorate _ _ _ _ stack _ (win, rect) = bool Nothing (Just rect) <$> isCollapsed stack win
    shrink _ _ = id

isCollapsed stack win = (&& win /= W.focus stack) <$> hasTag "collapsible" win

data EllipsisShrinker = EllipsisShrinker
    deriving (Read, Show)

instance Shrinker EllipsisShrinker where
    shrinkIt _ s = s : map (++ "…") (reverse $ inits $ init s)

data Column = Column
    { limit :: Int
    , colWeight :: Rational
    , winWeights :: M.Map Int Rational
    }
    deriving (Read, Show)

instance Default Column where
    def = Column 1 1 M.empty

data CustomLayout a
    = CustomLayout (W.Stack Column) Int Dimension
    | EmptyLayout [Column] Dimension
    deriving (Read, Show)

instance LayoutClass CustomLayout Window where
    doLayout layout rect stack = do
        let (cols, collapsedHeight) = case layout of
                CustomLayout c _ h -> (W.integrate c, h)
                EmptyLayout c h -> (c, h)
            wins = W.integrate stack

        collapsedWins <- forM wins $ isCollapsed stack
        let collapsedWins' = M.fromList $ zip (map fst $ filter snd $ zip wins collapsedWins) $ repeat Nothing
            allCollapsed = all (`M.member` collapsedWins')

            split rect weights = split' rect weights where
                split' rect@Rectangle {rect_height = h} (Just weight : weights) =
                    setHeight rect weights $ round $ fromIntegral (h - collapsedHeight' * numCollapsed) * relWeight
                    where
                        numCollapsed = fromIntegral (length $ filter isNothing weights)
                        relWeight = weight / (weight + sum (catMaybes weights))
                split' rect (Nothing : weights) = setHeight rect weights collapsedHeight'
                split' _ [] = []
                setHeight (Rectangle x y w h) weights h' =
                    Rectangle x y w h' : split' (Rectangle x (y + fromIntegral h') w (h - h')) weights
                collapsedHeight' = min collapsedHeight $ rect_height rect `div` fromIntegral (length weights)

            layoutCol col rect wins = zip wins $ split rect $ map findWeight wins where
                findWeight win = M.findWithDefault (Just 1) win $ M.union collapsedWins'' winWeights'
                collapsedWins'' = if allCollapsed wins then M.empty else collapsedWins'
                winWeights' = M.map Just $ M.mapKeys (wins !!) $ M.takeWhileAntitone (< length wins) $ winWeights col

            cumLimits = scanl (+) 0 $ limit <$> init cols

            colWins = takeWhile (not . null) $ zipWith take (map limit cols) $ map (`drop` wins) cumLimits
            colWeight' col wins = if allCollapsed wins then Nothing else Just $ colWeight col
            colRects = map mirrorRect $ split (mirrorRect rect) $ zipWith colWeight' cols colWins
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

    handleMessage (CustomLayout cols@W.Stack {W.focus = col} focus collapsedHeight) message =
        case fromMessage message of
            Just AddColumn -> fixFocus cols
                { W.focus = def {colWeight = colWeight col}
                , W.down = col : W.down cols
                }
            Just DeleteColumn -> skipLastCol $ fixFocus cols
                { W.focus = head $ W.down cols
                , W.down = tail $ W.down cols
                }
            Just (ModifyLimit f) -> skipLastCol $ fixFocus cols
                { W.focus = col {limit = max 1 $ f $ limit col}
                }
            Just (ModifyColWeight f) -> fixFocus cols
                { W.focus = col {colWeight = f $ colWeight col}
                }
            Just ResetColWeights -> fixFocus W.Stack
                { W.up = resetWeight <$> W.up cols
                , W.focus = resetWeight col
                , W.down = resetWeight <$> W.down cols
                }
            Just (ModifyWinWeight f) -> fixFocus cols
                { W.focus = col {winWeights = M.alter (mfilter (/= 1) . Just . f . fromMaybe 1) focus $ winWeights col}
                }
            Just ResetWinWeights -> fixFocus cols
                { W.focus = col {winWeights = M.empty}
                }
            Nothing -> return Nothing
        where
            fixFocus cols = do
                let focus' = min focus $ pred $ limit $ W.focus cols
                modifyWindowSet $ foldr (.) id $ replicate (focus - focus') W.focusUp
                return $ Just $ CustomLayout cols focus' collapsedHeight
            skipLastCol layout = if null (W.down cols) then return Nothing else layout
            resetWeight col = col {colWeight = 1}
    handleMessage (EmptyLayout _ _) _ = return Nothing

data LayoutMessage
    = AddColumn
    | DeleteColumn
    | ModifyLimit (Int -> Int)
    | ModifyColWeight (Rational -> Rational)
    | ResetColWeights
    | ModifyWinWeight (Rational -> Rational)
    | ResetWinWeights
    deriving (Typeable)

instance Message LayoutMessage

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
