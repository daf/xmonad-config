-- {-# OPTIONS_GHC -fglasgow-exts #-} -- required for XMonad.Layout.MultiToggle
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import XMonad
import XMonad.Layout.CenteredMaster
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run (spawnPipe)
import System.IO (hPutStrLn)
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.Roledex
import XMonad.Layout.Dishes
import XMonad.Layout.Magnifier
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Tabbed
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.DwmStyle
--import XMonad.Layout.GridVariants
import XMonad.Layout.HintedGrid
import Data.Ratio
import XMonad.Layout.Drawer

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib
import XMonad.Layout.LayoutHints
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.ManageDocks
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Layout.IndependentScreens
-- import XMonad.Layout.Magnifier as Mag
import XMonad.Config.Gnome
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.TwoPane
import XMonad.Layout.StackTile
import XMonad.Actions.FloatKeys

-- workspaces' :: [WorkspaceId]
-- workspaces' =  ["a", "b", "c", "d", "e", "f", "g", "h", "i"]

modMask' :: KeyMask
modMask' = mod4Mask

borderWidth' :: Dimension
borderWidth' = 1

normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#000000"
focusedBorderColor' = "#3696ef"


defaultGaps' :: [(Int,Int,Int,Int)]
defaultGaps' = [(15,0,0,0), (15,0,0,0)] -- 15 for default dzen font

terminal' :: String
terminal' = "gnome-terminal"


keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_t), spawn $ XMonad.terminal conf) -- %! Launch terminal
    --, ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- %! Launch dmenu
    , ((modMask,               xK_r     ), spawn "gmrun") -- %! Launch gmrun
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window
    , ((modMask .|. shiftMask, xK_s), spawn "gnome-terminal -x ssh l3ib.org") -- %! Launch ssh to l3ib
    , ((modMask .|. shiftMask, xK_y), spawn "chromium-browser") -- %! Launch browser

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask .|. shiftMask, xK_Return), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area
    -- , ((modMask .|. shiftMask, xK_m     ), sendMessage $ XMonad.Layout.MultiToggle.Toggle MAGNIFICATION) -- %! Magnify anything?
    , ((modMask,               xK_m     ), sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling
    , ((modMask,               xK_d     ), withFocused $ keysResizeWindow (-50,-50) (1,1)) -- %! shrink window
    , ((modMask,               xK_s     ), withFocused $ keysResizeWindow (50,50) (1,1)) -- %! biggen window

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- toggle the status bar gap
--    , ((modMask              , xK_b     ), modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i in if n == x then (0,0,0,0) else x)) -- %! Toggle the status bar gap
    , ((modMask              , xK_minus ), viewEmptyWorkspace)
    , ((modMask .|. shiftMask, xK_minus ), tagToEmptyWorkspace)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True) -- %! Restart xmonad
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{bracketleft,bracketright} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{bracketleft,bracketright} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_bracketright, xK_bracketleft] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myDWConfig :: Theme
myDWConfig = defaultTheme { inactiveBorderColor =  "black"
                          , inactiveColor       =  "#666666"
                          , inactiveTextColor   =  "black"
                          , activeBorderColor   =  "#3696ef"
                          , activeColor         =  "#285577"
                          , activeTextColor     =  "#ffffff"
                          , fontName            =  "xft:Consolas-8"
                          }

-- data MAGNIFICATION = MAGNIFICATION deriving (Read, Show, Eq, Typeable)
-- instance Transformer MAGNIFICATION Window where
--    transform _ x k = k (magnifiercz 1.2 x)

-- attempt to fill in my own accordion style layout
data AccordStack a = AccordStack Dimension deriving ( Show, Read  )

instance LayoutClass AccordStack a where
    pureLayout (AccordStack decoHeight) sc ws = [(W.focus ws, mainRect)] ++ (reverse (zip dns bottoms)) ++ zip ups (reverse tops)
      where
        ups = W.up ws
        dns = W.down ws
        lups = fromIntegral (length ups)
        ldns = fromIntegral (length dns)
        mainRect = Rectangle x my w mh
        my = y + tdh lups
        mh = h - tdh (ldns + lups)
        tops = map topRect $ [0..(lups - 1)]
        bottoms = map bottomRect $ [0..(ldns - 1)]
        tdh n = fromIntegral (n * decoHeight)
        (Rectangle x y w h) = sc
        topRect n = Rectangle x (y + tdh n) w (h - tdh n)
        bottomRect n = Rectangle x (y + tdh lups + (fromIntegral mh) + tdh n) w mh

data Roof a = Roof Dimension deriving ( Show, Read )

instance LayoutClass Roof a where
  pureLayout (Roof decoHeight) sc ws = [(W.focus ws, mainRect)] ++ zip ups (reverse tops) ++ zip dns (reverse bottoms)
    where
      ups = W.up ws
      dns = W.down ws
      lups = fromIntegral (length ups)
      ldns = fromIntegral (length dns)
      mainRect = Rectangle x my w mh
        where
          my = y + tdh ldns
          mh = h - tdh ldns
      tops = map topRect $ [0..(lups-1)]
      bottoms = map bottomRect $ [0..(ldns-1)]
      tdh n = fromIntegral (n * decoHeight)
      topRect n = Rectangle x (y + tdh (ldns + 1 + n)) w (h - (tdh (ldns + 1 + n)))
      bottomRect n = Rectangle x (y + tdh n) w (h - tdh n)
      (Rectangle x y w h) = sc

myLayoutHook = avoidStruts
               $ hints
               $ (noFrillsDeco shrinkText myDWConfig
               $ (mkToggle (single MIRROR)
               -- $ mkToggle (single MAGNIFICATION)
               $ (spacing 0 $ (Mirror tiled) ||| grido)))
               ||| tabbed shrinkText myDWConfig ||| stackertile ||| accordo ||| roofer
    where
      hints = layoutHints
      mtiled = centerMaster $ (Mirror tiled)
      tiled = Tall nmaster delta ratio
      nmaster = 1
      delta = 3/100
      ratio = 10/16
      magnify = magnifiercz (12%10)
      rmagnify = magnifiercz (10%12)
      twopane = TwoPane (3/100) (1/2)
      stackertile = StackTile 1 (3/100) (2/3)
--      grido = SplitGrid XMonad.Layout.GridVariants.L 1 2 (2/3) (16/10) (5/100)
--      grido = Grid (16/10)
      grido = centerMaster $ Grid True
      drawer = simpleDrawer 0.01 0.3 (ClassName "Empathy")
      dishes = Dishes 1 (1/8)
      accordo = noFrillsDeco shrinkText myDWConfig (AccordStack (decoHeight myDWConfig))
      roofer = noFrillsDeco shrinkText myDWConfig (Roof (decoHeight myDWConfig))

myManageHook = composeAll
    [ manageDocks
     ,className =? "Display" --> doFloat
     -- ,className =? "Steam" --> doFloat
     ,className =? "feh" --> doFloat 
     ,className =? "Eog" --> doFloat
     ,className =? "Visiblity.py" --> doIgnore
     ,className =? "Unity-2d-panel" --> doIgnore
     ,className =? "Unity-2d-shell" --> doIgnore ]

main =
    xmonad $ gnomeConfig
    {
        workspaces = withScreens 2 ["a","b","c","d","e","f","g","h","i"],
        manageHook = myManageHook <+> manageHook gnomeConfig,
        modMask = modMask',
        borderWidth = borderWidth',
        normalBorderColor = normalBorderColor',
        focusedBorderColor = focusedBorderColor',
        layoutHook = myLayoutHook,
        --defaultGaps = defaultGaps',
        terminal = terminal',
        keys = keys'
    }

