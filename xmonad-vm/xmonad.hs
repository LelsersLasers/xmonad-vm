--XMONAD CONFIG


-- IMPORTS

import XMonad
import Data.Monoid
import System.Exit
import Data.List

import XMonad.Actions.NoBorders

import XMonad.Util.SpawnOnce
import XMonad.Util.Run

import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))

import XMonad.Hooks.EwmhDesktops

import XMonad.Hooks.InsertPosition
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- basic preferences
myTerminal      = "termite"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth   = 3

-- the mod key (alt = mod1Mask)
myModMask       = mod1Mask

-- A tagging example:
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#2E3440"
-- myNormalBorderColor  = "#E7E0E4"
myFocusedBorderColor = "#E5E9F0"
-- myFocusedBorderColor = "#C97799"

------------------------------------------------------------------------
-- KEY BINDINGS
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm              , xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_d     ), spawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_d     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_q     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_Tab ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm             , xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    -- , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_u     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_p     ), sendMessage Expand)
    

    -- Shrink and Expand the Hight of a stacked window
    , ((modm,               xK_i), sendMessage MirrorShrink)
    , ((modm,               xK_o), sendMessage MirrorExpand)
    
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    
    -- Toggle the Borders on windows
    , ((modm              , xK_g ),   withFocused toggleBorder)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_x     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_g     ), spawn "xmonad --recompile; killall xmobar; xmonad --restart")
 
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------

-- window count

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--------------------------------------------------------------------------
-- layout vars

mySpacingRaw i = spacingRaw True (Border i i i i) True (Border i i i i) True
mySpacing i = spacing i
-- The default number of windows in the master pane
nmaster = 1
-- Default proportion of screen occupied by master pane
ratio   = 1/2
-- Percent of screen to increment by when resizing panes
delta   = 4/100

-- LAYOUTS

tilez    = renamed [Replace "tile"] 
         $ mySpacingRaw 20
         $ ResizableTall nmaster delta ratio []

floating = renamed [Replace "floating"]
         $ simpleFloat 

monocle  = renamed [Replace "monocle"]
         $ mySpacing 16
         $ Full

full     = renamed [Replace "full"]
         $ noBorders Full


myLayout = avoidStruts (tilez ||| floating ||| monocle ||| full)


-- ewmhDesktopsStartup :: X ()
-- ewmhDesktopsLogHook :: X ()

 
------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging
myLogHook dest = dynamicLogWithPP defaultPP 
    { ppOutput = hPutStrLn dest
    -- , ppVisible = wrap "(" ")"
    , ppCurrent = xmobarColor "#EBCB8B" "" . wrap "[" "]" -- Current workspace in xmobar
    , ppVisible = xmobarColor "#BF616A" ""                -- Visible but not current workspace
    , ppHidden = xmobarColor "#A3BE8C" "" . wrap "(" ")"   -- Hidden workspaces in xmobar
    , ppHiddenNoWindows = xmobarColor "#5E81AC" ""        -- Hidden workspaces (no windows)
    , ppExtras  = [windowCount]                         -- displays number of open window
    , ppTitle = xmobarColor "#D8DEE9" "" . shorten 120     -- Title of active window in xmobar
    , ppSep =  " | "                                      -- Separators in xmobar
    , ppUrgent = xmobarColor "#BF616A" "" . wrap "!" "!"
    , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
    }


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.

myStartupHook = do
  -- spawnOnce "wal -i /home/millankumar/Downloads/0112.jpg -b #2E3440"
  spawnOnce "nitrogen --restore"
  spawnOnce "picom --round-borders 1 &"
  spawnOnce "/usr/lib/polkit-kde-authentication-agent-1 &"

------------------------------------------------------------------------

myXmobarBar = "xmobar /home/millankumar/.config/xmobar/xmobarrc-save"

colorGrayAlt = "#333333"

-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.

main = do 
  xmproc <- spawnPipe myXmobarBar
  xmonad $ docks $ withUrgencyHook NoUrgencyHook $ defaults xmproc


-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.

defaults xmproc = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = insertPosition Below Newer <+> myManageHook,
        handleEventHook    = myEventHook,
       
        logHook            = myLogHook xmproc,
       
        startupHook        = myStartupHook
    }

