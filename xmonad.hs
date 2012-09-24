-- xmonad.hs: XMonad configuration file.
-- Copyright 2009-2011 Tom Vincent <http://www.tlvince.com/contact/>

import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.GridVariants
import XMonad.Layout.SimpleFloat

import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.AppendFile
import XMonad.Prompt.RunOrRaise

-- Data.Ratio for IM layout
import Data.Ratio ((%))
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Tabbed
import XMonad.Layout.SimpleFloat

import Graphics.X11.ExtraTypes.XF86(xF86XK_HomePage,
                                    xF86XK_Search,
                                    xF86XK_Mail,
                                    xF86XK_Launch5,
                                    xF86XK_Launch6,
                                    xF86XK_Launch7,
                                    xF86XK_Launch8,
                                    xF86XK_Launch9,
                                    xF86XK_AudioMute,
                                    xF86XK_AudioLowerVolume,
                                    xF86XK_AudioRaiseVolume,
                                    xF86XK_AudioPlay,
                                    xF86XK_Calculator,
                                    xF86XK_Favorites,
                                    xF86XK_Back,
                                    xF86XK_Forward,
                                    xF86XK_Sleep,
                                    xF86XK_PowerOff)
-- Required for default's
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Applications
    [((modm.|. shiftMask, xK_i),    spawn "xdg-open http://")
    ,((modm.|. shiftMask, xK_b),    spawn "thunderbird")
    ,((modm.|. shiftMask, xK_t),    spawn "~/src/bash/reuse-app.sh pcmanfm pcmanfm")
    ,((modm.|. shiftMask, xK_l),    spawn "leany")
    ,((modm.|. shiftMask, xK_p),    spawn "pidgin")
    ,((modm.|. shiftMask, xK_z),    spawn "uni")
    ,((modm.|. shiftMask, xK_s),    spawn "spotify")
    ,((modm.|. shiftMask, xK_m),    spawn "~/src/bash/dzen-tools/dzen-mpd-now-playing.sh")
    ,((0, xK_Print),                spawn "scrot -q 0 -e 'mv $f ~/pic/computing/screenshots/'")
    ,((mod1Mask, xK_Print),         spawn "scrot -s -q 0 -e 'mv $f ~/pic/computing/screenshots/'")
    ,((modm .|. shiftMask, xK_u),   spawn "urxvtc")

    -- Default window manager functions
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- My window manager functions
    ,((modm, xK_a),                 spawn "sh ~/src/bash/dzen-tasklist.sh")
    ,((modm, xK_c),                 spawn "dzen-time")
    ,((modm, xK_r),                 spawn "dmenu-run")
    ,((modm, xK_m),                 spawn "dmenu-menu")
    ,((modm.|. shiftMask, xK_r),    spawn "dmenu-run-recent")
    ,((modm, xK_s),                 spawn "lock")
    ,((modm .|. shiftMask, xK_f),   goToSelected defaultGSConfig)
    ,((modm, xK_g),                 windowPromptGoto defaultXPConfig)
    ,((modm, xK_f ),                runOrRaisePrompt defaultXPConfig)
    ,((modm .|. shiftMask, xK_n ),  appendFilePrompt defaultXPConfig "/home/tom/doc/notes.mkd")
    ,((modm, xK_v ),                spawn "xclip -selection PRIMARY -o | xclip -selection CLIPBOARD -i")
    ,((modm .|. shiftMask, xK_v ),  spawn "xclip -selection CLIPBOARD -o | xclip -selection PRIMARY -i")

    -- Multimedia Keys
    ,((0, xF86XK_HomePage),         spawn "xdg-open http://")
    ,((0, xF86XK_Search),           spawn "xdg-open http://www.google.co.uk/")
    ,((0, xF86XK_Mail),             spawn "xdg-email")
    ,((0, xF86XK_Launch5),          spawn "sh ~/src/bash/mpd-update.sh")
    ,((0, xF86XK_Launch6),          spawn "sh ~/src/bash/mpd-artist.sh")
    ,((0, xF86XK_Launch7),          spawn "sh ~/src/bash/mpd-playlist.sh")
    ,((0, xF86XK_Launch8),          spawn "mpc prev")
    ,((0, xF86XK_Launch9),          spawn "mpc next")
    ,((0, xF86XK_AudioMute),        spawn "amixer set Master toggle")
    ,((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 2%-")
    ,((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 2%+")
    ,((0, xF86XK_AudioPlay),        spawn "mpc toggle")
    ,((0, xF86XK_Calculator),       spawn "gcalctool")
    ,((0, xF86XK_Favorites),        spawn "~/src/bash/reuse-app.sh pcmanfm pcmanfm")
    ,((0, xF86XK_Sleep),            spawn "sudo s2disk")
    ,((0, xF86XK_PowerOff),         spawn "hslock & sudo s2ram -f")]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((modm .|. mask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e] [0,1]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

myManageHook = composeAll
    [ -- className =? "bluej-Boot"                 --> doShift "5:float"
--    , className =? "bluej-runtime-ExecServer"   --> doShift "5:float"
    className =? "Gcalctool"                  --> doCenterFloat
    , className =?  "Zenity"                    --> doCenterFloat
    , className =? "Xfce4-notifyd"              --> doIgnore
    , title =? "Event Tester"                   --> doFloat
    , className =? "MPlayer"                    --> doFloat
    , resource =? "spotify.exe"                    --> unFloat
    , className =? "Lanikai"                   --> doShift "6:mail"
    , className =? "Digikam"                   --> doShift "6:mail"
    , className =? "Qjackctl"                   --> doShift "7:audio"
    , className =? "Jack_mixer"                 --> doShift "7:audio"
    , className =? "Fst"                        --> doShift "7:audio"
    , className =? "Pidgin"                     --> doShift "8:chat"
    , className =? "Skype"                      --> doShift "8:chat"
    , className =? "Gimp"                       --> doShift "9:gimp"
    , isFullscreen                              --> doFullFloat ]
    <+> manageDocks
        where unFloat = ask >>= doF . W.sink

--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1", "2", "3", "4", "5:float", "6:mail" , "7:audio", "8:chat", "9:gimp"]

-- myLayout = smartBorders (Grid (16/10) ||| tiled ||| Mirror tiled ||| Full ||| simpleFloat)
          -- where
              -- tiled   = Tall nmaster delta ratio
              -- nmaster = 1     -- The default number of windows in the master pane
              -- ratio   = 1/2   -- Default proportion of screen occupied by master pane
              -- delta   = 3/100 -- Percent of screen to increment by when resizing panes

myLayoutHook = onWorkspace "5:float" float $ onWorkspace "6:mail" tabL $ onWorkspace "7:audio" tabL $ onWorkspace "8:chat" chatL $ onWorkspace "9:gimp" gimpL $ standardLayouts
    where
        standardLayouts = smartBorders (Grid (16/10) ||| tiled ||| Mirror tiled ||| Full ||| simpleFloat)

        --Layouts
        tiled = (Tall 1 (3/100) (1/2))

        --Chat Layout
        chatL = smartBorders $ (withIM ratio pidginRoster) $ (withIM ratio skypeRoster) $ Grid (16/10)
        ratio = 1%7
        pidginRoster = And (ClassName "Pidgin") (Role "buddy_list")
        skypeRoster = (Title "tlvince - Skype™ (Beta)")

        --Gimp Layout
        gimpL = smartBorders $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full

        tabL = smartBorders $ simpleTabbed
        float = smartBorders $ simpleFloat

main = xmonad $ defaultConfig {
      -- simple stuff
        terminal            = "~/src/bash/reuse-app.sh urxvtc urxvt",
        modMask             = mod4Mask,
        normalBorderColor   = "#444444",
        focusedBorderColor  = "#94bff3",
        --focusedBorderColor  = "#ff9900",
        workspaces          = myWorkspaces,

      -- Keys
        keys                = myKeys,

      -- hooks, layouts
        manageHook          = myManageHook,
        layoutHook          = myLayoutHook,
        handleEventHook     = ewmhDesktopsEventHook,
        logHook             = ewmhDesktopsLogHook,
        startupHook         = ewmhDesktopsStartup >> setWMName "LG3D"
}
