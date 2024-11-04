local AppRemap = require('app-remap')

local Yabai = require('yabai/yabai')

local log = hs.logger.new("init", "debug")


local keyboardApple = require('keyboard-apple-internal')
--local keyboardAdvantage2 = require('keyboard-kinesis-advantage2')
local keyboardGlobal = require('keyboard-global')
local mouseMagic = require('mouse-magic-mouse')
local mouseGlobal = require('mouse-global')




-- change shift+space to option+space
local remapIME = AppRemap:new('remapIME')
remapIME:setAppNames({'Emacs', 'Alacritty', 'kitty', 'DCV Viewer', 'Tabby'})
remapIME:setContained(false)

remapIME:bind({'shift'}, 'space', {'option'}, 'space')

remapIME:start()


-- change ctrl to cmd for specific keys
local remapCtrlCmd = AppRemap:new('remapCtrlCmd')
remapCtrlCmd:setAppNames({'DCV Viewer', 'Emacs', 'JetBrains Client', 'Code',
			  'Alacritty', 'kitty', 'Firefox', 'Tabby'})
remapCtrlCmd:setContained(false)

--remapCtrlCmd:bind({'ctrl'}, 'c', {'cmd'}, 'c')
--remapCtrlCmd:bind({'ctrl'}, 'x', {'cmd'}, 'x')
--remapCtrlCmd:bind({'ctrl'}, 'v', {'cmd'}, 'v')
--remapCtrlCmd:bind({'ctrl'}, 'f', {'cmd'}, 'f')
remapCtrlCmd:bind({'ctrl'}, 'l', {'cmd'}, 'l')
remapCtrlCmd:bind({'ctrl'}, 't', {'cmd'}, 't')
--remapCtrlCmd:bind({'ctrl'}, 'w', {'cmd'}, 'w')

remapCtrlCmd:bind({'ctrl'}, 'p', {''}, 'up')
remapCtrlCmd:bind({'ctrl'}, 'n', {''}, 'down')
remapCtrlCmd:bind({'ctrl'}, 'b', {''}, 'left')
remapCtrlCmd:bind({'ctrl'}, 'f', {''}, 'right')
remapCtrlCmd:bind({'option'}, 'p', {'option'}, 'up')
remapCtrlCmd:bind({'option'}, 'n', {'option'}, 'down')
remapCtrlCmd:bind({'option'}, 'b', {'option'}, 'left')
remapCtrlCmd:bind({'option'}, 'f', {'option'}, 'right')
remapCtrlCmd:bind({'ctrl'}, 'v', {''}, 'pagedown')
remapCtrlCmd:bind({'option'}, 'v', {''}, 'pageup')
remapCtrlCmd:bind({'ctrl'}, 'a', {'cmd'}, 'left')
remapCtrlCmd:bind({'ctrl'}, 'e', {'cmd'}, 'right')

remapCtrlCmd:bind({'option'}, 'w', {'cmd'}, 'c')
remapCtrlCmd:bind({'ctrl'}, 'w', {'cmd'}, 'x')
remapCtrlCmd:bind({'ctrl'}, 'y', {'cmd'}, 'v')

remapCtrlCmd:bind({'ctrl'}, 'd', {''}, 'forwarddelete')
remapCtrlCmd:bind({'ctrl'}, '/', {'cmd'}, 'z')
remapCtrlCmd:bind({'ctrl'}, 's', {'cmd'}, 'f')

remapCtrlCmd:start()
--remapCtrlCmd:apply(nil, appNames[hs.application.watcher.activated])


-- yabai
local yabai = Yabai:new({'alt'}, 't');
yabai:addPlaceForApp('Slack', 2)
yabai:addPlaceForApp('Alacritty', 3)
yabai:addPlaceForApp('Firefox', 4)
yabai:addPlaceForApp('Orion', 5)
yabai:addPlaceForApp('Logseq', 6)
yabai:addPlaceForApp('Microsoft Teams', 7)
yabai:addPlaceForApp('Microsoft Outlook', 8)
yabai:addPlaceForApp('Emacs', 9)
yabai:replaceApps()

yabai:bind({}, 'escape', function() yabai:clear() end)
yabai:bind({}, 'r', function() yabai:run({'--restart-service'}, function()
	ReloadConfiguration:start() end); yabai:clear() end)
yabai:bind({}, 's', function() yabai:replaceApps(); yabai:clear() end)
yabai:bind({}, 't', function() yabai:focusPreviousWindow(nil); yabai:clear() end)

yabai:bind({}, 'c', function() yabai:createSpace(nil); yabai:clear() end)
yabai:bind({}, 'k', function() yabai:destroyActiveSpace(nil); yabai:clear() end)

yabai:bind({}, 'm', function() yabai:toggleFullscreen(nil); yabai:clear() end)

yabai:bind({}, 'h', function() yabai:focusWindowPos('west', nil); yabai:clear() end)
yabai:bind({}, 'j', function() yabai:focusWindowPos('south', nil); yabai:clear() end)
yabai:bind({}, 'k', function() yabai:focusWindowPos('north', nil); yabai:clear() end)
yabai:bind({}, 'l', function() yabai:focusWindowPos('east', nil); yabai:clear() end)

yabai:bind({'alt'}, 'h', function() yabai:focusDisplayPos('west', nil); yabai:clear() end)
yabai:bind({'alt'}, 'j', function() yabai:focusDisplayPos('south', nil); yabai:clear() end)
yabai:bind({'alt'}, 'k', function() yabai:focusDisplayPos('north', nil); yabai:clear() end)
yabai:bind({'alt'}, 'l', function() yabai:focusDisplayPos('east', nil); yabai:clear() end)

for i = 1, 10 do
  local key = tostring(i)
  if i > 9 then key = '0' end
  yabai:bind({}, key, function() yabai:showSpaceInActiveDisplay(i, nil); yabai:clear() end)
  yabai:bind({'shift'}, key, function() yabai:sendActiveWindowToSpace(i, nil); yabai:clear() end)
  yabai:bind({'alt'}, key, function() yabai:focusDisplay(i, nil); yabai:clear() end)
end


-- switcher
local switcher = hs.window.switcher.new(hs.window.filter.new():setDefaultFilter{},
					{
					  textSize=10,
					  selectedThumbnailSize=256,
					  backgroundColor={0.3,0.3,0.3,0.7},
					  highlightColor={0.3,0.3,0.3,0.0},
					  titleBackgroundColor={0.3,0.3,0.3,0.0},
					}
)
hs.hotkey.bind('alt', 'tab', function() switcher:next() end)
hs.hotkey.bind('alt-shift', 'tab', function() switcher:previous() end)


-- expose
local expose = hs.expose.new(nil,
		       {
			  backgroundColor={0.3,0.3,0.3,0.7},
			  highlightColor={0.3,0.3,0.3,0.0},
			  includeOtherSpaces=false,
		       }
)
hs.hotkey.bind('cmd', 'e', function() expose:toggleShow() end)


-- application watcher
function applicationWatch(appName, eventType, appObject)
  -- arrange or kill the app
  if (eventType == hs.application.watcher.activated) then
    if (appName == 'Alacritty' or appName == 'Emacs') then
      log.d(appName)
      local win = hs.window.focusedWindow()
      if (not win:screen():name():find('Built-in', 1, true)) then
  	win:maximize()
      end
    elseif (appName == 'AmazonConnections') then
      log.d(appName)
      appObject:kill()
    end
  end
end
--local appWatcher = hs.application.watcher.new(applicationWatch)
--appWatcher:start()


-- sleep watcher
function sleepWatch(eventType)
  if (eventType == hs.caffeinate.watcher.systemDidWake) then
    keyboardApple:register()
    --keyboardAdvantage2:register()
    mouseMagic:register()
  end
end
local sleepWatcher = hs.caffeinate.watcher.new(sleepWatch)
sleepWatcher:start()


-- usb watcher
function usbCall()
  keyboardAdvantage2:register()
end

function usbWatch(data)
  if (data['eventType'] == 'added' and data['vendorID'] == 0x29ea and data['productID'] == 0x0102) then
    log.d('usb:', data['productName'])
    usbTimer = hs.timer.delayed.new(1, usbCall)
    usbTimer:start()
  end
end
--local usbWatcher = hs.usb.watcher.new(usbWatch)
--usbWatcher:start()
