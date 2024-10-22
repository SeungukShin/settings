local AppRemap = require('app-remap')

local keyboardApple = require('keyboard-apple-internal')
--local keyboardAdvantage2 = require('keyboard-kinesis-advantage2')
local keyboardGlobal = require('keyboard-global')

local mouseMagic = require('mouse-magic-mouse')
local mouseGlobal = require('mouse-global')

local Yabai = require('yabai/yabai')

local log = hs.logger.new("init", "debug")
local appNames = {
	[hs.application.watcher.activated] = hs.application.frontmostApplication():name(),
	[hs.application.watcher.deactivated] = ""
}
local updatedAppNames = {
	[hs.application.watcher.activated] = true,
	[hs.application.watcher.deactivated] = true
}

-- change shift+space to option+space
local changeInputMethod = AppRemap.new("changeInputMethod")
changeInputMethod:addAppName('Emacs')
changeInputMethod:addAppName('Alacritty')
changeInputMethod:addAppName('kitty')
changeInputMethod:addAppName('DCV Viewer')
changeInputMethod:addAppName('Tabby')
changeInputMethod:setContained(false)
changeInputMethod:bind({"shift"}, "space", {"option"}, "space")
changeInputMethod:apply(nil, appNames[hs.application.watcher.activated])

-- change ctrl to cmd for specific keys
local changeCtrlCmd = AppRemap.new("changeCtrlCmd")
changeCtrlCmd:addAppName('DCV Viewer')
changeCtrlCmd:addAppName('Emacs')
changeCtrlCmd:addAppName('JetBrains Client')
changeCtrlCmd:addAppName('Code')
changeCtrlCmd:addAppName('Alacritty')
changeCtrlCmd:addAppName('kitty')
changeCtrlCmd:addAppName('Firefox')
changeCtrlCmd:addAppName('Tabby')
changeCtrlCmd:setContained(false)

--changeCtrlCmd:bind({"ctrl"}, "c", {"cmd"}, "c")
--changeCtrlCmd:bind({"ctrl"}, "x", {"cmd"}, "x")
--changeCtrlCmd:bind({"ctrl"}, "v", {"cmd"}, "v")
--changeCtrlCmd:bind({"ctrl"}, "f", {"cmd"}, "f")
changeCtrlCmd:bind({"ctrl"}, "l", {"cmd"}, "l")
changeCtrlCmd:bind({"ctrl"}, "t", {"cmd"}, "t")
--changeCtrlCmd:bind({"ctrl"}, "w", {"cmd"}, "w")

changeCtrlCmd:bind({"ctrl"}, "p", {""}, "up")
changeCtrlCmd:bind({"ctrl"}, "n", {""}, "down")
changeCtrlCmd:bind({"ctrl"}, "b", {""}, "left")
changeCtrlCmd:bind({"ctrl"}, "f", {""}, "right")
changeCtrlCmd:bind({"option"}, "p", {"option"}, "up")
changeCtrlCmd:bind({"option"}, "n", {"option"}, "down")
changeCtrlCmd:bind({"option"}, "b", {"option"}, "left")
changeCtrlCmd:bind({"option"}, "f", {"option"}, "right")
changeCtrlCmd:bind({"ctrl"}, "v", {""}, "pagedown")
changeCtrlCmd:bind({"option"}, "v", {""}, "pageup")
changeCtrlCmd:bind({"ctrl"}, "a", {"cmd"}, "left")
changeCtrlCmd:bind({"ctrl"}, "e", {"cmd"}, "right")

changeCtrlCmd:bind({"option"}, "w", {"cmd"}, "c")
changeCtrlCmd:bind({"ctrl"}, "w", {"cmd"}, "x")
changeCtrlCmd:bind({"ctrl"}, "y", {"cmd"}, "v")

changeCtrlCmd:bind({"ctrl"}, "d", {""}, "forwarddelete")
changeCtrlCmd:bind({"ctrl"}, "/", {"cmd"}, "z")
changeCtrlCmd:bind({"ctrl"}, "s", {"cmd"}, "f")

changeCtrlCmd:apply(nil, appNames[hs.application.watcher.activated])



local yabai = Yabai:new({'alt'}, 't');

yabai:bind({}, 'escape', function() yabai:clear() end)
yabai:bind({}, 'r', function() yabai:run({'--restart-service'}, function() ReloadConfiguration:start() end); yabai:clear() end)
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




--local item = hs.menubar.new()
--item:setIcon(appImage:bitmapRepresentation({w=16,h=16}, true))
--item:setIcon(appImage:setSize({w=32,h=32}))
--local braket_l = ".###.\n.#...\n.###."
--item:setIcon(hs.image.imageFromASCII(braket_l):setSize({w=64,h=64}))
--app:insertElement({
--      type = "rectangle",
--      action = "fill",
--      frame = {x=0,y=0,w=32,h=32},
--}, 1)


-- alt tab
switcher = hs.window.switcher.new(hs.window.filter.new():setDefaultFilter{},
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
expose = hs.expose.new(nil,
		       {
			  backgroundColor={0.3,0.3,0.3,0.7},
			  highlightColor={0.3,0.3,0.3,0.0},
			  includeOtherSpaces=false,
		       }
)
hs.hotkey.bind('cmd', 'e', function() expose:toggleShow() end)





-- application watcher
function applicationWatch(appName, eventType, appObject)
	if (eventType == hs.application.watcher.activated or eventType == hs.application.watcher.deactivated) then
		appNames[eventType] = appName
		updatedAppNames[eventType] = not updatedAppNames[eventType]
		if (updatedAppNames[hs.application.watcher.activated] == updatedAppNames[hs.application.watcher.deactivated]) then
			changeInputMethod:apply(appNames[hs.application.watcher.deactivated], appNames[hs.application.watcher.activated])
			changeCtrlCmd:apply(appNames[hs.application.watcher.deactivated], appNames[hs.application.watcher.activated])
		end
	end
	--if (eventType == hs.application.watcher.activated) then
	--	if (appName == 'Alacritty' or appName == 'Emacs') then
	--		log.d(appName)
	--		local win = hs.window.focusedWindow()
	--		if (not win:screen():name():find('Built-in', 1, true)) then
	--			win:maximize()
	--		end
	--	elseif (appName == 'AmazonConnections') then
	--		log.d(appName)
	--		appObject:kill()
	--	end
	--end
end
appWatcher = hs.application.watcher.new(applicationWatch)
appWatcher:start()

-- sleep watcher
function sleepWatch(eventType)
	if (eventType == hs.caffeinate.watcher.systemDidWake) then
		log.d('wakeup')
		keyboardApple:register()
		--keyboardAdvantage2:register()
		mouseMagic:register()
	end
end
sleepWatcher = hs.caffeinate.watcher.new(sleepWatch)
sleepWatcher:start()

-- usb watcher
function usbCall()
	log.d('usb timer')
	--keyboardAdvantage2:register()
end

function usbWatch(data)
	if (data['eventType'] == 'added' and data['vendorID'] == 0x29ea and data['productID'] == 0x0102) then
		log.d('usb:', data['productName'])
		usbTimer = hs.timer.delayed.new(1, usbCall)
		usbTimer:start()
	end
end
usbWatcher = hs.usb.watcher.new(usbWatch)
usbWatcher:start()

