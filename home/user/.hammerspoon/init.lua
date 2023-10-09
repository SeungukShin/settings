local AppRemap = require('app-remap')

local keyboardApple = require('keyboard-apple-internal')
local keyboardGlobal = require('keyboard-global')

local mouseGlobal = require('mouse-global')

local log = hs.logger.new("init", "debug")
local currAppName = hs.application.frontmostApplication():name()

-- change shift+space to option+space
changeInputMethod = AppRemap.new("changeInputMethod")
changeInputMethod:setAppNames({
	['Emacs'] = true,
	['DCV Viewer'] = true,
})
changeInputMethod:setContained(false)
changeInputMethod:bind({"shift"}, "space", {"option"}, "space")
changeInputMethod:apply(nil, currAppName)

-- change ctrl to cmd for specific keys
changeCtrlCmd = AppRemap.new("changeCtrlCmd")
changeInputMethod:setAppNames({
	['Emacs'] = true,
	['DCV Viewer'] = true,
	['JetBrains Client'] = true,
	['Code'] = true,
	['Alacritty'] = true,
	['kitty'] = true,
})
changeInputMethod:setContained(false)
changeInputMethod:bind({"ctrl"}, "c", {"cmd"}, "c")
changeInputMethod:bind({"ctrl"}, "x", {"cmd"}, "x")
changeInputMethod:bind({"ctrl"}, "v", {"cmd"}, "v")
changeInputMethod:bind({"ctrl"}, "f", {"cmd"}, "f")
changeInputMethod:bind({"ctrl"}, "l", {"cmd"}, "l")
changeInputMethod:bind({"ctrl"}, "t", {"cmd"}, "t")
changeInputMethod:bind({"ctrl"}, "w", {"cmd"}, "w")
changeInputMethod:apply(nil, currAppName)

-- application watcher
appNames = {
	[hs.application.watcher.activated] = "",
	[hs.application.watcher.deactivated] = ""
}
updatedAppNames = {
	[hs.application.watcher.activated] = true,
	[hs.application.watcher.deactivated] = true
}
function applicationWatch(appName, eventType, appObject)
	if (eventType == hs.application.watcher.activated or eventType == hs.application.watcher.deactivated) then
		appNames[eventType] = appName
		updatedAppNames[eventType] = not updatedAppNames[eventType]
		if (updatedAppName[hs.application.watcher.activated] == updatedAppNames[hs.application.watcher.deactivated]) then
			changeInputMethod:apply(appNames[hs.application.watcher.deactivated], appNames[hs.application.watcher.activated])
			changeCtrlCmd:apply(appNames[hs.application.watcher.deactivated], appNames[hs.application.watcher.activated])
		end
	end
	if (eventType == hs.application.watcher.activated and appName == "AmazonConnections") then
		log.d(appName)
		appObject:kill()
	end
end
appWatcher = hs.application.watcher.new(applicationWatch)
appWatcher:start()

-- sleep watcher
function sleepWatch(eventType)
	if (eventType == hs.caffeinate.watcher.systemDidWake) then
		log.d('wakeup')
		keyboardApple:register()
	end
end
sleepWatcher = hs.caffeinate.watcher.new(sleepWatch)
sleepWatcher:start()
