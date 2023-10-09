-- SCROLL ON MOUSE MOVEMENT WHEN CTRL BUTTON DOWN
-- inspired by
-- https://superuser.com/questions/303424/can-i-enable-scrolling-with-middle-button-drag-in-os-x
--
-- add to hammerspoon config and hit hs.reload()

local scrollModifier = "ctrl"
local oldMousePosition = {}
local scrollIntensity = 0.5

ctrlDownMouseTracker = hs.eventtap.new({ hs.eventtap.event.types.mouseMoved }, function(e)
	oldMousePosition = hs.mouse.getAbsolutePosition()

	local dX = e:getProperty(hs.eventtap.event.properties['mouseEventDeltaX'])
	local dY = e:getProperty(hs.eventtap.event.properties['mouseEventDeltaY'])

	hs.eventtap.scrollWheel({dX * scrollIntensity, dY * scrollIntensity}, {})
	-- put the mouse back
	hs.mouse.setAbsolutePosition(oldMousePosition)
end)

dragCtrlToScroll = hs.eventtap.new({ hs.eventtap.event.types.flagsChanged }, function(e)
	local modifiers = e:getFlags()
	if (modifiers[scrollModifier]) then
		ctrlDownMouseTracker:start()
	else
		ctrlDownMouseTracker:stop()
	end
end)
dragCtrlToScroll:start()

return dragCtrlToScroll
