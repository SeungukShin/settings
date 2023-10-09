local log = hs.logger.new("AppRemap", "debug")

CAppRemap = {
	name = "AppRemap",
	version = "1.0",
	author = "Seunguk Shin <seunguk.shin@gmail.com>",
	license = "MIT - https://opensource.org/licenses/MIT",
}

local CAppRemapImpl = {

	setAppNames = function(self, appNames)
		self.appNames = appNames
	end,

	setContained = function(self, isContained)
		self.isContained = isContained
	end,

	bind = function(self, fromMods, fromKey, toMods, toKey)
		self.modal:bind(fromMods, fromKey, nil, function() hs.eventtap.keyStroke(toMods, toKey, 0) end)
	end,

	apply = function(self, prevAppName, currAppName)
		if (prevAppName and self.appNames[prevAppName] == self.appNames[currAppName]) then
			return
		end
		if self.appNames[currAppName] then
			if self.isContained then
				self.modal:enter()
				log.d(self.name .. " (" .. currAppName .. "): enabled")
			else
				self.modal:exit()
				log.d(self.name .. " (" .. currAppName .. "): disabled")
			end
		else
			if self.isContained then
				self.modal:exit()
				log.d(self.name .. " (" .. currAppName .. "): disabled")
			else
				self.modal:enter()
				log.d(self.name .. " (" .. currAppName .. "): enabled")
			end
		end
	end,
}

CAppRemap.new = function(paramName)
	local _self = {
		name = paramName,
		appNames = {},
		isContained = true,
		modal = hs.hotkey.modal.new(),
	}
	setmetatable(_self, {__index = CAppRemapImpl})

	return _self
end

return CAppRemap
