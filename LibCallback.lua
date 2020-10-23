local _NAME = "LibCallback"
local _VERSION = "1.0.0"
local _LICENSE = [[
    MIT License

    Copyright (c) 2020 Jayrgo

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
]]

assert(LibMan1, format("%s requires LibMan-1.x.x.", _NAME))

local lib--[[ , oldVersion ]] = LibMan1:New(_NAME, _VERSION, "_LICENSE", _LICENSE)
if not lib then return end

local safecall = lib.safecall
local xsafecall = lib.xsafecall

local tnew, tdel
do -- tnew, tdel
    local cache = setmetatable({}, {__mode = "k"})

    local next = next
    local select = select

    ---@vararg any
    ---@return table 
    function tnew(...)
        local t = next(cache)
        if t then
            cache[t] = nil
            local n = select("#", ...)
            for i = 1, n do t[i] = select(i, ...) end
            return t
        end
        return {...}
    end

    local wipe = wipe

    ---@param t table
    function tdel(t) cache[wipe(t)] = true end
end

local packargs
do -- packargs
    local select = select

    ---@vararg any
    ---@return table
    function packargs(...) return {n = select("#", ...), ...} end
end

local unpackargs
do -- unpackargs
    local unpack = unpack

    ---@param t table
    ---@return any
    function unpackargs(t) return unpack(t, 1, t.n) end
end

local getkey
do -- getkey
    local strhash
    do -- strhash
        local fmod = math.fmod
        local strbyte = strbyte
        local strlen = strlen

        ---@param str string
        ---@return string
        function strhash(str)
            -- Source: https://wow.gamepedia.com/StringHash
            local counter = 1
            local len = strlen(str)
            for i = 1, len, 3 do
                counter = fmod(counter * 8161, 4294967279) + -- 2^32 - 17: Prime!
                (strbyte(str, i) * 16776193) + ((strbyte(str, i + 1) or (len - i + 256)) * 8372226) +
                              ((strbyte(str, i + 2) or (len - i + 256)) * 3932164)
            end
            return fmod(counter, 4294967291) -- 2^32 - 5: Prime (and different from the prime in the loop)
        end
    end

    local getstring
    do -- getstring
        local tostring = tostring

        local prefixes = setmetatable({}, {
            __index = function(t, k)
                local v = tostring(function() end) .. "%s"
                t[k] = v
                return v
            end,
        })

        local format = format
        local type = type

        ---@param str string
        ---@return string
        function getstring(arg) return format(prefixes[type(arg)], tostring(arg)) end
    end

    local select = select
    local tconcat = table.concat

    ---@vararg any
    ---@return string key
    function getkey(...)
        local keys = tnew()
        for i = 1, select("#", ...) do keys[i] = getstring(select(i, ...)) end
        local key = strhash(tconcat(keys))
        tdel(keys)
        return key
    end
end

local select = select

---@param func function
---@vararg any
---@return function
local function getFunc(func, ...)
    if select("#", ...) == 0 then
        return func
    else
        local args = packargs(...)
        return function(...)
            local params = {}
            local argCount = args.n
            for i = 1, argCount do params[i] = args[i] end
            for i = 1, select("#", ...) do
                local n = argCount + i
                params[n] = select(i, ...)
                params.n = n
            end
            func(unpackargs(params))
        end
    end
end

local format = format
local error = error
local pairs = pairs
local tostring = tostring
local type = type

local TriggerEvent, xTriggerEvent
do
    local CopyTable = CopyTable

    ---@param self table
    ---@param event string
    ---@param func function
    ---@vararg any
    local function triggerEvent(self, event, func, ...)
        local events = self.events[event]
        if events then
            -- CopyTable is used, to avoid errors if the table is changed during iter
            for key, callback in pairs(CopyTable(events)) do
                if events[key] == callback then func(callback, ...) end
            end
        end
    end

    ---@param self table
    ---@param event string
    ---@vararg any
    function TriggerEvent(self, event, ...)
        if type(event) ~= "string" then
            error(format("Usage: %s:TriggerEvent(event[, ...]): 'event' - string expected got %s", tostring(lib),
                         type(event)), 2)
        end

        triggerEvent(self, event, safecall, ...)
    end

    ---@param self table
    ---@param event string
    ---@vararg any
    function xTriggerEvent(self, event, ...)
        if type(event) ~= "string" then
            error(format("Usage: %s:xTriggerEvent(event[, ...]): 'event' - string expected got %s", tostring(lib),
                         type(event)), 2)
        end

        triggerEvent(self, event, xsafecall, ...)
    end
end

---@param self table
---@param event string
local function Wipe(self, event)
    if type(event) ~= "string" and type(event) ~= "nil" then
        error(format("Usage: %s:Wipe(event): 'event' - string or nil expected got %s", tostring(lib), type(event)), 2)
    end
    if event then
        if not self.events[event] then return end
        self.events[event] = nil
        safecall(self.OnEventUnregistered, self, event)
    else
        for event in pairs(self.events) do self:Wipe(event) end -- luacheck: ignore 422
    end
end

local next = next

---@param target table
---@param RegisterCallback string
---@param UnregisterCallback string
---@return table
function lib:New(target, RegisterCallback, UnregisterCallback)
    if type(target) ~= "table" then
        error(format(
                  "Usage: %s:New(target[, RegisterCallback[, UnregisterCallback]]): 'target' - table expected got %s",
                  tostring(lib), type(target)), 2)
    end
    RegisterCallback = RegisterCallback and RegisterCallback or "RegisterCallback"
    if type(RegisterCallback) ~= "string" then
        error(format(
                  "Usage: %s:New(target[, RegisterCallback[, UnregisterCallback]]): 'RegisterCallback' - string expected got %s",
                  tostring(lib), type(RegisterCallback)), 2)
    end
    UnregisterCallback = UnregisterCallback and UnregisterCallback or "UnregisterCallback"
    if type(UnregisterCallback) ~= "string" then
        error(format(
                  "Usage: %s:New(target[, RegisterCallback[, UnregisterCallback]]): 'UnregisterCallback' - string expected got %s",
                  tostring(lib), type(UnregisterCallback)), 2)
    end

    local callbacks = {events = {}, TriggerEvent = TriggerEvent, xTriggerEvent = xTriggerEvent, Wipe = Wipe}

    ---@param self table
    ---@param event string
    ---@param callback function | table | string
    ---@vararg any
    target[RegisterCallback] = function(self, event, callback, ...) -- luacheck: ignore 432
        if type(event) ~= "string" then
            error(format("Usage: %s:%s(event[, object], callback[, ...]): 'event' - string expected got %s",
                         tostring(lib), RegisterCallback, type(event)), 2)
        end

        local regKey, regFunc
        if type(callback) == "table" then
            local object, callback = callback, select(1, ...) -- luacheck: ignore 422
            if type(callback) == "function" then
                regKey = getkey(object, callback, ...)
                regFunc = select("#", ...) > 1 and getFunc(callback, select(2, ...)) or getFunc(callback)
            elseif type(callback) == "string" then
                regKey = getkey(object, object[callback], ...)
                regFunc = select("#", ...) > 1 and getFunc(object[callback], object, select(2, ...)) or
                              getFunc(object[callback], object)
            else
                error(format(
                          "Usage: %s:%s(event, object, callback[, ...]): 'callback' - function or string expected got %s",
                          tostring(lib), RegisterCallback, type(callback)), 2)
            end
        elseif type(callback) == "function" then
            regKey = getkey(callback, ...)
            regFunc = getFunc(callback, ...)
        else
            error(format(
                      "Usage: %s:%s(event[, object], callback[, ...]): 'callback' - function or string expected got %s",
                      tostring(lib), RegisterCallback, type(callback)), 2)
        end

        local isFirst
        if not callbacks.events[event] then
            callbacks.events[event] = {}
            isFirst = true
        end
        callbacks.events[event][regKey] = regFunc
        if isFirst then safecall(callbacks.OnEventRegistered, self, event) end
    end

    ---@param self table
    ---@param event string
    ---@param callback function | table | string
    ---@vararg any
    target[UnregisterCallback] = function(self, event, callback, ...) -- luacheck: ignore 432
        if type(event) ~= "string" then
            error(format("Usage: %s:%s(event[, object], callback): 'event' - string expected got %s", tostring(lib),
                         UnregisterCallback, type(event)), 2)
        end

        local regKey
        if type(callback) == "table" then
            local object, callback = callback, select(1, ...) -- luacheck: ignore 422
            if type(callback) == "function" then
                regKey = getkey(object, callback, ...)
            elseif type(callback) == "string" then
                regKey = getkey(object, object[callback], ...)
            else
                error(format(
                          "Usage: %s:%s(event, object, callback[, ...]): 'callback' - function or string expected got %s",
                          tostring(lib), UnregisterCallback, type(callback)), 2)
            end
        elseif type(callback) == "function" then
            regKey = getkey(callback, ...)
        else
            error(format(
                      "Usage: %s:%s(event[, object], callback[, ...]): 'callback' - function or string expected got %s",
                      tostring(lib), UnregisterCallback, type(callback)), 2)
        end

        if callbacks.events[event] then
            callbacks.events[event][regKey] = nil
            if not next(callbacks.events[event]) then
                callbacks.events[event] = nil
                safecall(callbacks.OnEventUnregistered, self, event)
            end
        end
    end

    return callbacks
end
