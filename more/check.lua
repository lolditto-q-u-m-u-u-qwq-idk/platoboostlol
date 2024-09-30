local always_use_lpeg = false
  local register_global_module_table = false
  local global_module_name = 'json'



  -- global dependencies:
  local pairs, type, tostring, tonumber, getmetatable, setmetatable, rawset =
  pairs, type, tostring, tonumber, getmetatable, setmetatable, rawset
  local error, require, pcall, select = error, require, pcall, select
  local floor, huge = math.floor, math.huge
  local strrep, gsub, strsub, strbyte, strchar, strfind, strlen, strformat =
  string.rep, string.gsub, string.sub, string.byte, string.char,
  string.find, string.len, string.format
  local strmatch = string.match
  local concat = table.concat

  local json = { version = "dkjson 2.6" }

  local jsonlpeg = {}

  if register_global_module_table then
    if always_use_lpeg then
      _G[global_module_name] = jsonlpeg
     else
      _G[global_module_name] = json
    end
  end

  ---local _ENV = nil -- blocking globals in Lua 5.2 and later

  pcall (function()
    -- Enable access to blocked metatables.
    -- Don't worry, this module doesn't change anything in them.
    local debmeta = require "debug".getmetatable
    if debmeta then getmetatable = debmeta end
  end)

  json.null = setmetatable ({}, {
    __tojson = function () return "null" end
  })

  local function isarray (tbl)
    local max, n, arraylen = 0, 0, 0
    for k,v in pairs (tbl) do
      if k == 'n' and type(v) == 'number' then
        arraylen = v
        if v > max then
          max = v
        end
       else
        if type(k) ~= 'number' or k < 1 or floor(k) ~= k then
          return false
        end
        if k > max then
          max = k
        end
        n = n + 1
      end
    end
    if max > 10 and max > arraylen and max > n * 2 then
      return false -- don't create an array with too many holes
    end
    return true, max
  end

  local escapecodes = {
    ["\""] = "\\\"", ["\\"] = "\\\\", ["\b"] = "\\b", ["\f"] = "\\f",
    ["\n"] = "\\n", ["\r"] = "\\r", ["\t"] = "\\t"
  }

  local function escapeutf8 (uchar)
    local value = escapecodes[uchar]
    if value then
      return value
    end
    local a, b, c, d = strbyte (uchar, 1, 4)
    a, b, c, d = a or 0, b or 0, c or 0, d or 0
    if a <= 0x7f then
      value = a
     elseif 0xc0 <= a and a <= 0xdf and b >= 0x80 then
      value = (a - 0xc0) * 0x40 + b - 0x80
     elseif 0xe0 <= a and a <= 0xef and b >= 0x80 and c >= 0x80 then
      value = ((a - 0xe0) * 0x40 + b - 0x80) * 0x40 + c - 0x80
     elseif 0xf0 <= a and a <= 0xf7 and b >= 0x80 and c >= 0x80 and d >= 0x80 then
      value = (((a - 0xf0) * 0x40 + b - 0x80) * 0x40 + c - 0x80) * 0x40 + d - 0x80
     else
      return ""
    end
    if value <= 0xffff then
      return strformat ("\\u%.4x", value)
     elseif value <= 0x10ffff then
      -- encode as UTF-16 surrogate pair
      value = value - 0x10000
      local highsur, lowsur = 0xD800 + floor (value/0x400), 0xDC00 + (value % 0x400)
      return strformat ("\\u%.4x\\u%.4x", highsur, lowsur)
     else
      return ""
    end
  end

  local function fsub (str, pattern, repl)
    -- gsub always builds a new string in a buffer, even when no match
    -- exists. First using find should be more efficient when most strings
    -- don't contain the pattern.
    if strfind (str, pattern) then
      return gsub (str, pattern, repl)
     else
      return str
    end
  end

  local function quotestring (value)
    -- based on the regexp "escapable" in https://github.com/douglascrockford/JSON-js
    value = fsub (value, "[%z\1-\31\"\\\127]", escapeutf8)
    if strfind (value, "[\194\216\220\225\226\239]") then
      value = fsub (value, "\194[\128-\159\173]", escapeutf8)
      value = fsub (value, "\216[\128-\132]", escapeutf8)
      value = fsub (value, "\220\143", escapeutf8)
      value = fsub (value, "\225\158[\180\181]", escapeutf8)
      value = fsub (value, "\226\128[\140-\143\168-\175]", escapeutf8)
      value = fsub (value, "\226\129[\160-\175]", escapeutf8)
      value = fsub (value, "\239\187\191", escapeutf8)
      value = fsub (value, "\239\191[\176-\191]", escapeutf8)
    end
    return "\"" .. value .. "\""
  end
  json.quotestring = quotestring

  local function replace(str, o, n)
    local i, j = strfind (str, o, 1, true)
    if i then
      return strsub(str, 1, i-1) .. n .. strsub(str, j+1, -1)
     else
      return str
    end
  end

  -- locale independent num2str and str2num functions
  local decpoint, numfilter

  local function updatedecpoint ()
    decpoint = strmatch(tostring(0.5), "([^05+])")
    -- build a filter that can be used to remove group separators
    numfilter = "[^0-9%-%+eE" .. gsub(decpoint, "[%^%$%(%)%%%.%[%]%*%+%-%?]", "%%%0") .. "]+"
  end

  updatedecpoint()

  local function num2str (num)
    return replace(fsub(tostring(num), numfilter, ""), decpoint, ".")
  end

  local function str2num (str)
    local num = tonumber(replace(str, ".", decpoint))
    if not num then
      updatedecpoint()
      num = tonumber(replace(str, ".", decpoint))
    end
    return num
  end

  local function addnewline2 (level, buffer, buflen)
    buffer[buflen+1] = "\n"
    buffer[buflen+2] = strrep ("  ", level)
    buflen = buflen + 2
    return buflen
  end

  function json.addnewline (state)
    if state.indent then
      state.bufferlen = addnewline2 (state.level or 0,
      state.buffer, state.bufferlen or #(state.buffer))
    end
  end

  local encode2 -- forward declaration

  local function addpair (key, value, prev, indent, level, buffer, buflen, tables, globalorder, state)
    local kt = type (key)
    if kt ~= 'string' and kt ~= 'number' then
      return nil, "type '" .. kt .. "' is not supported as a key by JSON."
    end
    if prev then
      buflen = buflen + 1
      buffer[buflen] = ","
    end
    if indent then
      buflen = addnewline2 (level, buffer, buflen)
    end
    buffer[buflen+1] = quotestring (key)
    buffer[buflen+2] = ":"
    return encode2 (value, indent, level, buffer, buflen + 2, tables, globalorder, state)
  end

  local function appendcustom(res, buffer, state)
    local buflen = state.bufferlen
    if type (res) == 'string' then
      buflen = buflen + 1
      buffer[buflen] = res
    end
    return buflen
  end

  local function exception(reason, value, state, buffer, buflen, defaultmessage)
    defaultmessage = defaultmessage or reason
    local handler = state.exception
    if not handler then
      return nil, defaultmessage
     else
      state.bufferlen = buflen
      local ret, msg = handler (reason, value, state, defaultmessage)
      if not ret then return nil, msg or defaultmessage end
      return appendcustom(ret, buffer, state)
    end
  end

  function json.encodeexception(reason, value, state, defaultmessage)
    return quotestring("<" .. defaultmessage .. ">")
  end

  encode2 = function (value, indent, level, buffer, buflen, tables, globalorder, state)
    local valtype = type (value)
    local valmeta = getmetatable (value)
    valmeta = type (valmeta) == 'table' and valmeta -- only tables
    local valtojson = valmeta and valmeta.__tojson
    if valtojson then
      if tables[value] then
        return exception('reference cycle', value, state, buffer, buflen)
      end
      tables[value] = true
      state.bufferlen = buflen
      local ret, msg = valtojson (value, state)
      if not ret then return exception('custom encoder failed', value, state, buffer, buflen, msg) end
      tables[value] = nil
      buflen = appendcustom(ret, buffer, state)
     elseif value == nil then
      buflen = buflen + 1
      buffer[buflen] = "null"
     elseif valtype == 'number' then
      local s
      if value ~= value or value >= huge or -value >= huge then
        -- This is the behaviour of the original JSON implementation.
        s = "null"
       else
        s = num2str (value)
      end
      buflen = buflen + 1
      buffer[buflen] = s
     elseif valtype == 'boolean' then
      buflen = buflen + 1
      buffer[buflen] = value and "true" or "false"
     elseif valtype == 'string' then
      buflen = buflen + 1
      buffer[buflen] = quotestring (value)
     elseif valtype == 'table' then
      if tables[value] then
        return exception('reference cycle', value, state, buffer, buflen)
      end
      tables[value] = true
      level = level + 1
      local isa, n = isarray (value)
      if n == 0 and valmeta and valmeta.__jsontype == 'object' then
        isa = false
      end
      local msg
      if isa then -- JSON array
        buflen = buflen + 1
        buffer[buflen] = "["
        for i = 1, n do
          buflen, msg = encode2 (value[i], indent, level, buffer, buflen, tables, globalorder, state)
          if not buflen then return nil, msg end
          if i < n then
            buflen = buflen + 1
            buffer[buflen] = ","
          end
        end
        buflen = buflen + 1
        buffer[buflen] = "]"
       else -- JSON object
        local prev = false
        buflen = buflen + 1
        buffer[buflen] = "{"
        local order = valmeta and valmeta.__jsonorder or globalorder
        if order then
          local used = {}
          n = #order
          for i = 1, n do
            local k = order[i]
            local v = value[k]
            if v ~= nil then
              used[k] = true
              buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
              prev = true -- add a seperator before the next element
            end
          end
          for k,v in pairs (value) do
            if not used[k] then
              buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
              if not buflen then return nil, msg end
              prev = true -- add a seperator before the next element
            end
          end
         else -- unordered
          for k,v in pairs (value) do
            buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
            if not buflen then return nil, msg end
            prev = true -- add a seperator before the next element
          end
        end
        if indent then
          buflen = addnewline2 (level - 1, buffer, buflen)
        end
        buflen = buflen + 1
        buffer[buflen] = "}"
      end
      tables[value] = nil
     else
      return exception ('unsupported type', value, state, buffer, buflen,
      "type '" .. valtype .. "' is not supported by JSON.")
    end
    return buflen
  end

  function json.encode (value, state)
    state = state or {}
    local oldbuffer = state.buffer
    local buffer = oldbuffer or {}
    state.buffer = buffer
    updatedecpoint()
    local ret, msg = encode2 (value, state.indent, state.level or 0,
    buffer, state.bufferlen or 0, state.tables or {}, state.keyorder, state)
    if not ret then
      error (msg, 2)
     elseif oldbuffer == buffer then
      state.bufferlen = ret
      return true
     else
      state.bufferlen = nil
      state.buffer = nil
      return concat (buffer)
    end
  end

  local function loc (str, where)
    local line, pos, linepos = 1, 1, 0
    while true do
      pos = strfind (str, "\n", pos, true)
      if pos and pos < where then
        line = line + 1
        linepos = pos
        pos = pos + 1
       else
        break
      end
    end
    return "line " .. line .. ", column " .. (where - linepos)
  end

  local function unterminated (str, what, where)
    return nil, strlen (str) + 1, "unterminated " .. what .. " at " .. loc (str, where)
  end

  local function scanwhite (str, pos)
    while true do
      pos = strfind (str, "%S", pos)
      if not pos then return nil end
      local sub2 = strsub (str, pos, pos + 1)
      if sub2 == "\239\187" and strsub (str, pos + 2, pos + 2) == "\191" then
        -- UTF-8 Byte Order Mark
        pos = pos + 3
       elseif sub2 == "//" then
        pos = strfind (str, "[\n\r]", pos + 2)
        if not pos then return nil end
       elseif sub2 == "/*" then
        pos = strfind (str, "*/", pos + 2)
        if not pos then return nil end
        pos = pos + 2
       else
        return pos
      end
    end
  end

  local escapechars = {
    ["\""] = "\"", ["\\"] = "\\", ["/"] = "/", ["b"] = "\b", ["f"] = "\f",
    ["n"] = "\n", ["r"] = "\r", ["t"] = "\t"
  }

  local function unichar (value)
    if value < 0 then
      return nil
     elseif value <= 0x007f then
      return strchar (value)
     elseif value <= 0x07ff then
      return strchar (0xc0 + floor(value/0x40),
      0x80 + (floor(value) % 0x40))
     elseif value <= 0xffff then
      return strchar (0xe0 + floor(value/0x1000),
      0x80 + (floor(value/0x40) % 0x40),
      0x80 + (floor(value) % 0x40))
     elseif value <= 0x10ffff then
      return strchar (0xf0 + floor(value/0x40000),
      0x80 + (floor(value/0x1000) % 0x40),
      0x80 + (floor(value/0x40) % 0x40),
      0x80 + (floor(value) % 0x40))
     else
      return nil
    end
  end

  local function scanstring (str, pos)
    local lastpos = pos + 1
    local buffer, n = {}, 0
    while true do
      local nextpos = strfind (str, "[\"\\]", lastpos)
      if not nextpos then
        return unterminated (str, "string", pos)
      end
      if nextpos > lastpos then
        n = n + 1
        buffer[n] = strsub (str, lastpos, nextpos - 1)
      end
      if strsub (str, nextpos, nextpos) == "\"" then
        lastpos = nextpos + 1
        break
       else
        local escchar = strsub (str, nextpos + 1, nextpos + 1)
        local value
        if escchar == "u" then
          value = tonumber (strsub (str, nextpos + 2, nextpos + 5), 16)
          if value then
            local value2
            if 0xD800 <= value and value <= 0xDBff then
              -- we have the high surrogate of UTF-16. Check if there is a
              -- low surrogate escaped nearby to combine them.
              if strsub (str, nextpos + 6, nextpos + 7) == "\\u" then
                value2 = tonumber (strsub (str, nextpos + 8, nextpos + 11), 16)
                if value2 and 0xDC00 <= value2 and value2 <= 0xDFFF then
                  value = (value - 0xD800) * 0x400 + (value2 - 0xDC00) + 0x10000
                 else
                  value2 = nil -- in case it was out of range for a low surrogate
                end
              end
            end
            value = value and unichar (value)
            if value then
              if value2 then
                lastpos = nextpos + 12
               else
                lastpos = nextpos + 6
              end
            end
          end
        end
        if not value then
          value = escapechars[escchar] or escchar
          lastpos = nextpos + 2
        end
        n = n + 1
        buffer[n] = value
      end
    end
    if n == 1 then
      return buffer[1], lastpos
     elseif n > 1 then
      return concat (buffer), lastpos
     else
      return "", lastpos
    end
  end

  local scanvalue -- forward declaration

  local function scantable (what, closechar, str, startpos, nullval, objectmeta, arraymeta)
    local len = strlen (str)
    local tbl, n = {}, 0
    local pos = startpos + 1
    if what == 'object' then
      setmetatable (tbl, objectmeta)
     else
      setmetatable (tbl, arraymeta)
    end
    while true do
      pos = scanwhite (str, pos)
      if not pos then return unterminated (str, what, startpos) end
      local char = strsub (str, pos, pos)
      if char == closechar then
        return tbl, pos + 1
      end
      local val1, err
      val1, pos, err = scanvalue (str, pos, nullval, objectmeta, arraymeta)
      if err then return nil, pos, err end
      pos = scanwhite (str, pos)
      if not pos then return unterminated (str, what, startpos) end
      char = strsub (str, pos, pos)
      if char == ":" then
        if val1 == nil then
          return nil, pos, "cannot use nil as table index (at " .. loc (str, pos) .. ")"
        end
        pos = scanwhite (str, pos + 1)
        if not pos then return unterminated (str, what, startpos) end
        local val2
        val2, pos, err = scanvalue (str, pos, nullval, objectmeta, arraymeta)
        if err then return nil, pos, err end
        tbl[val1] = val2
        pos = scanwhite (str, pos)
        if not pos then return unterminated (str, what, startpos) end
        char = strsub (str, pos, pos)
       else
        n = n + 1
        tbl[n] = val1
      end
      if char == "," then
        pos = pos + 1
      end
    end
  end

  scanvalue = function (str, pos, nullval, objectmeta, arraymeta)
    pos = pos or 1
    pos = scanwhite (str, pos)
    if not pos then
      return nil, strlen (str) + 1, "no valid JSON value (reached the end)"
    end
    local char = strsub (str, pos, pos)
    if char == "{" then
      return scantable ('object', "}", str, pos, nullval, objectmeta, arraymeta)
     elseif char == "[" then
      return scantable ('array', "]", str, pos, nullval, objectmeta, arraymeta)
     elseif char == "\"" then
      return scanstring (str, pos)
     else
      local pstart, pend = strfind (str, "^%-?[%d%.]+[eE]?[%+%-]?%d*", pos)
      if pstart then
        local number = str2num (strsub (str, pstart, pend))
        if number then
          return number, pend + 1
        end
      end
      pstart, pend = strfind (str, "^%a%w*", pos)
      if pstart then
        local name = strsub (str, pstart, pend)
        if name == "true" then
          return true, pend + 1
         elseif name == "false" then
          return false, pend + 1
         elseif name == "null" then
          return nullval, pend + 1
        end
      end
      return nil, pos, "no valid JSON value at " .. loc (str, pos)
    end
  end

  local function optionalmetatables(...)
    if select("#", ...) > 0 then
      return ...
     else
      return {__jsontype = 'object'}, {__jsontype = 'array'}
    end
  end

  function json.decode (str, pos, nullval, ...)
    local objectmeta, arraymeta = optionalmetatables(...)
    return scanvalue (str, pos, nullval, objectmeta, arraymeta)
  end

  function json.use_lpeg ()
    local g = require ("lpeg")

    if g.version() == "0.11" then
      error "due to a bug in LPeg 0.11, it cannot be used for JSON matching"
    end

    local pegmatch = g.match
    local P, S, R = g.P, g.S, g.R

    local function ErrorCall (str, pos, msg, state)
      if not state.msg then
        state.msg = msg .. " at " .. loc (str, pos)
        state.pos = pos
      end
      return false
    end

    local function Err (msg)
      return g.Cmt (g.Cc (msg) * g.Carg (2), ErrorCall)
    end

    local function ErrorUnterminatedCall (str, pos, what, state)
      return ErrorCall (str, pos - 1, "unterminated " .. what, state)
    end

    local SingleLineComment = P"//" * (1 - S"\n\r")^0
    local MultiLineComment = P"/*" * (1 - P"*/")^0 * P"*/"
    local Space = (S" \n\r\t" + P"\239\187\191" + SingleLineComment + MultiLineComment)^0

    local function ErrUnterminated (what)
      return g.Cmt (g.Cc (what) * g.Carg (2), ErrorUnterminatedCall)
    end

    local PlainChar = 1 - S"\"\\\n\r"
    local EscapeSequence = (P"\\" * g.C (S"\"\\/bfnrt" + Err "unsupported escape sequence")) / escapechars
    local HexDigit = R("09", "af", "AF")
    local function UTF16Surrogate (match, pos, high, low)
      high, low = tonumber (high, 16), tonumber (low, 16)
      if 0xD800 <= high and high <= 0xDBff and 0xDC00 <= low and low <= 0xDFFF then
        return true, unichar ((high - 0xD800) * 0x400 + (low - 0xDC00) + 0x10000)
       else
        return false
      end
    end
    local function UTF16BMP (hex)
      return unichar (tonumber (hex, 16))
    end
    local U16Sequence = (P"\\u" * g.C (HexDigit * HexDigit * HexDigit * HexDigit))
    local UnicodeEscape = g.Cmt (U16Sequence * U16Sequence, UTF16Surrogate) + U16Sequence/UTF16BMP
    local Char = UnicodeEscape + EscapeSequence + PlainChar
    local String = P"\"" * (g.Cs (Char ^ 0) * P"\"" + ErrUnterminated "string")
    local Integer = P"-"^(-1) * (P"0" + (R"19" * R"09"^0))
    local Fractal = P"." * R"09"^0
    local Exponent = (S"eE") * (S"+-")^(-1) * R"09"^1
    local Number = (Integer * Fractal^(-1) * Exponent^(-1))/str2num
    local Constant = P"true" * g.Cc (true) + P"false" * g.Cc (false) + P"null" * g.Carg (1)
    local SimpleValue = Number + String + Constant
    local ArrayContent, ObjectContent

    -- The functions parsearray and parseobject parse only a single value/pair
    -- at a time and store them directly to avoid hitting the LPeg limits.
    local function parsearray (str, pos, nullval, state)
      local obj, cont
      local start = pos
      local npos
      local t, nt = {}, 0
      repeat
        obj, cont, npos = pegmatch (ArrayContent, str, pos, nullval, state)
        if cont == 'end' then
          return ErrorUnterminatedCall (str, start, "array", state)
        end
        pos = npos
        if cont == 'cont' or cont == 'last' then
          nt = nt + 1
          t[nt] = obj
        end
      until cont ~= 'cont'
      return pos, setmetatable (t, state.arraymeta)
    end

    local function parseobject (str, pos, nullval, state)
      local obj, key, cont
      local start = pos
      local npos
      local t = {}
      repeat
        key, obj, cont, npos = pegmatch (ObjectContent, str, pos, nullval, state)
        if cont == 'end' then
          return ErrorUnterminatedCall (str, start, "object", state)
        end
        pos = npos
        if cont == 'cont' or cont == 'last' then
          t[key] = obj
        end
      until cont ~= 'cont'
      return pos, setmetatable (t, state.objectmeta)
    end

    local Array = P"[" * g.Cmt (g.Carg(1) * g.Carg(2), parsearray)
    local Object = P"{" * g.Cmt (g.Carg(1) * g.Carg(2), parseobject)
    local Value = Space * (Array + Object + SimpleValue)
    local ExpectedValue = Value + Space * Err "value expected"
    local ExpectedKey = String + Err "key expected"
    local End = P(-1) * g.Cc'end'
    local ErrInvalid = Err "invalid JSON"
    ArrayContent = (Value * Space * (P"," * g.Cc'cont' + P"]" * g.Cc'last'+ End + ErrInvalid) + g.Cc(nil) * (P"]" * g.Cc'empty' + End + ErrInvalid)) * g.Cp()
    local Pair = g.Cg (Space * ExpectedKey * Space * (P":" + Err "colon expected") * ExpectedValue)
    ObjectContent = (g.Cc(nil) * g.Cc(nil) * P"}" * g.Cc'empty' + End + (Pair * Space * (P"," * g.Cc'cont' + P"}" * g.Cc'last' + End + ErrInvalid) + ErrInvalid)) * g.Cp()
    local DecodeValue = ExpectedValue * g.Cp ()

    jsonlpeg.version = json.version
    jsonlpeg.encode = json.encode
    jsonlpeg.null = json.null
    jsonlpeg.quotestring = json.quotestring
    jsonlpeg.addnewline = json.addnewline
    jsonlpeg.encodeexception = json.encodeexception
    jsonlpeg.using_lpeg = true

    function jsonlpeg.decode (str, pos, nullval, ...)
      local state = {}
      state.objectmeta, state.arraymeta = optionalmetatables(...)
      local obj, retpos = pegmatch (DecodeValue, str, pos, nullval, state)
      if state.msg then
        return nil, state.pos, state.msg
       else
        return obj, retpos
      end
    end

    -- cache result of this function:
    json.use_lpeg = function () return jsonlpeg end
    jsonlpeg.use_lpeg = json.use_lpeg

    return jsonlpeg
  end

  if always_use_lpeg then
    return json.use_lpeg()
  end






  module = {}

  -- bitshift functions (<<, >> equivalent)
  -- shift left
  function lsh(value, shift)
    return (value * (2 ^ shift)) % 256
  end

  -- shift right
  function rsh(value, shift)
    return math.floor(value / 2 ^ shift) % 256
  end

  -- return single bit (for OR)
  function bit(x, b)
    return (x % 2 ^ b - x % 2 ^ (b - 1) > 0)
  end

  -- logic OR for number values
  function lor(x, y)
    result = 0
    for p = 1, 8 do
      result = result + (((bit(x, p) or bit(y, p)) == true) and 2 ^ (p - 1) or 0)
    end
    return result
  end

  -- encryption table
  local base64chars = {
    [0] = "A",
    [1] = "B",
    [2] = "C",
    [3] = "D",
    [4] = "E",
    [5] = "F",
    [6] = "G",
    [7] = "H",
    [8] = "I",
    [9] = "J",
    [10] = "K",
    [11] = "L",
    [12] = "M",
    [13] = "N",
    [14] = "O",
    [15] = "P",
    [16] = "Q",
    [17] = "R",
    [18] = "S",
    [19] = "T",
    [20] = "U",
    [21] = "V",
    [22] = "W",
    [23] = "X",
    [24] = "Y",
    [25] = "Z",
    [26] = "a",
    [27] = "b",
    [28] = "c",
    [29] = "d",
    [30] = "e",
    [31] = "f",
    [32] = "g",
    [33] = "h",
    [34] = "i",
    [35] = "j",
    [36] = "k",
    [37] = "l",
    [38] = "m",
    [39] = "n",
    [40] = "o",
    [41] = "p",
    [42] = "q",
    [43] = "r",
    [44] = "s",
    [45] = "t",
    [46] = "u",
    [47] = "v",
    [48] = "w",
    [49] = "x",
    [50] = "y",
    [51] = "z",
    [52] = "0",
    [53] = "1",
    [54] = "2",
    [55] = "3",
    [56] = "4",
    [57] = "5",
    [58] = "6",
    [59] = "7",
    [60] = "8",
    [61] = "9",
    [62] = "-",
    [63] = "_",
  }

  -- function encode
  -- encodes input string to base64.
  function enc(data)
    local bytes = {}
    local result = ""
    for spos = 0, string.len(data) - 1, 3 do
      for byte = 1, 3 do
        bytes[byte] = string.byte(string.sub(data, (spos + byte))) or 0
      end
      result = string.format(
      "%s%s%s%s%s",
      result,
      base64chars[rsh(bytes[1], 2)],
      base64chars[lor(lsh((bytes[1] % 4), 4), rsh(bytes[2], 4))] or "=",
      ((#data - spos) > 1) and base64chars[lor(lsh(bytes[2] % 16, 2), rsh(bytes[3], 6))] or "=",
      ((#data - spos) > 2) and base64chars[(bytes[3] % 64)] or "="
      )
    end
    return result
  end

  -- decryption table
  local base64bytes = {
    ["A"] = 0,
    ["B"] = 1,
    ["C"] = 2,
    ["D"] = 3,
    ["E"] = 4,
    ["F"] = 5,
    ["G"] = 6,
    ["H"] = 7,
    ["I"] = 8,
    ["J"] = 9,
    ["K"] = 10,
    ["L"] = 11,
    ["M"] = 12,
    ["N"] = 13,
    ["O"] = 14,
    ["P"] = 15,
    ["Q"] = 16,
    ["R"] = 17,
    ["S"] = 18,
    ["T"] = 19,
    ["U"] = 20,
    ["V"] = 21,
    ["W"] = 22,
    ["X"] = 23,
    ["Y"] = 24,
    ["Z"] = 25,
    ["a"] = 26,
    ["b"] = 27,
    ["c"] = 28,
    ["d"] = 29,
    ["e"] = 30,
    ["f"] = 31,
    ["g"] = 32,
    ["h"] = 33,
    ["i"] = 34,
    ["j"] = 35,
    ["k"] = 36,
    ["l"] = 37,
    ["m"] = 38,
    ["n"] = 39,
    ["o"] = 40,
    ["p"] = 41,
    ["q"] = 42,
    ["r"] = 43,
    ["s"] = 44,
    ["t"] = 45,
    ["u"] = 46,
    ["v"] = 47,
    ["w"] = 48,
    ["x"] = 49,
    ["y"] = 50,
    ["z"] = 51,
    ["0"] = 52,
    ["1"] = 53,
    ["2"] = 54,
    ["3"] = 55,
    ["4"] = 56,
    ["5"] = 57,
    ["6"] = 58,
    ["7"] = 59,
    ["8"] = 60,
    ["9"] = 61,
    ["-"] = 62,
    ["_"] = 63,
    ["="] = nil,
  }

  module.base64bytes = base64bytes

  -- function decode
  -- decode base64 input to string
  function dec(data)
    local chars = {}
    local result = ""
    for dpos = 0, string.len(data) - 1, 4 do
      for char = 1, 4 do
        chars[char] = base64bytes[(string.sub(data, (dpos + char), (dpos + char)) or "=")]
      end
      result = string.format(
      "%s%s%s%s",
      result,
      string.char(lor(lsh(chars[1], 2), rsh(chars[2], 4))),
      (chars[3] ~= nil) and string.char(lor(lsh(chars[2], 4), rsh(chars[3], 2))) or "",
      (chars[4] ~= nil) and string.char(lor(lsh(chars[3], 6) % 192, chars[4])) or ""
      )
    end
    return result
  end

  function module.encode(content)
    local status, ret = pcall(enc, content)
    if not status then
      error("failed to encode content: " .. ret)
      return ""
    end
    return ret
  end

  function module.decode(content)
    local status, ret = pcall(dec, content)
    if not status then
      error("failed to decode content: " .. ret)
      return ""
    end
    return ret
  end


  sha256 = { }

  local MOD = 2 ^ 32
  local MODM = MOD - 1

  local function memoize(f)
    local mt = { }
    local t = setmetatable( { }, mt)
    function mt:__index(k)
      local v = f(k)
      t[k] = v
      return v
    end
    return t
  end

  local function make_bitop_uncached(t, m)
    local function bitop(a, b)
      local res, p = 0, 1
      while a ~= 0 and b ~= 0 do
        local am, bm = a % m, b % m
        res = res + t[am][bm] * p
        a =(a - am) / m
        b =(b - bm) / m
        p = p * m
      end
      res = res +(a + b) * p
      return res
    end
    return bitop
  end

  local function make_bitop(t)
    local op1 = make_bitop_uncached(t, 2 ^ 1)
    local op2 = memoize( function(a) return memoize( function(b) return op1(a, b) end) end)
    return make_bitop_uncached(op2, 2 ^(t.n or 1))
  end

  local bxor1 = make_bitop( { [0] = { [0] = 0, [1] = 1 }, [1] = { [0] = 1, [1] = 0 }, n = 4 })

  local function bxor(a, b, c, ...)
    local z = nil
    if b then
      a = a % MOD
      b = b % MOD
      z = bxor1(a, b)
      if c then z = bxor(z, c, ...) end
      return z
     elseif a then
      return a % MOD
     else
      return 0
    end
  end

  local function band(a, b, c, ...)
    local z
    if b then
      a = a % MOD
      b = b % MOD
      z =((a + b) - bxor1(a, b)) / 2
      if c then z = bit32_band(z, c, ...) end
      return z
     elseif a then
      return a % MOD
     else
      return MODM
    end
  end

  local function bnot(x) return(-1 - x) % MOD end

  local function rshift1(a, disp)
    if disp < 0 then return lshift(a, - disp) end
    return math.floor(a % 2 ^ 32 / 2 ^ disp)
  end

  local function rshift(x, disp)
    if disp > 31 or disp < -31 then return 0 end
    return rshift1(x % MOD, disp)
  end

  local function lshift(a, disp)
    if disp < 0 then return rshift(a, - disp) end
    return(a * 2 ^ disp) % 2 ^ 32
  end

  local function rrotate(x, disp)
    x = x % MOD
    disp = disp % 32
    local low = band(x, 2 ^ disp - 1)
    return rshift(x, disp) + lshift(low, 32 - disp)
  end

  local k = {
    0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,
    0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
    0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,
    0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
    0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,
    0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
    0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,
    0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
    0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,
    0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
    0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,
    0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
    0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,
    0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
    0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,
    0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2,
  }

  local function str2hexa(s)
    return(string.gsub(s, ".", function(c) return string.format("%02x", string.byte(c)) end))
  end

  local function num2s(l, n)
    local s = ""
    for i = 1, n do
      local rem = l % 256
      s = string.char(rem) .. s
      l =(l - rem) / 256
    end
    return s
  end

  local function s232num(s, i)
    local n = 0
    for i = i, i + 3 do n = n * 256 + string.byte(s, i) end
    return n
  end

  local function preproc(msg, len)
    local extra = 64 -((len + 9) % 64)
    len = num2s(8 * len, 8)
    msg = msg .. "\128" .. string.rep("\0", extra) .. len
    assert(#msg % 64 == 0)
    return msg
  end

  local function initH256(H)
    H[1] = 0x6a09e667
    H[2] = 0xbb67ae85
    H[3] = 0x3c6ef372
    H[4] = 0xa54ff53a
    H[5] = 0x510e527f
    H[6] = 0x9b05688c
    H[7] = 0x1f83d9ab
    H[8] = 0x5be0cd19
    return H
  end

  local function digestblock(msg, i, H)
    local w = { }
    for j = 1, 16 do w[j] = s232num(msg, i +(j - 1) * 4) end
    for j = 17, 64 do
      local v = w[j - 15]
      local s0 = bxor(rrotate(v, 7), rrotate(v, 18), rshift(v, 3))
      v = w[j - 2]
      w[j] = w[j - 16] + s0 + w[j - 7] + bxor(rrotate(v, 17), rrotate(v, 19), rshift(v, 10))
    end

    local a, b, c, d, e, f, g, h = H[1], H[2], H[3], H[4], H[5], H[6], H[7], H[8]
    for i = 1, 64 do
      local s0 = bxor(rrotate(a, 2), rrotate(a, 13), rrotate(a, 22))
      local maj = bxor(band(a, b), band(a, c), band(b, c))
      local t2 = s0 + maj
      local s1 = bxor(rrotate(e, 6), rrotate(e, 11), rrotate(e, 25))
      local ch = bxor(band(e, f), band(bnot(e), g))
      local t1 = h + s1 + ch + k[i] + w[i]
      h, g, f, e, d, c, b, a = g, f, e, d + t1, c, b, a, t1 + t2
    end

    H[1] = band(H[1] + a)
    H[2] = band(H[2] + b)
    H[3] = band(H[3] + c)
    H[4] = band(H[4] + d)
    H[5] = band(H[5] + e)
    H[6] = band(H[6] + f)
    H[7] = band(H[7] + g)
    H[8] = band(H[8] + h)
  end

  local function hex_to_binary(hex)
    return hex:gsub('..', function(hexval)
      return string.char(tonumber(hexval, 16))
    end )
  end

  local blocksize = 64 -- 512 bits

  local xor_with_0x5c = {
    [string.char(0)] = string.char(92),
    [string.char(1)] = string.char(93),
    [string.char(2)] = string.char(94),
    [string.char(3)] = string.char(95),
    [string.char(4)] = string.char(88),
    [string.char(5)] = string.char(89),
    [string.char(6)] = string.char(90),
    [string.char(7)] = string.char(91),
    [string.char(8)] = string.char(84),
    [string.char(9)] = string.char(85),
    [string.char(10)] = string.char(86),
    [string.char(11)] = string.char(87),
    [string.char(12)] = string.char(80),
    [string.char(13)] = string.char(81),
    [string.char(14)] = string.char(82),
    [string.char(15)] = string.char(83),
    [string.char(16)] = string.char(76),
    [string.char(17)] = string.char(77),
    [string.char(18)] = string.char(78),
    [string.char(19)] = string.char(79),
    [string.char(20)] = string.char(72),
    [string.char(21)] = string.char(73),
    [string.char(22)] = string.char(74),
    [string.char(23)] = string.char(75),
    [string.char(24)] = string.char(68),
    [string.char(25)] = string.char(69),
    [string.char(26)] = string.char(70),
    [string.char(27)] = string.char(71),
    [string.char(28)] = string.char(64),
    [string.char(29)] = string.char(65),
    [string.char(30)] = string.char(66),
    [string.char(31)] = string.char(67),
    [string.char(32)] = string.char(124),
    [string.char(33)] = string.char(125),
    [string.char(34)] = string.char(126),
    [string.char(35)] = string.char(127),
    [string.char(36)] = string.char(120),
    [string.char(37)] = string.char(121),
    [string.char(38)] = string.char(122),
    [string.char(39)] = string.char(123),
    [string.char(40)] = string.char(116),
    [string.char(41)] = string.char(117),
    [string.char(42)] = string.char(118),
    [string.char(43)] = string.char(119),
    [string.char(44)] = string.char(112),
    [string.char(45)] = string.char(113),
    [string.char(46)] = string.char(114),
    [string.char(47)] = string.char(115),
    [string.char(48)] = string.char(108),
    [string.char(49)] = string.char(109),
    [string.char(50)] = string.char(110),
    [string.char(51)] = string.char(111),
    [string.char(52)] = string.char(104),
    [string.char(53)] = string.char(105),
    [string.char(54)] = string.char(106),
    [string.char(55)] = string.char(107),
    [string.char(56)] = string.char(100),
    [string.char(57)] = string.char(101),
    [string.char(58)] = string.char(102),
    [string.char(59)] = string.char(103),
    [string.char(60)] = string.char(96),
    [string.char(61)] = string.char(97),
    [string.char(62)] = string.char(98),
    [string.char(63)] = string.char(99),
    [string.char(64)] = string.char(28),
    [string.char(65)] = string.char(29),
    [string.char(66)] = string.char(30),
    [string.char(67)] = string.char(31),
    [string.char(68)] = string.char(24),
    [string.char(69)] = string.char(25),
    [string.char(70)] = string.char(26),
    [string.char(71)] = string.char(27),
    [string.char(72)] = string.char(20),
    [string.char(73)] = string.char(21),
    [string.char(74)] = string.char(22),
    [string.char(75)] = string.char(23),
    [string.char(76)] = string.char(16),
    [string.char(77)] = string.char(17),
    [string.char(78)] = string.char(18),
    [string.char(79)] = string.char(19),
    [string.char(80)] = string.char(12),
    [string.char(81)] = string.char(13),
    [string.char(82)] = string.char(14),
    [string.char(83)] = string.char(15),
    [string.char(84)] = string.char(8),
    [string.char(85)] = string.char(9),
    [string.char(86)] = string.char(10),
    [string.char(87)] = string.char(11),
    [string.char(88)] = string.char(4),
    [string.char(89)] = string.char(5),
    [string.char(90)] = string.char(6),
    [string.char(91)] = string.char(7),
    [string.char(92)] = string.char(0),
    [string.char(93)] = string.char(1),
    [string.char(94)] = string.char(2),
    [string.char(95)] = string.char(3),
    [string.char(96)] = string.char(60),
    [string.char(97)] = string.char(61),
    [string.char(98)] = string.char(62),
    [string.char(99)] = string.char(63),
    [string.char(100)] = string.char(56),
    [string.char(101)] = string.char(57),
    [string.char(102)] = string.char(58),
    [string.char(103)] = string.char(59),
    [string.char(104)] = string.char(52),
    [string.char(105)] = string.char(53),
    [string.char(106)] = string.char(54),
    [string.char(107)] = string.char(55),
    [string.char(108)] = string.char(48),
    [string.char(109)] = string.char(49),
    [string.char(110)] = string.char(50),
    [string.char(111)] = string.char(51),
    [string.char(112)] = string.char(44),
    [string.char(113)] = string.char(45),
    [string.char(114)] = string.char(46),
    [string.char(115)] = string.char(47),
    [string.char(116)] = string.char(40),
    [string.char(117)] = string.char(41),
    [string.char(118)] = string.char(42),
    [string.char(119)] = string.char(43),
    [string.char(120)] = string.char(36),
    [string.char(121)] = string.char(37),
    [string.char(122)] = string.char(38),
    [string.char(123)] = string.char(39),
    [string.char(124)] = string.char(32),
    [string.char(125)] = string.char(33),
    [string.char(126)] = string.char(34),
    [string.char(127)] = string.char(35),
    [string.char(128)] = string.char(220),
    [string.char(129)] = string.char(221),
    [string.char(130)] = string.char(222),
    [string.char(131)] = string.char(223),
    [string.char(132)] = string.char(216),
    [string.char(133)] = string.char(217),
    [string.char(134)] = string.char(218),
    [string.char(135)] = string.char(219),
    [string.char(136)] = string.char(212),
    [string.char(137)] = string.char(213),
    [string.char(138)] = string.char(214),
    [string.char(139)] = string.char(215),
    [string.char(140)] = string.char(208),
    [string.char(141)] = string.char(209),
    [string.char(142)] = string.char(210),
    [string.char(143)] = string.char(211),
    [string.char(144)] = string.char(204),
    [string.char(145)] = string.char(205),
    [string.char(146)] = string.char(206),
    [string.char(147)] = string.char(207),
    [string.char(148)] = string.char(200),
    [string.char(149)] = string.char(201),
    [string.char(150)] = string.char(202),
    [string.char(151)] = string.char(203),
    [string.char(152)] = string.char(196),
    [string.char(153)] = string.char(197),
    [string.char(154)] = string.char(198),
    [string.char(155)] = string.char(199),
    [string.char(156)] = string.char(192),
    [string.char(157)] = string.char(193),
    [string.char(158)] = string.char(194),
    [string.char(159)] = string.char(195),
    [string.char(160)] = string.char(252),
    [string.char(161)] = string.char(253),
    [string.char(162)] = string.char(254),
    [string.char(163)] = string.char(255),
    [string.char(164)] = string.char(248),
    [string.char(165)] = string.char(249),
    [string.char(166)] = string.char(250),
    [string.char(167)] = string.char(251),
    [string.char(168)] = string.char(244),
    [string.char(169)] = string.char(245),
    [string.char(170)] = string.char(246),
    [string.char(171)] = string.char(247),
    [string.char(172)] = string.char(240),
    [string.char(173)] = string.char(241),
    [string.char(174)] = string.char(242),
    [string.char(175)] = string.char(243),
    [string.char(176)] = string.char(236),
    [string.char(177)] = string.char(237),
    [string.char(178)] = string.char(238),
    [string.char(179)] = string.char(239),
    [string.char(180)] = string.char(232),
    [string.char(181)] = string.char(233),
    [string.char(182)] = string.char(234),
    [string.char(183)] = string.char(235),
    [string.char(184)] = string.char(228),
    [string.char(185)] = string.char(229),
    [string.char(186)] = string.char(230),
    [string.char(187)] = string.char(231),
    [string.char(188)] = string.char(224),
    [string.char(189)] = string.char(225),
    [string.char(190)] = string.char(226),
    [string.char(191)] = string.char(227),
    [string.char(192)] = string.char(156),
    [string.char(193)] = string.char(157),
    [string.char(194)] = string.char(158),
    [string.char(195)] = string.char(159),
    [string.char(196)] = string.char(152),
    [string.char(197)] = string.char(153),
    [string.char(198)] = string.char(154),
    [string.char(199)] = string.char(155),
    [string.char(200)] = string.char(148),
    [string.char(201)] = string.char(149),
    [string.char(202)] = string.char(150),
    [string.char(203)] = string.char(151),
    [string.char(204)] = string.char(144),
    [string.char(205)] = string.char(145),
    [string.char(206)] = string.char(146),
    [string.char(207)] = string.char(147),
    [string.char(208)] = string.char(140),
    [string.char(209)] = string.char(141),
    [string.char(210)] = string.char(142),
    [string.char(211)] = string.char(143),
    [string.char(212)] = string.char(136),
    [string.char(213)] = string.char(137),
    [string.char(214)] = string.char(138),
    [string.char(215)] = string.char(139),
    [string.char(216)] = string.char(132),
    [string.char(217)] = string.char(133),
    [string.char(218)] = string.char(134),
    [string.char(219)] = string.char(135),
    [string.char(220)] = string.char(128),
    [string.char(221)] = string.char(129),
    [string.char(222)] = string.char(130),
    [string.char(223)] = string.char(131),
    [string.char(224)] = string.char(188),
    [string.char(225)] = string.char(189),
    [string.char(226)] = string.char(190),
    [string.char(227)] = string.char(191),
    [string.char(228)] = string.char(184),
    [string.char(229)] = string.char(185),
    [string.char(230)] = string.char(186),
    [string.char(231)] = string.char(187),
    [string.char(232)] = string.char(180),
    [string.char(233)] = string.char(181),
    [string.char(234)] = string.char(182),
    [string.char(235)] = string.char(183),
    [string.char(236)] = string.char(176),
    [string.char(237)] = string.char(177),
    [string.char(238)] = string.char(178),
    [string.char(239)] = string.char(179),
    [string.char(240)] = string.char(172),
    [string.char(241)] = string.char(173),
    [string.char(242)] = string.char(174),
    [string.char(243)] = string.char(175),
    [string.char(244)] = string.char(168),
    [string.char(245)] = string.char(169),
    [string.char(246)] = string.char(170),
    [string.char(247)] = string.char(171),
    [string.char(248)] = string.char(164),
    [string.char(249)] = string.char(165),
    [string.char(250)] = string.char(166),
    [string.char(251)] = string.char(167),
    [string.char(252)] = string.char(160),
    [string.char(253)] = string.char(161),
    [string.char(254)] = string.char(162),
    [string.char(255)] = string.char(163),
  }

  local xor_with_0x36 = {
    [string.char(0)] = string.char(54),
    [string.char(1)] = string.char(55),
    [string.char(2)] = string.char(52),
    [string.char(3)] = string.char(53),
    [string.char(4)] = string.char(50),
    [string.char(5)] = string.char(51),
    [string.char(6)] = string.char(48),
    [string.char(7)] = string.char(49),
    [string.char(8)] = string.char(62),
    [string.char(9)] = string.char(63),
    [string.char(10)] = string.char(60),
    [string.char(11)] = string.char(61),
    [string.char(12)] = string.char(58),
    [string.char(13)] = string.char(59),
    [string.char(14)] = string.char(56),
    [string.char(15)] = string.char(57),
    [string.char(16)] = string.char(38),
    [string.char(17)] = string.char(39),
    [string.char(18)] = string.char(36),
    [string.char(19)] = string.char(37),
    [string.char(20)] = string.char(34),
    [string.char(21)] = string.char(35),
    [string.char(22)] = string.char(32),
    [string.char(23)] = string.char(33),
    [string.char(24)] = string.char(46),
    [string.char(25)] = string.char(47),
    [string.char(26)] = string.char(44),
    [string.char(27)] = string.char(45),
    [string.char(28)] = string.char(42),
    [string.char(29)] = string.char(43),
    [string.char(30)] = string.char(40),
    [string.char(31)] = string.char(41),
    [string.char(32)] = string.char(22),
    [string.char(33)] = string.char(23),
    [string.char(34)] = string.char(20),
    [string.char(35)] = string.char(21),
    [string.char(36)] = string.char(18),
    [string.char(37)] = string.char(19),
    [string.char(38)] = string.char(16),
    [string.char(39)] = string.char(17),
    [string.char(40)] = string.char(30),
    [string.char(41)] = string.char(31),
    [string.char(42)] = string.char(28),
    [string.char(43)] = string.char(29),
    [string.char(44)] = string.char(26),
    [string.char(45)] = string.char(27),
    [string.char(46)] = string.char(24),
    [string.char(47)] = string.char(25),
    [string.char(48)] = string.char(6),
    [string.char(49)] = string.char(7),
    [string.char(50)] = string.char(4),
    [string.char(51)] = string.char(5),
    [string.char(52)] = string.char(2),
    [string.char(53)] = string.char(3),
    [string.char(54)] = string.char(0),
    [string.char(55)] = string.char(1),
    [string.char(56)] = string.char(14),
    [string.char(57)] = string.char(15),
    [string.char(58)] = string.char(12),
    [string.char(59)] = string.char(13),
    [string.char(60)] = string.char(10),
    [string.char(61)] = string.char(11),
    [string.char(62)] = string.char(8),
    [string.char(63)] = string.char(9),
    [string.char(64)] = string.char(118),
    [string.char(65)] = string.char(119),
    [string.char(66)] = string.char(116),
    [string.char(67)] = string.char(117),
    [string.char(68)] = string.char(114),
    [string.char(69)] = string.char(115),
    [string.char(70)] = string.char(112),
    [string.char(71)] = string.char(113),
    [string.char(72)] = string.char(126),
    [string.char(73)] = string.char(127),
    [string.char(74)] = string.char(124),
    [string.char(75)] = string.char(125),
    [string.char(76)] = string.char(122),
    [string.char(77)] = string.char(123),
    [string.char(78)] = string.char(120),
    [string.char(79)] = string.char(121),
    [string.char(80)] = string.char(102),
    [string.char(81)] = string.char(103),
    [string.char(82)] = string.char(100),
    [string.char(83)] = string.char(101),
    [string.char(84)] = string.char(98),
    [string.char(85)] = string.char(99),
    [string.char(86)] = string.char(96),
    [string.char(87)] = string.char(97),
    [string.char(88)] = string.char(110),
    [string.char(89)] = string.char(111),
    [string.char(90)] = string.char(108),
    [string.char(91)] = string.char(109),
    [string.char(92)] = string.char(106),
    [string.char(93)] = string.char(107),
    [string.char(94)] = string.char(104),
    [string.char(95)] = string.char(105),
    [string.char(96)] = string.char(86),
    [string.char(97)] = string.char(87),
    [string.char(98)] = string.char(84),
    [string.char(99)] = string.char(85),
    [string.char(100)] = string.char(82),
    [string.char(101)] = string.char(83),
    [string.char(102)] = string.char(80),
    [string.char(103)] = string.char(81),
    [string.char(104)] = string.char(94),
    [string.char(105)] = string.char(95),
    [string.char(106)] = string.char(92),
    [string.char(107)] = string.char(93),
    [string.char(108)] = string.char(90),
    [string.char(109)] = string.char(91),
    [string.char(110)] = string.char(88),
    [string.char(111)] = string.char(89),
    [string.char(112)] = string.char(70),
    [string.char(113)] = string.char(71),
    [string.char(114)] = string.char(68),
    [string.char(115)] = string.char(69),
    [string.char(116)] = string.char(66),
    [string.char(117)] = string.char(67),
    [string.char(118)] = string.char(64),
    [string.char(119)] = string.char(65),
    [string.char(120)] = string.char(78),
    [string.char(121)] = string.char(79),
    [string.char(122)] = string.char(76),
    [string.char(123)] = string.char(77),
    [string.char(124)] = string.char(74),
    [string.char(125)] = string.char(75),
    [string.char(126)] = string.char(72),
    [string.char(127)] = string.char(73),
    [string.char(128)] = string.char(182),
    [string.char(129)] = string.char(183),
    [string.char(130)] = string.char(180),
    [string.char(131)] = string.char(181),
    [string.char(132)] = string.char(178),
    [string.char(133)] = string.char(179),
    [string.char(134)] = string.char(176),
    [string.char(135)] = string.char(177),
    [string.char(136)] = string.char(190),
    [string.char(137)] = string.char(191),
    [string.char(138)] = string.char(188),
    [string.char(139)] = string.char(189),
    [string.char(140)] = string.char(186),
    [string.char(141)] = string.char(187),
    [string.char(142)] = string.char(184),
    [string.char(143)] = string.char(185),
    [string.char(144)] = string.char(166),
    [string.char(145)] = string.char(167),
    [string.char(146)] = string.char(164),
    [string.char(147)] = string.char(165),
    [string.char(148)] = string.char(162),
    [string.char(149)] = string.char(163),
    [string.char(150)] = string.char(160),
    [string.char(151)] = string.char(161),
    [string.char(152)] = string.char(174),
    [string.char(153)] = string.char(175),
    [string.char(154)] = string.char(172),
    [string.char(155)] = string.char(173),
    [string.char(156)] = string.char(170),
    [string.char(157)] = string.char(171),
    [string.char(158)] = string.char(168),
    [string.char(159)] = string.char(169),
    [string.char(160)] = string.char(150),
    [string.char(161)] = string.char(151),
    [string.char(162)] = string.char(148),
    [string.char(163)] = string.char(149),
    [string.char(164)] = string.char(146),
    [string.char(165)] = string.char(147),
    [string.char(166)] = string.char(144),
    [string.char(167)] = string.char(145),
    [string.char(168)] = string.char(158),
    [string.char(169)] = string.char(159),
    [string.char(170)] = string.char(156),
    [string.char(171)] = string.char(157),
    [string.char(172)] = string.char(154),
    [string.char(173)] = string.char(155),
    [string.char(174)] = string.char(152),
    [string.char(175)] = string.char(153),
    [string.char(176)] = string.char(134),
    [string.char(177)] = string.char(135),
    [string.char(178)] = string.char(132),
    [string.char(179)] = string.char(133),
    [string.char(180)] = string.char(130),
    [string.char(181)] = string.char(131),
    [string.char(182)] = string.char(128),
    [string.char(183)] = string.char(129),
    [string.char(184)] = string.char(142),
    [string.char(185)] = string.char(143),
    [string.char(186)] = string.char(140),
    [string.char(187)] = string.char(141),
    [string.char(188)] = string.char(138),
    [string.char(189)] = string.char(139),
    [string.char(190)] = string.char(136),
    [string.char(191)] = string.char(137),
    [string.char(192)] = string.char(246),
    [string.char(193)] = string.char(247),
    [string.char(194)] = string.char(244),
    [string.char(195)] = string.char(245),
    [string.char(196)] = string.char(242),
    [string.char(197)] = string.char(243),
    [string.char(198)] = string.char(240),
    [string.char(199)] = string.char(241),
    [string.char(200)] = string.char(254),
    [string.char(201)] = string.char(255),
    [string.char(202)] = string.char(252),
    [string.char(203)] = string.char(253),
    [string.char(204)] = string.char(250),
    [string.char(205)] = string.char(251),
    [string.char(206)] = string.char(248),
    [string.char(207)] = string.char(249),
    [string.char(208)] = string.char(230),
    [string.char(209)] = string.char(231),
    [string.char(210)] = string.char(228),
    [string.char(211)] = string.char(229),
    [string.char(212)] = string.char(226),
    [string.char(213)] = string.char(227),
    [string.char(214)] = string.char(224),
    [string.char(215)] = string.char(225),
    [string.char(216)] = string.char(238),
    [string.char(217)] = string.char(239),
    [string.char(218)] = string.char(236),
    [string.char(219)] = string.char(237),
    [string.char(220)] = string.char(234),
    [string.char(221)] = string.char(235),
    [string.char(222)] = string.char(232),
    [string.char(223)] = string.char(233),
    [string.char(224)] = string.char(214),
    [string.char(225)] = string.char(215),
    [string.char(226)] = string.char(212),
    [string.char(227)] = string.char(213),
    [string.char(228)] = string.char(210),
    [string.char(229)] = string.char(211),
    [string.char(230)] = string.char(208),
    [string.char(231)] = string.char(209),
    [string.char(232)] = string.char(222),
    [string.char(233)] = string.char(223),
    [string.char(234)] = string.char(220),
    [string.char(235)] = string.char(221),
    [string.char(236)] = string.char(218),
    [string.char(237)] = string.char(219),
    [string.char(238)] = string.char(216),
    [string.char(239)] = string.char(217),
    [string.char(240)] = string.char(198),
    [string.char(241)] = string.char(199),
    [string.char(242)] = string.char(196),
    [string.char(243)] = string.char(197),
    [string.char(244)] = string.char(194),
    [string.char(245)] = string.char(195),
    [string.char(246)] = string.char(192),
    [string.char(247)] = string.char(193),
    [string.char(248)] = string.char(206),
    [string.char(249)] = string.char(207),
    [string.char(250)] = string.char(204),
    [string.char(251)] = string.char(205),
    [string.char(252)] = string.char(202),
    [string.char(253)] = string.char(203),
    [string.char(254)] = string.char(200),
    [string.char(255)] = string.char(201),
  }

  -------------------------------------------------------------------------

  function sha256.sha256(msg)
    msg = preproc(msg, #msg)
    local H = initH256( { })
    for i = 1, #msg, 64 do digestblock(msg, i, H) end
    return str2hexa(num2s(H[1], 4) .. num2s(H[2], 4) .. num2s(H[3], 4) .. num2s(H[4], 4) ..
    num2s(H[5], 4) .. num2s(H[6], 4) .. num2s(H[7], 4) .. num2s(H[8], 4))
  end

  function sha256.sha256_binary(msg)
    return hex_to_binary(sha256.sha256(msg))
  end

  function sha256.hmac_sha256(key, text)
    assert(type(key) == 'string', "key passed to hmac_sha256 should be a string")
    assert(type(text) == 'string', "text passed to hmac_sha256 should be a string")

    if #key > blocksize then
      key = sha256.sha256_binary(key)
    end

    local key_xord_with_0x36 = key:gsub('.', xor_with_0x36) .. string.rep(string.char(0x36), blocksize - #key)
    local key_xord_with_0x5c = key:gsub('.', xor_with_0x5c) .. string.rep(string.char(0x5c), blocksize - #key)

    return sha256.sha256(key_xord_with_0x5c .. sha256.sha256_binary(key_xord_with_0x36 .. text))
  end











  
  local function base64url_encode(input)
    local base64encoded = module.encode(input) 
    base64encoded = string.gsub(base64encoded, '+', '-')
    base64encoded = string.gsub(base64encoded, '/', '_')
    base64encoded = string.gsub(base64encoded, '=', '') 
    return base64encoded
  end
  local function base64url_decode(input)
    
    local base64encoded = string.gsub(input, '-', '+')
    base64encoded = string.gsub(base64encoded, '_', '/')


    local padding = 4 - (#input % 4)
    if padding < 4 then
      base64encoded = base64encoded .. string.rep('=', padding)
    end

   
    local decoded = module.decode(base64encoded)
    return decoded
  end

function create_jwt(header, payload, secret)

    local header_json =json.encode (header, { indent = false })
    local payload_json = json.encode (payload, { indent = false })
    
    local header_b64url = base64url_encode(header_json)
    local payload_b64url = base64url_encode(payload_json)

    local unsigned_token = header_b64url .. "." .. payload_b64url
    local signature = sha256.hmac_sha256(secret, unsigned_token)
    local signature_b64url = base64url_encode(signature)

    return unsigned_token .. "." .. signature_b64url
  end
  
  
  function check_jwt(payload,dittoer,secret,checker)

    local signature = sha256.hmac_sha256(secret, payload[1].."."..payload[2])
    if base64url_decode(dittoer)==signature then

     else
   
      return false
    end
    if base64url_decode(payload[2]) then
      dittotableer,a,b=json.decode(base64url_decode(payload[2]),1,nil)
      if dittotableer.exp and dittotableer.iat then

       else
        return false
      end
      
      if os.time()>=dittotableer.exp or dittotableer.exp < dittotableer.iat then
        
        return false
      end
      return dittotableer
      

     else

      return false
    end


  end






--End of initialization  


  function checker(checkkey,dittokey)
  local header = {
    alg = "HS256",
    typ = "JWT"
  }
  
  
      if checkkey then
        if dittokey then
        payload = {
        exp = os.time() + 75,
        iat = os.time(),
        ["platoboost-id"]=YourAccountid,
        hwid=Yourhwid,
        mode="check",
        key=dittokey
        
        
      }  
    else
    error("Where is your key?")
    end
         else
       payload = {
        exp = os.time() + 25,
        iat = os.time(),
        ["platoboost-id"]=YourAccountid,
        hwid=Yourhwid,
        mode="whitelist",
        
        
      }
end

      jwt = create_jwt(header, payload, secret)
  

      jwt=request({
        Url="yourserver.php?token="..jwt,
        Method="GET"
      }).Body

      
      local parts = {}
      for part in jwt:gmatch("[^.]+" ) do
        table.insert(parts, part)
      end

      if #parts == 3 then
        ditto={parts[1],parts[2]}
        dittolol=check_jwt(ditto,parts[3],secret,dittoaaaaaaaaaa)
        if dittolol then
          if dittolol.check=="true" then
         
            return true 
            else
         
            return false
            end
      
       end
     return false
       end
     end
 
 function getkey()
   return "https://gateway.platoboost.com/a/"..YourAccountid.."?id="..Yourhwid
   end
 --how to use 
 --You need to make sure that the previous code is complete
 YourAccountid=8990
  Yourhwid=gethwid()
  secret="your_secret_key"
  --Your configuration
 
 
 if checker(false) then
   print("Whitelisted")
   else
   print("Not Whitelist")
   setclipboard(getkey())
   end
 --This is check the whitelist
 
 
 if checker(true,"Your key") then
   print("Redeem successfully. Hwid has been whitelisted.")
   else
   print("error key")
   end
 --checker(checkkey?(true/false),your key want to check(string)) 
 --return boolean (true/false)
 
 --getkey()
 --return the getkey link(string)
 
 --You should know
 --platoboost uses the whitelist to check users.
 --The key is not used to check the user.
 --The key is used to redeem the whitelist, that is, to get the current Hwid whitelisted 
 --if an hwid already has a whitelist, it will update the whitelist time. 
 --If the time set by the key is less than the current whitelist time, 
 --the whitelist time will not be updated. 
 --Otherwise, the whitelist time will be updated to the time set by your key.
 
