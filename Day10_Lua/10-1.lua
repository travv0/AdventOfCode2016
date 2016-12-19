seeking_chip1 = 61
seeking_chip2 = 17

function file_exists(file)
  local f = io.open(file, "rb")
  if f then f:close() end
  return f ~= nil
end

function lines_from(file)
  if not file_exists(file) then return {} end
  lines = {}
  for line in io.lines(file) do
    lines[#lines + 1] = line
  end
  return lines
end

bots = {}

Bot = {}
Bot.__index = Bot

function Bot:new()
  o = {microchips = {}}
  setmetatable(o, self)
  self.__index = self
  return o
end

function Bot:give(low_bot, high_bot)
  if not bots[low_bot] then
    bots[low_bot] = Bot:new()
  end
  if not bots[high_bot] then
    bots[high_bot] = Bot:new()
  end
  if self.microchips[1] > self.microchips[2] then
    bots[low_bot]:receive(self.microchips[2])
    bots[high_bot]:receive(self.microchips[1])
  else
    bots[low_bot]:receive(self.microchips[1])
    bots[high_bot]:receive(self.microchips[2])
  end
  self.microchips = {}
end

function Bot:receive(m)
  if #self.microchips < 2 then
    table.insert(self.microchips, m)
  else
    error("Bot already has 2 microchips")
  end
end

function table_count(t)
  local i = 0
  for _ in pairs(t) do i = i + 1 end
  return i
end

function parse_line(line_num, line)
  local parts = extract(line, "%S+")

  if parts[1] == "value" then
    local chip_num = tonumber(parts[2])
    local bot_num = tonumber(parts[6])

    if not bots[bot_num] then
      bots[bot_num] = Bot:new()
    end

    bots[bot_num]:receive(chip_num)
    table.remove(lines, line_num)
  elseif parts[1] == "bot" then
    local bot_num = tonumber(parts[2])
    local low_bot = tonumber(parts[7])
    local high_bot = tonumber(parts[12])

    if bots[bot_num] then
      if #bots[bot_num].microchips < 2 then
        print("Bot " .. bot_num .. " doesn't have enough microchips to act!")
      elseif completion_check(bots[bot_num].microchips[1], bots[bot_num].microchips[2]) then
        return bot_num
      else
        print("Bot " .. bot_num .. " is giving low to " .. low_bot .. " and high to " .. high_bot)
        bots[bot_num]:give(low_bot, high_bot)
        table.remove(lines, line_num)
      end
    else
      print("Bot " .. bot_num .. " not yet initialized.")
    end
  else
    error("Invalid command" .. parts[1])
  end

  return false
end

function completion_check(m1, m2)
  if m1 == seeking_chip1 and m2 == seeking_chip2 or
     m2 == seeking_chip1 and m1 == seeking_chip2 then
       return true
  end
  return false
end

function extract(s, separator)
  local r = {}
  local i = 1
  for m in string.gmatch(s, separator) do
    r[i] = m
    i = i + 1
  end
  return r
end

file = 'input.txt'
lines = lines_from(file)

ans = false
while not ans do
  for k, v in pairs(lines) do
    ans = parse_line(k, v)
    if ans then
      print("The answer is: " .. ans)
      break
    end
  end
end
