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

function table_count(t)
  local i = 0
  for _ in pairs(t) do i = i + 1 end
  return i
end

instructions = {}

bots = {}
output = {}

Bot = {}
Bot.__index = Bot

function Bot:new(num)
  local o = {number = num, microchips = {}}
  setmetatable(o, self)
  self.__index = self
  return o
end

function Bot:give(low_bot, low_to_bot, high_bot, high_to_bot)
  if not bots[low_bot] then
    bots[low_bot] = Bot:new(low_bot)
  end
  if not bots[high_bot] then
    bots[high_bot] = Bot:new(high_bot)
  end

  local temp1 = self.microchips[1]
  local temp2 = self.microchips[2]

  self.microchips = {}

  if temp1 > temp2 then
    if low_to_bot then
      bots[low_bot]:receive(temp2)
    else
      output[low_bot] = temp2
    end

    if high_to_bot then
      bots[high_bot]:receive(temp1)
    else
      output[high_bot] = temp1
    end
  else
    if low_to_bot then
      bots[low_bot]:receive(temp1)
    else
      output[low_bot] = temp1
    end

    if high_to_bot then
      bots[high_bot]:receive(temp2)
    else
      output[high_bot] = temp2
    end
  end

  self.microchips = {}
end

function carry_out_instruction(num)
  local i = instructions[num]

  if not bots[num] then
    bots[num] = Bot:new(num)
  end

  bots[num]:give(i.low_to, i.low_to_bot, i.high_to, i.high_to_bot)
end

function Bot:receive(m)
  if #self.microchips < 2 then
    table.insert(self.microchips, m)
  else
    error("Bot already has 2 microchips")
  end

  if #self.microchips == 2 then
    carry_out_instruction(self.number)
  end
end

function table_count(t)
  local i = 0
  for _ in pairs(t) do i = i + 1 end
  return i
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

function view_outputs()
  for k, v in pairs(output) do
    print(k .. ": " .. v)
  end
end

function view_instructions()
  for k, v in pairs(instructions) do
    print(k .. ": " .. v.low_to .. ", " .. v.high_to)
  end
end

function populate_instructions_table(lines)
  for row_num, instruction in pairs(lines) do
    local parts = extract(instruction, "%S+")

    if parts[1] == "bot" then
      instructions[tonumber(parts[2])] = { low_to = tonumber(parts[7]),
                                           low_to_bot = parts[6] == "bot" and true or false,
                                           high_to = tonumber(parts[12]),
                                           high_to_bot = parts[11] == "bot" and true or false }
    end
  end
end

function give_bots_microchips(lines)
  for row_num, instruction in pairs(lines) do
    local parts = extract(instruction, "%S+")

    if parts[1] == "value" then
      local chip_num = tonumber(parts[2])
      local bot_num = tonumber(parts[6])

      if not bots[bot_num] then
        bots[bot_num] = Bot:new(bot_num)
      end

      bots[bot_num]:receive(chip_num)
    end
  end
end

file = 'input.txt'
lines = lines_from(file)

populate_instructions_table(lines)
while not (output[0] or output[1] or output[2]) do
  give_bots_microchips(lines)
end

print(output[0] * output[1] * output[2] .. " is the answer.")
