local byte = {}

-- byte_string: string of raw bytes representing a number
-- return: numeric value param
function byte.bin_to_num(byte_string)
    local numeric = 0
    for i=1,#byte_string do
        numeric = bit32.lshift(numeric, 8) + byte_string:byte(i,i)
    end
    return numeric
end

-- byte_count: amount of bytes number should be saved in (left padded zeros)
-- ...: list of numbers
-- return: binary string containing all the numbers
function byte.nums_to_bin(byte_count, ...)
    local byte_list = {}
    for i,num in ipairs({...}) do
        byte_list[i] = byte.num_to_bin(num, byte_count)
    end
    return table.concat(byte_list)
end

-- ...: variable amount of numbers which are assumed to be 1 byte in size
function byte.bytes_to_bin(...)
    return byte.nums_to_bin(1, ...)
end


-- number: number to convert to string of raw bytes
-- byte_count: amount of bytes number should be saved in (left padded zeros)
-- return: string of raw bytes representing the number
function byte.num_to_bin(number, byte_count)
    assert(number <= math.pow(2, 8*byte_count), string.format("Number %d is too big to be stored in %d bytes!", number, byte_count))
    local byte_list = {}
    for i=byte_count,1,-1 do
        table.insert(byte_list, string.char(bit32.band(0xFF, bit32.rshift(number, 8*(i - 1)))))
    end
    return table.concat(byte_list)
end

return byte
