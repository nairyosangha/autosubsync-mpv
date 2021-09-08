local P = {}

local TimeStamp = {}
local TimeStamp_mt = { __index = TimeStamp }
function TimeStamp:new(hours, minutes, seconds)
	local new = {}
	new.hours = hours
	new.minutes = minutes
	new.seconds = seconds
	return setmetatable(new, TimeStamp_mt)
end

function TimeStamp.toTimeStamp(seconds)
	local diff, h, m, s = seconds, 0, 0, 0
	h = math.floor(diff / 3600)
	diff = diff - (h * 3600)
	m = math.floor(diff / 60)
	diff = diff - (m * 60)
	s = diff
	return TimeStamp:new(h, m, s)
end

function TimeStamp:toSeconds()
	return (3600 * self.hours) + (60 * self.minutes) + self.seconds
end

function TimeStamp:adjustTime(seconds)
	return self.toTimeStamp(self:toSeconds() + seconds)
end

function TimeStamp:toString(decimal_symbol)
	local seconds_fmt = string.format("%06.3f", self.seconds):gsub("%.", decimal_symbol)
	return string.format("%02d:%02d:%s", self.hours, self.minutes, seconds_fmt)
end

function TimeStamp.to_seconds(seconds, milliseconds)
	return tonumber(string.format("%s.%s", seconds, milliseconds))
end

local AbstractSubtitle = {}
local AbstractSubtitle_mt = { __index = AbstractSubtitle }

function AbstractSubtitle:create()
	local new = {}
	return setmetatable(new, AbstractSubtitle_mt)
end

function AbstractSubtitle:save()
	print(string.format("Writing '%s' to file..", self.filename))
	local f = io.open(self.filename, 'w')
	f:write(self:toString())
	f:close()
end

-- strip Byte Order Mark from file, if it's present
function AbstractSubtitle:sanitize(line)
	local bom_table = { 0xEF, 0xBB, 0xBF } -- TODO maybe add other ones (like UTF-16)
	local function has_bom()
		for i=1,#bom_table do
			if i > #line then return false end
			local ch, byte = line:sub(i,i), line:byte(i,i)
			if byte ~= bom_table[i] then return false end
		end
		return true
	end
	return has_bom() and string.sub(line, #bom_table+1) or line
end

function AbstractSubtitle:parse_file(filename)
	local lines = {}
	for line in io.lines(filename) do
		if #lines == 0 then line = self:sanitize(line) end
		line = line:gsub('\r\n?', '') -- make sure there's no carriage return
		table.insert(lines, line)
	end
	return lines
end

function AbstractSubtitle:shift_timing(diff_seconds)
	for _,entry in pairs(self.entries) do
		if self.valid_entry(entry) then
			entry.start_time = entry.start_time:adjustTime(diff_seconds)
			entry.end_time = entry.end_time:adjustTime(diff_seconds)
		end
	end
end

function AbstractSubtitle.valid_entry(entry)
	return entry ~= nil
end

local function inheritsFrom ( baseClass )
	local new_class = {}
	local class_mt = { __index = new_class }

	function new_class:create(filename)
		local instance = {
			filename = filename,
			language = nil,
			header = nil, -- will be empty for srt, some stuff for ass
			entries = {} -- list of entries
		}
		setmetatable(instance, class_mt)
		return instance
	end

	if baseClass then
		setmetatable(new_class, { __index = baseClass })
	end
	return new_class
end

local SRT = inheritsFrom(AbstractSubtitle)
function SRT.entry()
	return { index = nil, start_time = nil, end_time = nil, text = {} }
end

function SRT:populate(filename)
	local timestamp_fmt = "^(%d+):(%d+):(%d+),(%d+) %-%-> (%d+):(%d+):(%d+),(%d+)$"
	local function parse_timestamp(timestamp)
		local function to_seconds(seconds, milliseconds)
			return tonumber(string.format("%s.%s", seconds, milliseconds))
		end
		local _, _, from_h, from_m, from_s, from_ms, to_h, to_m, to_s, to_ms = timestamp:find(timestamp_fmt)
		return TimeStamp:new(from_h, from_m, to_seconds(from_s, from_ms)), TimeStamp:new(to_h, to_m, to_seconds(to_s, to_ms))
	end

	local new = self:create(filename)
	local entry = self.entry()
	local f_idx, idx = 1, 1
	for _, line in pairs(self:parse_file(filename)) do
		if idx == 1 then
			assert(line:match("^%d+$"), string.format("SRT FORMAT ERROR (line %d): expected a number but got '%s'", f_idx, line))
			entry.index = line
		elseif idx == 2 then
			assert(line:match("^%d+:%d+:%d+,%d+ %-%-> %d+:%d+:%d+,%d+$"), string.format("SRT FORMAT ERROR (line %d): expected a timecode string but got '%s'", f_idx, line))
			local t_start, t_end = parse_timestamp(line)
			entry.start_time, entry.end_time = t_start, t_end
		else
			if #line == 0 then -- end of text
				table.insert(new.entries, entry)
				entry = SRT.entry()
				idx = 0
			else
				table.insert(entry.text, line)
			end
		end
		idx = idx + 1
		f_idx = f_idx + 1
	end
	return new
end

function SRT:toString()
	local stringbuilder = {}
	local function append(s)
		table.insert(stringbuilder, s)
	end
	for _,entry in pairs(self.entries) do
		append(entry.index)
		local timestamp_string = string.format("%s --> %s", entry.start_time:toString(","), entry.end_time:toString(","))
		append(timestamp_string)
		if type(entry.text) == 'table' then
			append(table.concat(entry.text, "\n"))
		else append(entry.text) end
		append('')
	end
	return table.concat(stringbuilder, '\n')
end

local ASS = inheritsFrom(AbstractSubtitle)
ASS.header_mapper = { ["Start"] = "start_time", ["End"] = "end_time" }

function ASS.valid_entry(entry)
	return entry['type'] ~= nil
end

function ASS:toString()
	local stringbuilder = {}
	local function append(s) table.insert(stringbuilder, s) end
	append(self.header)
	append('[Events]')
	for i=1, #self.entries do
		if i == 1 then
			-- stringbuilder for events header
			local event_sb = {}; for _,v in pairs(self.event_header) do table.insert(event_sb, v) end
			append(string.format("Format: %s", table.concat(event_sb, ", ")))
		end
		local entry = self.entries[i]
		local entry_sb = {}
		for _, col in pairs(self.event_header) do
			local value = entry[col]
			local timestamp_entry_column = self.header_mapper[col]
			if timestamp_entry_column then
				value = entry[timestamp_entry_column]:toString(".")
			end
			table.insert(entry_sb, value)
		end
		append(string.format("%s: %s", entry['type'], table.concat(entry_sb, ",")))
	end
	return table.concat(stringbuilder, '\n')
end

function ASS:populate(filename, language)
	local header, events, parser = {}, {}, nil
	for _, line in pairs(self:parse_file(filename)) do
		local _,_,event = string.find(line, "^%[([^%]]+)%]%s*$")
		if event then
			if event == "Events" then
				parser = function(x) table.insert(events, x) end
			else
				parser = function(x) table.insert(header, x) end
				parser(line)
			end
		else
			parser(line)
		end
	end
	-- create subtitle instance
	local ev_regex = "^(%a+):%s(.+)$"
	local function parse_event(header_columns, ev)
		local function create_timestamp(timestamp_str)
			local timestamp_fmt = "^(%d+):(%d+):(%d+).(%d+)"
			local _,_,h,m,s,ms = timestamp_str:find(timestamp_fmt)
			return TimeStamp:new(h,m,TimeStamp.to_seconds(s, ms))
		end
		local new_event = {}
		local _, _, ev_type, ev_values = string.find(ev, ev_regex)
		new_event['type'] = ev_type
		-- skipping last column, since that's the text, which can contain commas
		local last_idx = 0; for i=1, #header_columns-1 do
			local col = header_columns[i]
			local idx = string.find(ev_values, ",", last_idx+1)
			local val = ev_values:sub(last_idx+1, idx-1)
			local timestamp_entry_column = self.header_mapper[col]
			if timestamp_entry_column then
				new_event[timestamp_entry_column] = create_timestamp(val)
			else
				new_event[col] = val
			end
			last_idx = idx
		end
		new_event[header_columns[#header_columns]] = ev_values:sub(last_idx+1)
		return new_event
	end

	local sub = self:create(filename)
	sub.header = table.concat(header, "\n")
	sub.language = language
	-- remove and process first entry in events, which is a header
	local _,_,colstring = string.find(table.remove(events, 1), "^%a+:%s(.+)$")
	local columns = {}; for i in colstring:gmatch("[^%,%s]+") do table.insert(columns, i) end
	sub.event_header = columns
	for _, event in pairs(events) do
		if #event > 0 then
			table.insert(sub.entries, parse_event(columns, event))
		end
	end
	return sub
end

local PGS = inheritsFrom(AbstractSubtitle)
PGS.section_mapper = { PDS = 0x14, ODS = 0x15, PCS = 0x16, WDS = 0x17, [0x14] = 'PDS', [0x15] = 'ODS', [0x16] = 'PCS', [0x17] = 'WDS', [0x80] = 'END' }
PGS.yCbCr_rgb_map = {}

function PGS:populate(filename)
	local f = io.open(filename, "rb")
	local f_data, f_idx = f:read("a"), 1
	f:close()

	local function create_iterable(size)
		-- return iterator which takes the amount of bytes that should be returned as parameter
		if f_idx + size - 1 > #f_data then return nil end
		local index, byte_str = 1, f_data:sub(f_idx, f_idx + size - 1)
		assert(size == #byte_str, string.format("not equal! size (%d) ~= #byte_str (%d)", size, #byte_str))
		f_idx = f_idx + size
		return function (amount_of_bytes)
			if index <= #byte_str then
				index = index + amount_of_bytes
                assert(index - 1 <= size, string.format("Trying to read beyond the size given (%d) when calling create_iterable()", size))
				return string.pack(string.format("=c%d", amount_of_bytes), byte_str:sub(index - amount_of_bytes, index - 1))
			end
		end
	end


	local pgs_segment = {}
	local mt_pgt_sgmt = {
		__name = "PGS SEGMENT",
		__index = pgs_segment,
		__tostring = function (x)
			local sb = {}
			table.insert(sb, string.format("------ %s ------", x.name))
			for _,v in ipairs(x.data) do
				local xv, xv_string = x.data[v], nil
				if type(xv) == 'table' then
					xv_string = "\n\t" .. tostring(xv)
				elseif type(xv) == 'string' then
					xv_string = #xv > 10 and string.format("%d bytes", #xv) or string.format("0x%06X\t(%d)", x:get_numeric(v), x:get_numeric(v))
				end
				table.insert(sb, string.format("%-25s %s", v .. ":", xv_string))
			end
			local sep = {}; for _=1,#sb[1] do table.insert(sep, "-") end
			return table.concat(sb, '\n')
		end,
	}
	function pgs_segment.create(name)
		return setmetatable({ name = name, data = {} }, mt_pgt_sgmt)
	end

	function pgs_segment:add(key, value)
		table.insert(self.data, key)
		self.data[key] = value
	end

	function pgs_segment:get(key, ...)
		local results = { self.data[key] }
		for i,k in pairs({select(1, ...)}) do
			results[i+1] = self.data[k]
		end
		return table.unpack(results)
	end

	function pgs_segment:get_numeric(key, ...)
		local function convert(value)
			return string.unpack(string.format(">I%d", #value), value)
		end
		local results = { convert(self.data[key]) }
		for i,k in pairs({select(1, ...)}) do
			results[i+1] = convert(self.data[k])
		end
		return table.unpack(results)
	end

	function pgs_segment:dump()
		local x = {}
		for i,v in ipairs(self.data) do
			x[i] = v
		end
		return table.concat(x)
	end

	local function parse_pcs(segment_size)
		local seg = pgs_segment.create("Presentation Composition Segment")
		local iter = create_iterable(segment_size) 				-- pcs size = 2+2+1+2+1+1+1+1 = 11
		local function parse_window_information_object()
			local segment = pgs_segment.create("Window Information Object")
			segment:add('object_id', iter(2)) 					-- ID of ODS segment that defines image to be shown
			segment:add('window_id', iter(1)) 					-- ID of the WDS segment to which the image is allocated in the PCS. Up to two images may be assigned to one window
			segment:add('obj_cropped_flag', iter(1)) 			-- 0x40: Force display of the cropped image object, 0x00: Off
			segment:add('obj_horizontal_pos', iter(2)) 			-- X offset from the top left pixel of the image on the screen
			segment:add('obj_vertical_pos', iter(2)) 			-- Y offset from the top left pixel of the image on the screen
			if segment:get_numeric('obj_cropped_flag') == 0x40 then 	-- these flags are only used if obj_cropped_flag is ON
				segment:add('obj_cropping_hor_pos', nil) 				-- X offset from the top left pixel of the cropped object in the screen.
				segment:add('obj_cropping_ver_pos', nil) 				-- Y offset from the top left pixel of the cropped object in the screen.
				segment:add('obj_cropping_width', nil) 					-- Width of the cropped object in the screen.
				segment:add('obj_cropping_height_pos', nil) 			-- Height of the cropped object in the screen.
			end
			return segment
		end
		seg:add('width', iter(2)) 				-- video width in pixels
		seg:add('height', iter(2)) 				-- video height in pixels
		seg:add('frame_rate', iter(1)) 			-- always 0x10
		seg:add('comp_number', iter(2)) 		-- number of this composition
		seg:add('comp_state', iter(1)) 			-- 0x00: Normal, 0x40: Acquisition Point, 0x80: Epoch start
		seg:add('palette_update_flag', iter(1)) -- boolean describing whether this PCS describes a Palette only Display Update, 0x00: false, 0x80: true
		seg:add('palette_id', iter(1)) 			-- ID of palette to be used in Palete only Display Update
		seg:add('number_comp_objects', iter(1)) -- number of comp objects in this segment
		if seg:get_numeric('number_comp_objects') > 0 then
			local comp_objects_seg = pgs_segment.create("Composition Objects")
			for i=1, seg:get_numeric('number_comp_objects') do
				comp_objects_seg:add(string.format("Composition Object %d", i), parse_window_information_object())
			end
			seg:add('composition_objects', comp_objects_seg)
		end
		assert(iter(1) == nil, "Iterator should be empty!")
		return seg
	end

	local function parse_wds(segment_size)
		local iter = create_iterable(segment_size) 				-- wds 1+1+2+2+2+2 = 10 bytes
		local seg = pgs_segment.create("Window Definition Segment")
		seg:add('number_of_windows', iter(1)) 					-- Number of windows defined in this segment
		seg:add('window_list', pgs_segment.create("Window List"))
		for i=1,seg:get_numeric('number_of_windows') do
			local window_seg = pgs_segment.create("Window Segment")
			window_seg:add('window_id', iter(1))
			window_seg:add('window_hor_pos', iter(2)) 			-- X offset from the top left pixel of the window in the screen.
			window_seg:add('window_ver_pos', iter(2)) 			-- Y offset from the top left pixel of the window in the screen.
			window_seg:add('window_width', iter(2))
			window_seg:add('window_height', iter(2))
			seg:get('window_list'):add(string.format("Window Segment %d", i), window_seg)
		end
		assert(iter(1) == nil, "Iterator should be empty!")
		return seg
	end

	local function parse_pds(segment_size)
		local iter = create_iterable(segment_size)
		local segment = pgs_segment.create("Palette Definition Segment")
		segment:add('palette_id', iter(1))					-- ID of the palette
		segment:add('palette_version_number', iter(1)) 		-- Version of this palette within the Epoch
		segment:add('palette_entries', {})
		for _=1, (segment_size-2)/5 do
			local seg = pgs_segment.create("Palette Entry")
			seg:add('palette_entry_id', iter(1)) 			-- Entry number of the palette
			seg:add('luminance', iter(1)) 					-- Luminance (Y value)
			seg:add('color_difference_red', iter(1)) 		-- Color Difference Red (Cr value)
			seg:add('color_difference_blue', iter(1)) 		-- Color Difference Blue (Cb value)
			seg:add('transparency', iter(1)) 				-- Transparency (Alpha value)
			table.insert(segment:get('palette_entries'), seg)
		end
		assert(iter(1) == nil, "Iterator should be empty!")
		return segment
	end

	local function parse_ods(segment_size)
		local iter = create_iterable(segment_size)
		local segment = pgs_segment.create("Object Definition Segment")
		segment:add('object_id', iter(2)) 				-- ID of this object
		segment:add('object_version_number', iter(1)) 	-- Version of this object
		-- If the image is split into a series of consecutive fragments, the last fragment has this flag set. Possible values:
			-- 0x40: Last in sequence
			-- 0x80: First in sequence
			-- 0xC0: First and last in sequence (0x40 | 0x80)
		segment:add('last_in_sequence_flag', iter(1))
		segment:add('object_data_length', iter(3)) 		-- The length of the RLE data buffer with the compressed image data.
		segment:add('width', iter(2)) 					-- Width of the image
		segment:add('height', iter(2)) 					-- Height of the image
		local len = segment:get_numeric('object_data_length')
		-- for some reason 'object_data_length' also counts the 4 bytes used for the width and the height
		segment:add('object_data', iter(len - 4)) 		-- Image data compressed using Run-length Encoding (RLE)
		assert(iter(1) == nil, "Iterator should be empty!")
		return segment
	end

	local segment_type_map = { [0x14] = parse_pds, [0x15] = parse_ods, [0x16] = parse_pcs, [0x17] = parse_wds, [0x80] = function() return nil end }
	local function parse_header(segment_size)
		local seg = pgs_segment.create("Display Set Header")
		local iter = create_iterable(segment_size) 		-- header size = 2+4+4+1+2 = 13 bytes
		if iter then
			seg:add('magic_number', iter(2)) 			-- always 'PG'
			seg:add('presentation_timestamp', iter(4)) 	-- sub picture shown on screen
			seg:add('decoding_timestamp', iter(4)) 		-- time decoding picture starts (always 0 in practice)
			seg:add('segment_type', iter(1)) 			-- byte indicating following segment ( see segment_type_map )
			seg:add('segment_size', iter(2)) 			-- size of following segment
			assert(iter(1) == nil, "Iterator should be empty!")
			return seg
		end
	end

	local sub = self:create(filename)
	-- segment consist of header and related data (except for END (0x80) header, which has no extra data)
	local header, data = parse_header(13), nil
	local display_set = pgs_segment.create("Display Set 0x00")
	local ds_fmt = "%s %s" -- e.g.: PDS Header, WDS Definition
	while header do
		local size, type_ = header:get_numeric('segment_size', 'segment_type')
		assert(header:get('magic_number') == 'PG', "Magic number was not 'PG', corrupted sub file!")
		data = segment_type_map[type_](size)
		display_set:add(ds_fmt:format(PGS.section_mapper[type_], 'Header'), header)
		if data == nil then
			table.insert(sub.entries, display_set)
			display_set = pgs_segment.create(ds_fmt:format("Display Set", string.format("0x%06X", f_idx - 1)))
		else
			display_set:add(ds_fmt:format(PGS.section_mapper[type_], 'Definition'), data)
		end
		header = parse_header(13)
	end
	table.insert(sub.entries, display_set)
    return sub
end

function PGS:decode_lre(data)
	local function conv_numeric(byte_table)
		local total, bit_shifter = 0, 0
		for i=#byte_table,1,-1 do
			total = total + ( byte_table[i] << bit_shifter )
			bit_shifter = bit_shifter + 8 -- size of byte
		end
		return total
	end

    local function make_iter()
        local idx = 0
        return function ()
            idx = idx + 1
            if idx <= #data then
				return data:byte(idx,idx)
            end
        end
    end
    local iter = make_iter()

	local function erase_indicator_bits(byte)
		-- indicator is first 2 digits of byte, so flip those to 0
		local mask = ~(3 << 6) -- 00111111
		return byte & mask
	end

    local lines = {}
    local segments = {}
    local last_byte = iter()
    while last_byte do
        if last_byte ~= 0 then
			-- CCCCCCCC (1 pixel in color C)
            table.insert(segments, { color = last_byte, pixel_count = 1 })
		else
        	local byte = iter()
            if byte == 0 then
                -- last byte == 0 and byte == 0: end of line
                table.insert(lines, segments)
                segments = {}
            else
                -- last_byte == 0 and byte != 0, so next x bytes are part of current segment
                -- eg: byte = 01000100: check first 2 digits
                -- - 00 (0): segment ends at this byte (including itself)
                -- - 01 (1): segment ends at next byte (including itself)
                -- - 10 (2): segment ends at next byte (including itself)
                -- - 11 (3): segment ends byte after next byte (including itself)
                local pixel_count, color = nil, nil
                local shift, result = byte >> 6, erase_indicator_bits(byte)
                if shift == 0 then -- 00000000 00LLLLLL (L pixels in color 0)
                    color = 0
                    pixel_count = result
                elseif shift == 1 then -- 00000000 01LLLLLL LLLLLLLL (L pixels in color 0)
                    color = 0
                    local c = { result, iter() }
                    pixel_count = conv_numeric(c)
                elseif shift == 2 then -- 00000000 10LLLLLL CCCCCCCC (L pixels in color C)
                    pixel_count = result
                    color = iter()
                elseif shift == 3 then -- 00000000 11LLLLLL LLLLLLLL CCCCCCCC (L pixels in color C)
                    pixel_count = conv_numeric({ result, iter() })
                    color = iter()
                end
                table.insert(segments, { color = color, pixel_count = pixel_count })
            end
        end
        last_byte = iter()
    end
    return lines
end

function PGS:dump_image(idx, filename)
	assert(idx <= #self.entries, "Invalid index!")

	local ods_def = self.entries[idx]:get("ODS Definition")
	local pds_def = self.entries[idx]:get("PDS Definition")

	if ods_def == nil or pds_def == nil then return end
	local palette_entries = pds_def:get('palette_entries')

	local function yuv_to_rgb(id)
		local function compute(y, cb, cr, a)
			local function clamp(x)
				if x < 0 then return 0
				elseif x > 255 then return 255
				end
				return math.floor(x + 0.5)
			end
			-- https://www.fourcc.org/fccyvrgb.php
			local r,g,b
			r = clamp(y + (1.370705 * (cr-128)))
			g = clamp(y - (0.698001 * (cr-128)) - (0.337633 * (cb-128)))
			b = clamp(y + (1.732446 * (cb-128)))
			return string.pack("=BBBB", r, g, b, a)
		end
		-- Fall back on custom tranparent entry if ID was out of range
		local p = palette_entries[id+1] -- entries are 0 indexed
		if p == nil then
			return string.pack("=BBBB", 0, 0, 0, 0)
		end
		local y,cb,cr,a = p:get_numeric('luminance', 'color_difference_blue', 'color_difference_red', 'transparency')
		local yCbCr_packed = string.pack("=BBBB", y, cb, cr, a)
		self.yCbCr_rgb_map[yCbCr_packed] = self.yCbCr_rgb_map[yCbCr_packed] or compute(y, cb, cr, a)
		return self.yCbCr_rgb_map[yCbCr_packed]
	end
	local image_data = ods_def:get('object_data')
	local decoded_data = self:decode_lre(image_data)
	local pixels = {}
	for _,line in pairs(decoded_data) do
		for _,segment in pairs(line) do
			table.insert(pixels, string.rep(yuv_to_rgb(segment.color), segment.pixel_count))
		end
	end
	local f = io.open(filename, 'wb')
	f:write(string.pack("=I2I2", ods_def:get_numeric('width', 'height')))
	f:write(table.concat(pixels))
	f:close()
end

P.AbstractSubtitle = AbstractSubtitle
P.ASS = ASS
P.SRT = SRT
P.PGS = PGS
return P
