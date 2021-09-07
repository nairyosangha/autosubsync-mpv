st = require("subtitle")

local fn = "./JPN_MULTICOLOR.pgs"
print(fn)
local sub = st.PGS:populate(fn)
local pds_idx = st.PGS.section_mapper['PDS']

--print(#sub, pds_idx)
--print(sub.entries[1][pds_idx].segment)
--for _,v in pairs(sub.entries[5][pds_idx].segment:get('palette_entries')) do print(v) end
--local lines = st.PGS:decode_lre(sub.entries[1][0x15].segment.data.object_data)
--for i,line in ipairs(lines) do
--	local line_width = 0
--	--print(string.format("----- LINE %d ------", i))
--	for j,segment in pairs(line) do
--		line_width = line_width + segment.pixel_count
--		print(string.format("\t ----- SEGMENT %d ------", j))
--		for k,v in pairs(segment) do
--			print("\t", k, v)
--		end
--	end
--	print(string.format("----- LINE %d WIDTH %d ------", i, line_width))
--end

for i,e in pairs(sub.entries) do
	for k,v in pairs(e) do
		print(string.format("PDS %d, part %d\n%s\n%s", i, k, v.header, v.segment))
	end
	sub:dump_image(i, string.format("raw/%s_pixels_%05d.bin", fn, i))
	if i > 20 then break end
	io.read()
end
