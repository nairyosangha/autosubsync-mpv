st = require("subtitle")

local sub = st.PGS:populate("./Bubblegum_Crisis_01-jp-color.sup")
print(#sub)
print(sub[1][0x15].header)
--print(sub[1][0x15].segment.data.object_data)
local lines = st.PGS:decode_lre(sub[1][0x15].segment.data.object_data)
for i,line in ipairs(lines) do
	print(string.format("----- LINE %d ------", i))
	for j,segment in pairs(line) do
		print(string.format("\t ----- SEGMENT %d ------", j))
		for k,v in pairs(segment) do
			print("\t", k, v)
		end
	end
end

