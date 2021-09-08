st = require("subtitle")

local fn = "./JPN_MULTICOLOR.pgs"
local sub = st.PGS:populate(fn)

for i,displayset in ipairs(sub.entries) do
	--print(displayset)
	print(i)
	sub:dump_image(i, string.format("raw/%s_pixels_%05d.bin", fn, i))
end
