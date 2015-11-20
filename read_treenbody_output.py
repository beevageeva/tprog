import re
import sys

with open(sys.argv[1]) as f:
	modelNumber = 2
	marr = re.split('-----+', f.read())
	fnbase = "Model%d.txt"
	for m in marr:
		m = m.strip()
		if m!="":
			print("model: %d" % modelNumber)
			with open(fnbase % modelNumber, "w") as wf:
				wf.write(m)
			modelNumber+=1
