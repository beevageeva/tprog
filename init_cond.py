import numpy as np


from conf_3ranges import ranges
filename = "3ranges.in"
modelfn = "Model1.txt"
from math import pi
before = 0
wf = open(filename, "w")
mf = open(modelfn, "w")
for r in ranges:
	genSize = r['endrange'] - before 
	before = r['endrange']
	if r['shape'] == 'sphere':
		radius = np.random.uniform(0,r['radius'],[genSize])
		theta = np.random.uniform(0,pi,[genSize])
		phi = np.random.uniform(0,pi,[genSize])
		x = radius * np.cos(phi) * np.sin(theta)
		y = radius * np.sin(phi) * np.sin(theta)
		y += r['position']
		z = radius * np.cos(theta)
		#pos = np.stack((x,y,z), axis=-1)
		
		v = r['velocity']
		if v['vtype'] == 'random':
			vel = np.random.uniform(v['start'], v['end'],[genSize,3])
		for i in range(genSize):
			#wf.write("%d %s %s\n" % (r['mass'],  " ".join(np.char.mod('%f',  pos[i,:])),  " ".join(np.char.mod('%f', vel[i,:]))  ))
			wf.write("%f %f %f %f %f %f %f\n" % (r['mass'],  x[i],y[i], z[i], vel[i,0], vel[i,1], vel[i,2] ))
			mf.write("%f %f %f %f %f %f\n" % (x[i],y[i], z[i], vel[i,0], vel[i,1], vel[i,2]))

wf.close()
mf.close()


