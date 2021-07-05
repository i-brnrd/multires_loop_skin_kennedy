#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul 10 17:20:20 2020

@author: louise
"""




import matplotlib.pyplot as plt
import numpy as np
import math as math
import matplotlib.image as mpimg
import glob

depth=[]
data=[]

avhole=[]
holes=[]
holelay=[]

n = 200
h=6.62607004*10**(-30) #cm^2 kg/s
c=2.99792458*10**(10) #cm/s
nphotons=8000000 
#ri=1.38
P=1

zlength=0.1 #cm
znumber=200
zvoxel_length=zlength/(znumber) #cm
vox_vol=(zvoxel_length)**3

L=P*(zlength**2)


#incIr420=((nphotons*h*c)/420*10**(-7))/(zlength**2)
#incIr630=((nphotons*h*c)/630*10**(-7))/(zlength**2)

file_list=glob.glob('jmean*.dat')


fd = open('jmean-400-test.dat', 'rb')
datain=np.fromfile(file=fd, dtype=np.double).reshape(n,n,n)
fd.close()
data=np.flipud(datain)

# average all holes
for i in range(n):
	for j in range(n):
		holev=datain[:400,i,j]
		holes.append(holev)
	
#Do maths to calculate fluence rate from path data
holes=np.asarray(holes)
#j=(holes[20000]*L/(nphotons*vox_vol))
j=holes[1970]

print(j)

#print(j.count(1))

#use data for voxel number axis and datain for axis produced in python like depth. 	
for i in range(znumber):
	idepth=(i*zvoxel_length)*10 #mm
	depth.append(idepth)

depths=np.asarray(depth)

#depth_sc=np.arange(0.0001,0.0201,0.0001)

#depth_b=np.arange(0.02,0.100535,0.000535)

#depth_d=np.arange(0.1,0.50799286, 0.00799286)






#depth=[]

#for i in depth_sc:
   # depth.append(i)
    
#for i in depth_b:
    #depth.append(i)
    
#for i in depth_d:
    #depth.append(i)

plot_depth=depths
plot_fluence=j
#for i in range(n*n):
#	plt.plot(depths, holes250[i])	
#plt.plot(depths, avhole250s)	
plt.plot(plot_depth, plot_fluence)
	
plt.legend()
plt.title('Fluence at 222nm')
#plt.yscale("log")
plt.xlabel('depth(mm)')
plt.ylabel('Normalised fluence rate')
plt.show()