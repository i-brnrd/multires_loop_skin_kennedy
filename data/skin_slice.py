import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm



depth=[]
n = 100
fd = open('jmean222.dat', 'rb')
datain=np.fromfile(file=fd, dtype=np.double).reshape(801,n,n)
fd.close()
data=np.flipud(datain)

xmax=0.05
ymax=0.05
zmax=0.25

a=4
b=2

#for i in range(n):
 #   z = (i-1)*2.*xmax/n
  #  for j in range(n):
   #     y=(j-1)*2.*ymax/n
    #    for k in range(n):
     #       x=(k-1)*2.*zmax/n 
      #      if np.sqrt(a**2*(x-0.05)**2+b**2*(y-0.05)**2) < -(z-0.075): 
       #     	datain[i,j,k]=0

            	


zlength=0.5 #cm
znumber=800
zvoxel_length=zlength/znumber #cm

dslice=data[799,1:n,1:n] #use data for voxel number axis and datain for axis produced in python like depth. 

print(dslice)

for i in range(znumber):
	idepth=(i*zvoxel_length) #cm
	depth.append(idepth)
    
print(np.count_nonzero(np.max(dslice)))
	

image=plt.imshow(dslice,extent=[min(depth),max(depth),min(depth),max(depth)], norm=LogNorm())
plt.xlabel('x (cm)') 
plt.ylabel('z (cm)') 
plt.gca().invert_yaxis()
plt.title('Fluence through a Cut Irradiated by Direct 222nm')
plt.colorbar()
plt.show()






