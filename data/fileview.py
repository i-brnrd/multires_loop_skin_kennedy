#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov 17 12:03:40 2020

@author: louise
"""
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LogNorm



fd = open('writetest.dat', 'rb')
datain=np.fromfile(file=fd, dtype=np.double)
fd.close()

n=int(len(datain)/2)


data=datain.reshape(2,n)
    
value=[]
wavelength=[]

#for i in range(len(data)):
 #   v=float(data[i][1])
  #  value.append(v) 
    
#for i in range(len(data)):
 #   v=float(data[i][0])
  #  wavelength.append(v)   
    
print(data)