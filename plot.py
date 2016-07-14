#! /usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt

data = []
with open('plot.dat') as f:
    next(f)
    for l in f:
        x, y = [float(d) for d in l.strip().split()]
        data.append((x, y))

data = np.array(data)

plt.plot(data[:,0], data[:,1])
plt.show()
