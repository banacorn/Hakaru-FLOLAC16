#! /usr/bin/env python3

import argparse
import numpy as np
import matplotlib.pyplot as plt

parser = argparse.ArgumentParser(description='plotting data')
parser.add_argument('filename')

def plot(filename):
    data = []
    with open(filename) as f:
        next(f)
        for l in f:
            x, y = [float(d) for d in l.strip().split()]
            data.append((x, y))

    data = np.array(data)

    plt.plot(data[:,0], data[:,1], drawstyle='steps', linewidth=2)
    plt.show()

if __name__ == '__main__':
    args = parser.parse_args()
    plot(args.filename)
