import numpy as np
import math

from csaps import csaps

def fitSpline(X, Y):
    def getW(xs):
        ds = []
        for i in range(len(xs)):
            if i == 0:
                d = 2 * (xs[0] - xs[1])
            elif i == len(xs)-1:
                d = 2 * (xs[-2] - xs[-1])
            else:
                d = xs[i-1] - xs[i+1]
            ds.append(d)
        return ds

    sp = csaps(X, Y, smooth=0.5)
    ddsp = sp.spline.derivative(2)
    ddx = [x for x in ddsp.solve() if ddsp(x-0.01) * ddsp(x+0.01) < 0]
    i = np.argmax(getW(sp(ddx)))
    return ddx[i]

def selectBarcode(args):
    with open(args.input, 'r') as fl:
        Y = [float(l.strip()) for l in fl]
    X = np.log10(np.array(list(range(1, len(Y)+1))))
    Y = np.log10(np.array(Y))
    print(10**fitSpline(X, Y))