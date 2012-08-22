import pprint
import math
import numpy

def _T(ticks, p1):
    """

    Arguments:
    - `ticks`: An array of the ticks it took to win each game
    """
    return (1.0 - (numpy.average(ticks)/numpy.amax(ticks)))**p1

def _S(ticks, p2, tmax=50, tmin=3):
    """

    Arguments:
    - `ticks`:
    - `p2`:
    - `tmax`:
    - `tmin`:
    """
    N = float(len(ticks))
    std_max = 0.5*math.sqrt(N/(N-1.0))*(tmax-tmin)
    std_ticks = numpy.std(ticks)
    return (std_ticks/std_max)**p2

def _Hn(cells, p3):
    Vn = float(numpy.sum(cells))
    value = 0.0
    for row in cells:
        for cell in row:
            if cell > 0:
                value += (cell/Vn)*math.log10(cell/Vn)
    value = -(1.0/math.log10(Vn))*value
    return value

def _H(dogvisits, p3):
    accum = 0.0
    N = 0
    for visits in dogvisits:
        for cells in visits:
            Hn = _Hn(cells, p3)
            accum += Hn
            N += 1
    return accum/N

def interest(ticks, dogvisits, gamma, delta, epsilon):
    """
    """
    p1 = 1.0
    p2 = 1.0
    p3 = 1.0
    top = (gamma*_T(ticks, p1) + delta*_S(ticks, p2) + epsilon*_H(dogvisits, p3))
    return top / (gamma+delta+epsilon)
