import pprint
import math
import numpy

def _T(ticks, p1):
    """

    Arguments:
    - `ticks`: An array of the ticks it took to win each game
    """
    return (1.0 - (numpy.average(ticks)/numpy.amax(ticks)))**p1

def _S(ticks, p2, tmax=50.0, tmin=8.0):
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
    if Vn < 2.0:
        return 0.0
    value = 0.0
    for row in cells:
        for cell in row:
            if cell > 0:
                value += (cell/Vn)*math.log10(cell/Vn)
    value = (-1.0/math.log10(Vn))*value
    return value**p3

def _H(visits, p3):
    v_accum = 0.0
    v_n = 0
    for cells in visits:
        Hn = _Hn(cells, p3)
        v_accum += Hn
        v_n += 1
    return (v_accum/v_n)


def interest(ticks, visits, p1, p2, p3):
    """
    """
    gamma = 1.0
    delta = 2.0
    epsilon = 1.0
    T = _T(ticks, p1)
    S = _S(ticks, p2)
    H = _H(visits, p3)
    print ticks
    print "T =", T, "S =", S, "H =", H
    top = (gamma*T + delta*S + epsilon*H)
    return top / (gamma+delta+epsilon)
