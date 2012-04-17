import random
from math import *


_cat_move = ""

def control_ai(current, cat, dogs, goal, field):
    """Control AI.

    This AI fetches the cat move set by the simulation's "setCatMove", allowing
    for external control of the AI, such as through keyboard input.
    """
    return _cat_move


def random_ai(current, cat, dogs, goal, field):
    """Randomly moving AI

    """
    return random.choice(('left','right','up','down'))

def exit_achiever(current, cat, dogs, goal, field):
    """Exit-achieving cat

    """
    x_diff = goal[0]-cat[0]
    y_diff = goal[1]-cat[1]
    if abs(x_diff) > abs(y_diff):
        if x_diff > 0:
            return 'right'
        else:
            return 'left'
    else:
        if y_diff > 0:
            return 'down'
        else:
            return 'up'

def pfb_cost(x,y,dogs,goal):
        cost = 0.0
        for dogx, dogy in dogs:
            cost += 75 / (abs(dogx-x) + abs(dogy-y))
        cost += sqrt((4*(goal[0]-x))**2 + (4*(goal[1]-y))**2)
        return cost

def potential_field_cat(current, cat, dogs, goal, field):
    """A Potential-Field Based cat AI

    """
    def diravgcost(dogs, goal, begin, end, step):
        def ensureInside(pos):
            return (
                max(0.75, #HAX
                    min(pos[0],
                        field[0]-0.75)), #HAX
                max(0.75, #HAX
                    min(pos[1],
                        field[1]-0.75)) #HAX
            )

        end = ensureInside(end)
        if begin == end:
            return pfb_cost(begin[0], begin[1], dogs, goal)

        dirvector = (end[0]-begin[0], end[1]-begin[1])
        normlength = sqrt(dirvector[0]**2+dirvector[1]**2)
        motionvector = (dirvector[0]/(normlength* (1.0/step)), dirvector[1]/(normlength * (1.0/step)))
        x,y = begin
        steps = 0
        costsum = 0.0
        going = True
        while going:
            c = pfb_cost(x, y, dogs, goal)
            costsum += c
            steps += 1
            x += motionvector[0]
            y += motionvector[1]
            traveled = sqrt((x-begin[0])**2 + (y-begin[1])**2)/normlength
            if traveled > 1.001:
                going = False
        return costsum/steps
    DIRS = {
        "left": (-2.0, 0.0), #HAX
        "right": (2.0, 0.0), #HAX
        "up": (0.0, -2.0), #HAX
        "down": (0.0, 2.0) #HAX
    }
    mindir = ""
    mincost = 1000000. #Arbitrary high number

    # Order specified to ensure match with ML-code
    for direction in ("right", "left", "up", "down"):
        v = DIRS[direction]
        c = diravgcost(dogs, goal, cat, (cat[0]+v[0], cat[1]+v[1]), 0.5)
        if c < mincost:
            mindir = direction
            mincost = c
    return mindir

def follower_ai(current, cat, dogs, goal, field):
    """Follower AI for Dog.

    Tries to minimize the manhattan distance between self and cat.
    """
    diffx = cat[0] - current[0]
    diffy = cat[1] - current[1]
    if abs(diffx) > abs(diffy):
        if diffx > 0:
            return 'right'
        else:
            return 'left'
    else:
        if diffy > 0:
            return 'down'
        else:
            return 'up'
