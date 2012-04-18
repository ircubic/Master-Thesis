import random
from math import *
from simulation.chars import *
from simulation.util import *

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
    x_diff = goal.x-current.x
    y_diff = goal.y-current.y
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
        for dog in dogs:
            dogdist = (abs(dog.x-x) + abs(dog.y-y))
            if dogdist > 0:
                cost += 75 / dogdist
            else:
                cost += 2**32
        cost += sqrt((4*(goal.x-x))**2 + (4*(goal.y-y))**2)
        return cost

def potential_field_cat(current, cat, dogs, goal, field):
    """A Potential-Field Based cat AI

    """
    def diravgcost(dogs, goal, begin, end, step):
        def ensureInside(pos):
            radius = cat.radius
            return (
                max(radius,
                    min(pos[0],
                        field[0]-radius)),
                max(radius,
                    min(pos[1],
                        field[1]-radius))
            )

        end = ensureInside(end)
        if begin == end:
            return pfb_cost(begin[0], begin[1], dogs, goal)

        dirvector = (end[0]-begin[0], end[1]-begin[1])
        normlength = sqrt(dirvector[0]**2+dirvector[1]**2)
        motionvector = (dirvector[0]/(normlength * (1.0/step)), dirvector[1]/(normlength * (1.0/step)))
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

    speed = current.getSpeed()
    DIRS = {
        "left": (-speed, 0.0),
        "right": (speed, 0.0),
        "up": (0.0, -speed),
        "down": (0.0, speed)
    }
    mindir = ""
    mincost = 1000000. #Arbitrary high number

    # Order specified to ensure match with ML-code
    for direction in ("right", "left", "up", "down"):
        (vx, vy) = DIRS[direction]
        c = diravgcost(dogs, goal, (cat.x, cat.y), (cat.x+vx, cat.y+vy), 0.25)
        if c < mincost:
            mindir = direction
            mincost = c
    return mindir

def follower_ai(current, cat, dogs, goal, field):
    """Follower AI for Dog.

    Tries to minimize the manhattan distance between self and cat.
    """
    diffx = cat.x - current.x
    diffy = cat.y - current.y
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

# fun f( Self, Cat, Dogs, Goal, Field ) =
#  case
#    collide(
#      circle(getDistance( Self, Goal ),
#             radius(getQuadDistance(Goal, Cat))),
#      Self
#    )
#   of true => right
#    | false => (
#      case
#        collide( Goal,
#                 rect( getDistance( Goal, Cat ), Field ) )
#       of false => up
#        | true => left
#      )
def f(current, cat, dogs, goal, field):
    if collideShapes(Circle(getDistance(current, goal), getQuadDistance(goal, cat)),
                     current.getShape()):
        return 'right'
    else:
        if collideShapes(goal.getShape(),
                         Rect(getDistance(goal, cat), field)):
            return 'up'
        else:
            return 'left'
