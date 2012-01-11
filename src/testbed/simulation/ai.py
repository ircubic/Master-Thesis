import random

_cat_move = ""

def control_ai(current, cat, dogs, goal):
    """Control AI.

    This AI fetches the cat move set by the simulation's "setCatMove", allowing
    for external control of the AI, such as through keyboard input.
    """
    return _cat_move


def random_ai(current, cat, dogs, goal):
    """Randomly moving AI

    """
    return random.choice(('left','right','up','down'))

def follower_ai(current, cat, dogs, goal):
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
