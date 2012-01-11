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
