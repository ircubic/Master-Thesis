class Entity(object):
    """An entity in the simulation.
    """

    def __init__(self, position, speed):
        """Set the entity's initial state.

        Arguments:
        - `position`: the entity's start position
        - `speed`: the entity's movement speed
        """
        self._position = list(position)
        self._speed = speed
        self._last_direction = ""


    def move(self, direction):
        """Move in the designated direction.

        Moves the entity one step in the given direction, with the distance
        given by the entity's speed.

        Arguments:
        - `direction`: The direction to move. One of "left", "right", "up" or "down".
        """
        self._last_direction = direction
        if direction == 'left':
            self._position[0] -= self._speed
        elif direction == 'right':
            self._position[0] += self._speed
        elif direction == 'up':
            self._position[1] -= self._speed
        elif direction == 'down':
            self._position[1] += self._speed


    def getPosition(self, ):
        """Gets the entity's position
        """
        return tuple(self._position)
