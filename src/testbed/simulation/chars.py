class Shape(object):

    def __init__(self, position):
        self._position = list(position)

    def move(self, direction, speed):
        if direction == 'left':
            self._position[0] -= speed
        elif direction == 'right':
            self._position[0] += speed
        elif direction == 'up':
            self._position[1] -= speed
        elif direction == 'down':
            self._position[1] += speed

    def getPosition(self, ):
        """Gets the entity's position
        """
        return tuple(self._position)

    def setPosition(self, new_pos):
        """Set the entity's position

        Arguments:
        - `new_pos`:
        """
        self._position = list(new_pos)

    def getLeft(self):
        return self._position[0]

    def getRight(self):
        return self._position[0]

    def getTop(self):
        return self._position[1]

    def getBottom(self):
        return self._position[1]

class Rect(Shape):

    def __init__(self, position, size):
        super(Rect, self).__init__(position)
        self.setSize(size)

    def getSize(self):
        return tuple(self._size)

    def setSize(self, new_size):
        self._size = list(new_size)

    def getLeft(self):
        return self._position[0] - (self._size[0]/2.0)

    def getRight(self):
        return self._position[0] + (self._size[0]/2.0)

    def getTop(self):
        return self._position[1] - (self._size[1]/2.0)

    def getBottom(self):
        return self._position[1] + (self._size[1]/2.0)

class Circle(Shape):

    def __init__(self, position, radius):
        super(Circle, self).__init__(position)
        self.setRadius(radius)

    def getRadius(self):
        return self._radius

    def setRadius(self, new_radius):
        self._radius = new_radius

    def getLeft(self):
        return self._position[0] - (self._radius)

    def getRight(self):
        return self._position[0] + (self._radius)

    def getTop(self):
        return self._position[1] - (self._radius)

    def getBottom(self):
        return self._position[1] + (self._radius)

class Entity(object):
    """An entity in the simulation.
    """

    def __init__(self, shape, speed):
        """Set the entity's initial state.

        Arguments:
        - `position`: the entity's start position
        - `speed`: the entity's movement speed
        """
        self._shape = shape
        self.setSpeed(speed)
        self._last_direction = ""


    def move(self, direction):
        """Move in the designated direction.

        Moves the entity one step in the given direction, with the distance
        given by the entity's speed.

        Arguments:
        - `direction`: The direction to move. One of "left", "right", "up" or "down".
        """
        self._last_direction = direction
        self._shape.move(direction, self._speed)


    def getPosition(self, ):
        """Gets the entity's position
        """
        return self._shape.getPosition()

    def setPosition(self, new_pos):
        """Set the entity's position

        Arguments:
        - `new_pos`:
        """
        self._shape.setPosition(new_pos)

    def getShape(self):
        return self._shape

    def setShape(self, new_shape):
        self._shape = new_shape

    def getSpeed(self):
        return self._speed

    def setSpeed(self, new_speed):
        self._speed = float(new_speed)

    @property
    def x(self):
        return self.getPosition()[0]

    @property
    def y(self):
        return self.getPosition()[1]

    @property
    def radius(self):
        return self._shape.getRadius()

    @property
    def size(self):
        return self._shape.getSize()
