import pygame
from pygame.sprite import Sprite
from utils import load_png, normalize

class Goal(Sprite):
    """A class that encapsulates the goal
    """

    def __init__(self, start_pos):
        """

        Arguments:
        - `start_pos`:
        - `image`:
        """
        Sprite.__init__(self)
        self.image = load_png('bow.png')
        self.rect = self.image.get_rect()
        self.rect.midtop = start_pos

    def get_rect(self, ):
        """
        """
        return self.rect.copy()


class Char(Sprite):
    """A class that encapsulates the player character, the cat.
    """
    MOVE_MAP = {
        'left': (-1,0),
        'right': (1,0),
        'up': (0,-1),
        'down': (0,1),
    }

    def __init__(self, start_pos, image, speed):
        """Initialize the cat
        """
        Sprite.__init__(self)
        self.image = load_png(image)
        self.speed = speed
        self.rect = self.image.get_rect()
        self.rect.center = start_pos
        self.area = pygame.display.get_surface().get_rect()

        self.vector = [0,0]

    def get_pos(self, ):
        return self.rect.topleft

    def get_rect(self, ):
        """Get a copy of this character's rectangle
        """
        return self.rect.copy()


    def get_img(self, ):
        return self.image

    def move(self, direction):
        """Start movement in designated direction

        Arguments:
        - `direction`:
        """
        move_vector = self.MOVE_MAP[direction]
        self.vector[0] += move_vector[0]
        self.vector[1] += move_vector[1]

    def stop(self, direction):
        """Stop movement in designated direction

        """
        move_vector = self.MOVE_MAP[direction]
        self.vector[0] -= move_vector[0]
        self.vector[1] -= move_vector[1]

    def update(self, ):
        """Update the state of the kitty
        """
        v = normalize(self.vector)
        newrect = self.rect.move(v[0]*self.speed,
                                 v[1]*self.speed)
        if self.area.contains(newrect):
            self.rect = newrect


class Cat(Char):
    DEFAULT_PIC = 'nyan.png'
    SPEED = 4

    def __init__(self, start_pos):
        """Set up a cat
        """
        Char.__init__(self, start_pos, self.DEFAULT_PIC, self.SPEED)

class Dog(Char):
    DEFAULT_PIC = 'dog.png'
    SPEED = 4

    def __init__(self, start_pos):
        """Set up a cat
        """
        Char.__init__(self, start_pos, self.DEFAULT_PIC, self.SPEED)
