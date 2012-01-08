import pygame
import random
import os.path
import math
from pygame.locals import *

from simulation import Simulation


def random_ai(cat, dogs, goal):
    """Randomly moving AI

    """
    return random.choice(('left','right','up','down'))


def load_png(name):
	""" Load image and return image object"""
	fullname = os.path.join('img', name)
	try:
		image = pygame.image.load(fullname)
		if image.get_alpha() is None:
			image = image.convert()
		else:
			image = image.convert_alpha()
	except pygame.error, message:
        	print 'Cannot load image:', fullname
        	raise SystemExit, message
	return image

class InputAI(object):
    """An AI that is intended to work with key inputs.
    """

    def __init__(self, keyinput):
        """Set up the AI with the given InputState.

        Arguments:
        - `keyinput`: An InputState that the AI is to take directions from.
        """
        self._keyinput = keyinput

    def __call__(self, cat, dogs, goal):
        """The actual AI method.

        """
        return self._keyinput.getDirection()



class GameEntity(object):
    """An entity in the game.

    Just a simple container for the entity's image, current game position and
    movement target.

    Game position can differ from simulation position, as the simulation
    position is only updated every n game ticks, whereas the game position is
    updated every game tick. This necessitates interpolation between the
    previous and current sim positions.
    """

    def __init__(self, image, position):
        """Initialize the entity with its image

        Arguments:
        - `image`:
        - `position`:
        """
        self.image = load_png(image)
        self.position = list(position)
        self.current_target = position
        self.rect = self.image.get_rect()
        self.rect.center = self.position

    def getRect(self, ):
        """Get the entity's rect.
        """
        return self.rect

    def updateTarget(self, new_target):
        """Update the entity's movement target

        Arguments:
        - `new_target`:
        """
        self.old_target = self.current_target
        self.current_target = new_target

    def updatePosition(self, percentage):
        """Update the entity's position.

        """
        target_diff = (self.current_target[0] - self.old_target[0],
                       self.current_target[1] - self.old_target[1])
        self.position = [self.old_target[0] + target_diff[0]*percentage,
                         self.old_target[1] + target_diff[1]*percentage]
        self.rect.center = self.position


DIR_MAP = {
    K_UP: 'up',
    K_DOWN: 'down',
    K_LEFT: 'left',
    K_RIGHT: 'right',
}


class InputState(object):
    """Contains the current input state.

    Keeps track of the keys that are currently held down, and the order in which they were pressed, making sure
    that the expressed direction fits what is expected.
    """

    def __init__(self, ):
        """
        """

        self.current_direction = ""
        self.other_directions = []

    def keydown(self, direction):
        """Handle key being pressed

        Arguments:
        - `direction`:
        """
        self.other_directions.append(self.current_direction)
        self.current_direction = str(direction)

    def keyup(self, direction):
        """Handle key being released

        Arguments:
        - `direction`:
        """
        if self.current_direction == direction:
            self.current_direction = self.other_directions.pop()
        elif direction in self.other_directions:
            self.other_directions.remove(direction)

    def getDirection(self, ):
        """Get the current movement direction.

        """
        return self.current_direction



class Game(object):
    """Represents the game part of the test bed.

    This part contains code for displaying the state of the simulation in a way
    that makes it appear like a playable game.
    """

    # Pixels Per (simulation) Unit
    PPU = 30.0
    # Game ticks per sim tick
    TICKFACTOR = 6.0

    CAT_IMG = 'nyan.png'
    DOG_IMG = 'dog.png'
    GOAL_IMG = 'bow.png'

    DEBUG = True

    def __init__(self):
        """
        """
        self.input = InputState()
        self.simulation = Simulation(InputAI(self.input), random_ai)
        self.simstate = self.simulation.getState()
        field = self.simulation.getFieldSize()

        self.screen = pygame.display.set_mode((int(math.ceil(field[0]*self.PPU)),
                                               int(math.ceil(field[1]*self.PPU))))
        pygame.display.set_caption('Dead End')

        bg = pygame.Surface(self.screen.get_size())
        self.background = bg.convert()
        self.background.fill((200,200,200))

        self.font = pygame.font.Font(None, 35)
        self.subfont = pygame.font.Font(None, 25)

        self.clock = pygame.time.Clock()
        self.tickcount = 0
        self.run = True
        self.gameover = False

        self.cat = GameEntity(self.CAT_IMG,
                              self._simToGamePosition(self.simstate["cat"]))
        self.dogs = []
        for dog in self.simstate["dogs"]:
            self.dogs.append(GameEntity(self.DOG_IMG, self._simToGamePosition(dog)))
        self.goal = GameEntity(self.GOAL_IMG,
                               self._simToGamePosition(self.simstate["goal"]))



    def _simToGamePosition(self, position):
        """Convert a simulation position to a game position

        Arguments:
        - `position`:
        """
        return (position[0]*self.PPU, position[1]*self.PPU)


    def mainLoop(self, ):
        """The main loop of the game.
        """
        while self.run:
            self.handleEvents()
            self.updateGameState()
            self.clock.tick(30)
            self.draw()


    def draw(self):
        """
        """
        self.screen.blit(self.background, (0,0))
        self.screen.blit(self.goal.image, self.goal.getRect())
        if self.DEBUG:
            rect2 = pygame.Rect(0,0, 5*self.PPU, 2*self.PPU)
            rect2.center = self.goal.position
            pygame.draw.rect(self.screen, (255,0,0), rect2, 1)
        for entity in self.dogs + [self.cat]:
            rect = entity.getRect()
            self.screen.blit(entity.image, rect)
            if self.DEBUG:
                rect2 = pygame.Rect(0,0, 1.5*self.PPU, 1.5*self.PPU)
                rect2.center = entity.position
                pygame.draw.rect(self.screen, (255,0,0), rect2, 1)
        pygame.display.flip()


    def handleEvents(self):
        """
        """
        for event in pygame.event.get():
            if event.type == QUIT:
                self.run = False
            elif event.type == KEYDOWN:
                if event.key in DIR_MAP:
                    self.input.keydown(DIR_MAP[event.key])
            elif event.type == KEYUP:
                if event.key in DIR_MAP:
                    self.input.keyup(DIR_MAP[event.key])


    def updateGameState(self):
        """
        """
        subtick = self.tickcount % self.TICKFACTOR
        percentage = subtick/self.TICKFACTOR

        # We're done with one round of game ticks, time for next sim tick
        if subtick == 0:
            # If simulation has not finished yet, keep ticking the simulation,
            # otherwise set game's gameover flag and update all entities to
            # final position, as we are on the final tick of this game
            if self.simstate["gameover"] == False:
                self.simstate = self.simulation.simtick()
                self.cat.updateTarget(self._simToGamePosition(self.simstate["cat"]))
                for simdog, gamedog in zip(self.simstate["dogs"], self.dogs):
                    gamedog.updateTarget(self._simToGamePosition(simdog))
            else:
                self.gameover = True
                self.cat.updatePosition(1.0)
                for dog in self.dogs:
                    dog.updatePosition(1.0)

        if not self.gameover:
            self.cat.updatePosition(percentage)
            for dog in self.dogs:
                dog.updatePosition(percentage)

            self.tickcount += 1
