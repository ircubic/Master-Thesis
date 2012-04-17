import random
from datetime import datetime

from util import *
import ai
from chars import Entity

class Simulation(object):
    """Represents the simulation part of the test bed.

    This is the part that contains all the logic for the game and handles the
    decision making for the entities.
    """

    CAT_RADIUS = 0.75
    DOG_SIZE = (1.5, 1.5)
    GOAL_SIZE = (5, 2)
    CAT_SPEED = 2.0
    DOG_SPEED = CAT_SPEED*3.0/4.0


    def __init__(self, cat_ai, dog_ai, field_size=(16,16), num_dogs=5):
        """Initialize the simulation.

        Sets up the entities in the simulation (cat and dogs) with the passed in
        AIs. Refer to simulation.ai for predefined AIs as well as the signature
        used for the AI functions.

        Arguments:
        - `cat_ai`: The AI used for the cat.
        - `dog_ai`: The AI used for the dog.
        - `field_size`: The size of the field.
        - `num_dogs`: The amount of dogs to put on the board.
        """
        self._last_tick = datetime.now()
        self._gameover = False
        self._win = False
        self._field_size = field_size

        # Set up the field and place entities within
        height = field_size[1]
        width = field_size[0]
        # We want the cat to start flush with the bottom, in the center
        self._cat = Entity((random.uniform(self.CAT_RADIUS, width-self.CAT_RADIUS),
                            height-self.CAT_RADIUS),
                           self.CAT_SPEED)

        # The goal is flush with the top, also centered
        self._goal = Entity((width/2.0, self.GOAL_SIZE[1]/2.0), 0)

        # The dogs are arranged randomly within the top half of the screen
        self._dogs = []
        dog_padding = (self.DOG_SIZE[0]/2.0,self.DOG_SIZE[1]/2.0)
        x_range = (dog_padding[0], width-dog_padding[0])
        y_range = (dog_padding[1], (height/2.0)-dog_padding[1])
        for i in range(num_dogs):
            dogx = random.uniform(x_range[0], x_range[1])
            dogy = random.uniform(y_range[0], y_range[1])
            self._dogs.append(Entity((dogx, dogy), self.DOG_SPEED))

        self._cat_ai = cat_ai
        self._dog_ai = dog_ai

    def setCatMove(self, direction):
        """Set the cat's move if using ai.ControlAI.

        Arguments:
        - `direction`:
        """
        ai._cat_move = direction

    def getState(self, ):
        """Get the state of the simulation.

        Returns a simplified version of the simulation state, containing only
        the positions of all the entities, whether the game is over and if it
        was won or lost.
        """
        state = {}
        state["updated"] = self._last_tick
        state["cat"] = self._cat.getPosition()
        state["dogs"] = [d.getPosition() for d in self._dogs]
        state["goal"] = self._goal.getPosition()
        state["gameover"] = self._gameover
        state["win"] = self._win
        return state


    def getFieldSize(self, ):
        """Get the size of the game field.
        """
        return self._field_size



    def simtick(self, ):
        """Do one tick of the simulation.
        """
        if not self._gameover:
            self._last_tick = datetime.now()
            moves = self._aiStep()
            self._updateState(moves)
            collisions = self._checkCollisions()
            if collisions:
                self._gameover = True
                if "goal" in collisions:
                    self._win = True
                if "dog" in collisions:
                    self._win = False
        return self.getState()


    def _aiStep(self, ):
        """Do one AI step.

        Call the AI functions for all the entities and gather up their movement
        decisions, then return them.
        """
        state = self.getState()
        moves = []
        moves.append(self._cat_ai(state["cat"], state["cat"],
                                  state["dogs"], state["goal"]))
        for i in range(len(self._dogs)):
            moves.append(self._dog_ai(state["dogs"][i], state["cat"],
                                      state["dogs"], state["goal"]))
        return moves

    def circleBox(self, radius):
        return (radius*2.0, radius*2.0)

    def _updateState(self, moves):
        """Update the positions of entities according to the moves.

        Arguments:
        - `moves`: The list of moves to do. The first element is for the cat,
        the remaining are for the dogs.
        """
        self._cat.move(moves[0])
        self._ensureInside(self._cat, self.circleBox(self.CAT_RADIUS))
        for i, dog in enumerate(self._dogs):
            dog.move(moves[i+1])
            self._ensureInside(dog, self.DOG_SIZE)


    def _ensureInside(self, entity, size):
        """Ensure that the entity is inside the field.

        Arguments:
        - `entity`: The given entity
        - `size`: The bounding box of the entity
        """
        pos = entity.getPosition()
        new_pos = list(pos)

        left = pos[0]-size[0]*0.5
        right = pos[0]+size[0]*0.5
        top = pos[1]-size[1]*0.5
        bottom = pos[1]+size[1]*0.5

        if left < 0:
            new_pos[0] = size[0]*0.5
        elif right > self._field_size[0]:
            new_pos[0] = self._field_size[0]-size[0]*0.5
        else:
            new_pos[0] = pos[0]

        if top < 0:
            new_pos[1] = size[1]*0.5
        elif bottom > self._field_size[1]:
            new_pos[1] = self._field_size[1]-size[0]*0.5
        else:
            new_pos[1] = pos[1]

        entity.setPosition(new_pos)


    def _checkCollisions(self, ):
        """Check for collisions between cat and other entities.
        """
        collisions = []
        cat_circle = self._cat.getPosition() + (self.CAT_RADIUS,)
        goal_rect = self._goal.getPosition() + self.GOAL_SIZE
        for dog in self._dogs:
            dog_rect = dog.getPosition() + self.DOG_SIZE
            if collideCircleWithRect(cat_circle, dog_rect):
                collisions.append("dog")
        if collideCircleWithRect(cat_circle, goal_rect):
            collisions.append("goal")
        return collisions


if __name__ == '__main__':
    from pprint import pprint
    sim = Simulation(ai.random_ai, ai.follower_ai, num_dogs=4)
    pprint(sim.getState())
    sim.simtick()
    pprint(sim.getState())
    sim.simtick()
    pprint(sim.getState())
