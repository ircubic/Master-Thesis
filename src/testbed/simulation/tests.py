import unittest
from datetime import datetime
from simulation.chars import Entity
from simulation import Simulation

class TestEntity(unittest.TestCase):

    def setUp(self, ):
        """
        """
        self.position = [1,2]
        self.speed = 1.5
        self.entity = Entity(self.position, self.speed)

    def testGetPosition(self):
        pos = self.entity.getPosition()
        self.assertSequenceEqual(pos, self.position)

    def testNoStateExposure(self, ):
        """
        """
        pos_changed = self.entity.getPosition()
        self.assertTrue(isinstance(pos_changed,tuple))

    def testMove(self):
        pos = list(self.entity.getPosition())
        pos[0] -= self.speed
        self.entity.move('left')
        self.assertSequenceEqual(pos, self.entity.getPosition())
        pos[0] += self.speed
        self.entity.move('right')
        self.assertSequenceEqual(pos, self.entity.getPosition())
        pos[1] += self.speed
        self.entity.move('down')
        self.assertSequenceEqual(pos, self.entity.getPosition())
        pos[1] -= self.speed
        self.entity.move('up')
        self.assertSequenceEqual(pos, self.entity.getPosition())


class TestSimulation(unittest.TestCase):
    """Tests for the Simulation class
    """

    class mock_ai(object):
        """A mock AI callable for testing
        """

        def __init__(self, ):
            self.calls = 0
            self.cat = []
            self.goal = []
            self.dogs = []

        def __call__(self, current, cat, dogs, goal):
            """AI call. Stores data about the call and returns a default direction.

            Arguments:
            - `cat`: The cat position
            - `dogs`: The dog positions
            - `goal`: The goal position
            """
            self.dogs.append(dogs)
            self.cat.append(cat)
            self.goal.append(goal)
            self.calls += 1
            return "left"

    def setUp(self, ):
        """Set up a default starting state
        """
        self.cat_ai = self.mock_ai()
        self.dog_ai = self.mock_ai()
        self.DOGS = 5
        self.FIELDSIZE = (16,16)
        self.sim = Simulation(self.cat_ai, self.dog_ai,
                              field_size=self.FIELDSIZE, num_dogs=self.DOGS)

    def _all_in_stage(self, state):
        self.assertTrue(self._in_stage(state["cat"], self.sim.CAT_SIZE))
        for dog in state["dogs"]:
            self.assertTrue(self._in_stage(dog, (self.sim.DOG_RADIUS*2,
                                                 self.sim.DOG_RADIUS*2)))
        self.assertTrue(self._in_stage(state["goal"], self.sim.GOAL_SIZE))

    def _in_stage(self, x, size):
        return (x[0]-(size[0]*0.5) >= 0 and
                x[0]+(size[0]*0.5) <= self.FIELDSIZE[0] and
                x[1]-(size[1]*0.5) >= 0 and
                x[1]+(size[1]*0.5) <= self.FIELDSIZE[1])


    def testGetState(self, ):
        """Tests the getState function.

        Checks that the correct members exist in the state, and that they are of
        correct type.
        """

        state = self.sim.getState()
        self.assertItemsEqual(state.keys(), ["cat", "dogs", "gameover", "win", "updated", "goal"])
        self.assertTrue(isinstance(state["cat"], tuple))
        self.assertTrue(isinstance(state["dogs"], list))
        for dog in state["dogs"]:
            self.assertTrue(isinstance(dog, tuple))
        self.assertTrue(isinstance(state["goal"], tuple))
        self.assertTrue(isinstance(state["gameover"], bool))
        self.assertTrue(isinstance(state["win"], bool))
        self.assertTrue(isinstance(state["updated"], datetime))


    def testInit(self, ):
        """Test that the simulation is initialized correctly
        """

        state = self.sim.getState()
        self.assertEqual(len(state["dogs"]), self.DOGS)
        self.assertEqual(state["gameover"], False)
        self.assertEqual(state["win"], False)
        self._all_in_stage(state)


    def testSimtickWin(self, ):
        """Test the simtick method for a winning move
        """
        # Move cat to the right of the goal (so the ai moving left will win)
        goalright = self.sim._goal.getPosition()[0]+self.sim.GOAL_SIZE[0]*0.5
        self.sim._cat.setPosition([goalright+self.sim.CAT_SIZE[0]*0.5,0])
        old_state = self.sim.getState()
        new_state = self.sim.simtick()
        self.assertGreater(new_state["updated"], old_state["updated"])
        self.assertTrue(new_state["gameover"])
        self.assertTrue(new_state["win"])

        # Make sure it stops ticking after game is over
        done_state = self.sim.simtick()
        self.assertEqual(new_state, done_state)


    def testSimtickLose(self, ):
        """Test the simtick method for a losing move
        """
        # Move the cat inbetween the dogs and the left wall, to ensure collision

        dog_pos = self.sim._dogs[0].getPosition()
        dog_pos = (self.sim.CAT_SIZE[0]+self.sim.DOG_RADIUS, dog_pos[1])
        self.sim._dogs[0].setPosition(dog_pos)
        self.sim._cat.setPosition([0, dog_pos[1]])
        state = self.sim.simtick()
        self.assertTrue(state["gameover"])
        self.assertFalse(state["win"])


    def testSimtickWinOrLose(self, ):
        """Test that the simtick method resolves loss properly.

        In a simultaneous loss and win situation, the loss should win out.
        """
        # Move cat so it will hit the goal, then move a dog on top of the cat,
        # so it will kill the cat in the same turn.
        goalright = self.sim._goal.getPosition()[0]+self.sim.GOAL_SIZE[0]*0.5
        newpos = [goalright+self.sim.CAT_SIZE[0]*0.5,0]
        self.sim._cat.setPosition(newpos)
        self.sim._dogs[0].setPosition(newpos)
        state = self.sim.simtick()
        self.assertTrue(state["gameover"])
        self.assertFalse(state["win"])


    def testAiStep(self, ):
        """Test that _aiStep works properly.
        """
        pre_state = self.sim.getState()
        moves = self.sim._aiStep()
        post_state = self.sim.getState()

        # Check that AIs have been called the right amount of times
        self.assertEqual(self.cat_ai.calls, 1)
        self.assertEqual(self.dog_ai.calls, self.DOGS)

        # Check correct return value
        self.assertEqual(moves, ["left"]*(self.DOGS+1))

        # Assert that state has not changed anywhere during the call
        self.assertEqual(pre_state["cat"], self.cat_ai.cat[0])
        self.assertEqual(pre_state["dogs"], self.cat_ai.dogs[0])
        self.assertEqual(pre_state["goal"], self.cat_ai.goal[0])
        for i in range(5):
            self.assertItemsEqual(self.cat_ai.cat[0], self.dog_ai.cat[i])
            self.assertItemsEqual(self.cat_ai.dogs[0], self.dog_ai.dogs[i])
            self.assertItemsEqual(self.cat_ai.goal[0], self.dog_ai.goal[i])
        self.assertEqual(pre_state, post_state)


    def testUpdateState(self, ):
        """Test that _updateState works.
        """
        # Basic sanity check
        expected_state = self.sim.getState()
        expected_state["cat"] = (expected_state["cat"][0], expected_state["cat"][1]-self.sim.CAT_SPEED)
        expected_state["dogs"] = [(x[0], x[1]-self.sim.DOG_SPEED) for x in expected_state["dogs"]]
        moves = ["up"]*(self.DOGS+1)
        self.sim._updateState(moves)
        post_state = self.sim.getState()
        self.assertEqual(expected_state, post_state)
        self._all_in_stage(post_state)

        # Now check that attempting to move outside the stage will move the
        # entity back in flush with the edge
        old_pos = self.sim._dogs[1].getPosition()
        self.sim._dogs[1].setPosition((old_pos[0], self.sim.DOG_RADIUS))
        self.sim._updateState(moves)
        post_state = self.sim.getState()
        self._all_in_stage(post_state)
        self.assertEqual(self.sim._dogs[1]._position[1], self.sim.DOG_RADIUS)

    def testEnsureInside(self, ):
        """Tests that _ensureInside works.
        """
        # Check that ensure inside moves entity back inside, and only touches
        # the proper coordinate, for all sides
        move_base = self.sim.CAT_SIZE[0]*0.5
        field = self.sim._field_size
        for i in range(4):
            # sides in order: left, right, top, bottom
            if i == 0:
                coord = 0
                setto = 0
                movement = move_base
            elif i == 1:
                coord = 0
                setto = field[0]
                movement = -move_base
            elif i == 2:
                coord = 1
                setto = 0
                movement = move_base
            elif i == 3:
                coord = 1
                setto = field[1]
                movement = -move_base

            old_pos = list(self.sim._cat.getPosition())
            old_pos[coord] = setto
            self.sim._cat.setPosition(old_pos)
            self.sim._ensureInside(self.sim._cat, self.sim.CAT_SIZE)
            new_pos = self.sim._cat.getPosition()
            self._all_in_stage(self.sim.getState())
            expected_pos = [old_pos[0], old_pos[1]]
            expected_pos[coord] = setto+movement
            self.assertEqual(tuple(expected_pos), new_pos)

        # Check that entities that are inside are NOT moved
        old_pos = self.sim._goal.getPosition()
        self.sim._ensureInside(self.sim._goal, self.sim.GOAL_SIZE)
        new_pos = self.sim._goal.getPosition()
        self.assertEqual(old_pos, new_pos)


    def testCheckCollisions(self, ):
        """Test that _checkCollisions works
        """
        # Shouldn't collide when starting, returns empty list, but tests False
        self.assertFalse(self.sim._checkCollisions())

        # Move dog onto cat, should collide and return ["dog"]
        pos = self.sim._dogs[0]._position
        pos[0] = self.sim._cat._position[0]
        pos[1] = self.sim._cat._position[1]
        self.assertEqual(self.sim._checkCollisions(), ["dog"])

        # Move goal onto cat, should collide and return ["dog", "goal"]
        pos = self.sim._goal._position
        pos[0] = self.sim._cat._position[0]
        pos[1] = self.sim._cat._position[1]
        self.assertEqual(self.sim._checkCollisions(), ["dog", "goal"])

        # Move the last dog onto the goal, should collide and return
        # ["dog", "dog", "goal"]
        pos = self.sim._dogs[-1]._position
        pos[0] = self.sim._cat._position[0]
        pos[1] = self.sim._cat._position[1]
        self.assertEqual(self.sim._checkCollisions(), ["dog", "dog", "goal"])


    def testCollideRectWithRect(self, ):
        """Test that the rect to rect collision works
        """
        rect = (5, 5, 10, 10)
        overlapping = (12, 12, 4, 4)
        inside = (5, 5, 2, 2)
        outside = (15, 15, 5, 5)

        self.assertTrue(self.sim._collideRectWithRect(rect, rect))
        self.assertTrue(self.sim._collideRectWithRect(rect, overlapping))
        self.assertTrue(self.sim._collideRectWithRect(rect, inside))
        self.assertFalse(self.sim._collideRectWithRect(rect, outside))

    def testCollideCircleWithRect(self, ):
        """Test that the rect to circle collision works
        """
        rect = (5, 5, 10, 10)
        encompasses = (5, 5, 10)
        inside = (5, 5, 2)
        outside = (12, 12, 1)
        corner = (0, 0, 1)
        edgecase = (5, 1, 2)
        self.assertTrue(self.sim._collideCircleWithRect(encompasses, rect))
        self.assertTrue(self.sim._collideCircleWithRect(inside, rect))
        self.assertTrue(self.sim._collideCircleWithRect(corner, rect))
        self.assertTrue(self.sim._collideCircleWithRect(edgecase, rect))
        self.assertFalse(self.sim._collideCircleWithRect(outside, rect))
