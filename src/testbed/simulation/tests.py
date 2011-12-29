import unittest
from simulation.chars import Entity

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
