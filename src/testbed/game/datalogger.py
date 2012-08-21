import numpy

class _gamestats:

    def __init__(self, width, height, numdogs):
        """
        """
        self.won = False
        self.ticks = 0
        self.entropy = [ self.emptyEntropy(width, height)
                         for i in range(numdogs) ]

    def emptyEntropy(self, width, height):
        """

        Arguments:
        - `width`:
        - `height`:
        """
        return numpy.zeros((width, height), dtype=int)

    def setEntropy(self, dogs):
        for dog, entropy in zip(dogs, self.entropy):
            x = round(dog[0])
            y = round(dog[1])
            entropy[y][x] += 1


class GameDataLogger:

    def __init__(self, ):
        """
        """
        self.games = []
        self.currentgame = None

    def gameStarted(self, field, dogs):
        """

        Arguments:
        - `field`:
        """
        self.currentgame = _gamestats(field[0], field[1], len(dogs))
        self.games.append(self.currentgame)


    def gameTicked(self, state):
        """

        Arguments:
        - `state`:
        """

        self.currentgame.ticks += 1
        self.currentgame.setEntropy(state["dogs"])


    def gameEnded(self, won):
        """

        Arguments:
        - `won`:
        """
        self.currentgame.won = won
        self.currentgame = None

    def calculateInterest(self, ):
        """
        """
        return 0

    def getStats(self, x, y, z):
        """
        """
        N = len(self.games)
        wins = sum([1 if x.won else 0 for x in self.games])
        interest = self.calculateInterest()
        return N, wins, interest
