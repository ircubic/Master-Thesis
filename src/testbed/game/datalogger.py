import numpy
from game.interest import interest

class _gamestats:

    def __init__(self, width, height, numdogs):
        """
        """
        self.won = False
        self.ticks = 0
        self.entropy = self.emptyEntropy(width, height)

    def emptyEntropy(self, width, height):
        """

        Arguments:
        - `width`:
        - `height`:
        """
        return numpy.zeros((width, height), dtype=int)

    def setEntropy(self, dogs):
        for dog in dogs:
            x = round(dog[0])
            y = round(dog[1])
            self.entropy[y][x] += 1


class GameDataLogger:

    def __init__(self, maxticks):
        """
        """
        self.games = []
        self._maxticks = maxticks
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
        if won:
            self.currentgame.ticks = self._maxticks
        self.currentgame.won = won
        self.currentgame = None

    def calculateInterest(self, p1, p2, p3):
        """
        """
        ticks = [game.ticks for game in self.games]
        visits = []

        for _game in self.games:
            visits.append(_game.entropy)

        I = interest(ticks, visits, p1, p2, p3)
        return I

    def getStats(self, p1, p2, p3):
        """
        """
        N = len(self.games)
        wins = sum([1 if x.won else 0 for x in self.games])
        I = 0
        if N > 2:
            I = self.calculateInterest(p1,p2,p3)
        return N, wins, I
