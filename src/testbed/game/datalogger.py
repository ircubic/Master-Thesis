import numpy
from game.interest import interest

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

    def calculateInterest(self, g, d, e):
        """
        """
        ticks = [game.ticks for game in self.games]
        dogvisits = []
        for i in range(len(self.games[0].entropy)):
            dogvisits.append([])

        for _game in self.games:
            for i, dogentropy in enumerate(_game.entropy):
                dogvisits[i].append(dogentropy)

        I = interest(ticks, dogvisits, g, d, e)
        return I

    def getStats(self, g, d, e):
        """
        """
        N = len(self.games)
        wins = sum([1 if x.won else 0 for x in self.games])
        I = 0
        if N > 2:
            I = self.calculateInterest(g,d,e)
        return N, wins, I
