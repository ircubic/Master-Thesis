#!/usr/bin/env python

###
# Dead End Testbed
#--
# This is the testbed for the thesis, which implements the Dead End game. This
# game is a simple predator-prey game where the player controls a "cat", which
# is trying to reach the goal at the top of the game field. The enemies are
# "dogs" that try to capture the cat. The dogs move slower than the cat, but are
# present in larger numbers and must collaborate to take down the cat.
#
# The testbed is split into two parts, the simulation part and the game part.
# This is to decouple the logic/simulation part from the part that displays the
# testbed and takes input, making it a playable game.
#
# The simulation part is intended to be shared with the ADATE specification,
# either by maintaining a separate ML version, or by translating the entire
# python code into ML and calling that from Python. The simulation's public
# interface is designed with that in mind.
#
# The game part wraps around the simulation part, providing a visual depiction
# of the simulation, as well as sending keyboard input into the simulation. The
# game part runs at a much higher pace (30FPS) than the simulation (which is
# updated only once every n game ticks).
#
###

import pygame
from game import Game

def main():
    """Little main function stub
    """
    pygame.init()
    g = Game()
    g.mainLoop()
    pygame.quit()

if __name__ == '__main__':
    main()
