#!/usr/bin/env python
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
