#!/usr/bin/env python
import pygame
import pygame.locals
import math

from chars import Cat, Dog

KEY_MAP = {
    pygame.locals.K_UP: 'up',
    pygame.locals.K_DOWN: 'down',
    pygame.locals.K_LEFT: 'left',
    pygame.locals.K_RIGHT: 'right',
}
def main_loop(screen):
    background = pygame.Surface(screen.get_size())
    background = background.convert()
    kitty = Cat((320,400))
    dogs = []
    nums = 5.0
    for i in range(int(nums)):
        space = screen.get_size()[0]/float(nums)
        y = 175 - 75*math.sin(i*math.pi/(nums-1))
        x = (space/2)+i*space
        dogs.append(Dog((x,y)))

    clock = pygame.time.Clock()
    run = True
    while run:
        clock.tick(60)
        for event in pygame.event.get():
            if event.type == pygame.locals.QUIT:
                print 'Quitting'
                return
            elif event.type == pygame.locals.KEYDOWN:
                if event.key in KEY_MAP:
                    kitty.move(KEY_MAP[event.key])
            elif event.type == pygame.locals.KEYUP:
                if event.key in KEY_MAP:
                    kitty.stop(KEY_MAP[event.key])

        kitty.update()

        background.fill((200,200,200))
        screen.blit(background, (0,0))
        kitty.draw(screen)
        for dog in dogs:
            dog.draw(screen)

        pygame.display.flip()


pygame.init()

screen = pygame.display.set_mode((640,480))
pygame.display.set_caption('Dead End')

main_loop(screen)

pygame.quit()
