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

def handle_events():
    events = []
    for event in pygame.event.get():
        if event.type == pygame.locals.QUIT:
            events.append({'type':'quit'})
        elif event.type == pygame.locals.KEYDOWN:
            if event.key in KEY_MAP:
                events.append({'type':'move',
                               'dir':KEY_MAP[event.key]})
        elif event.type == pygame.locals.KEYUP:
            if event.key in KEY_MAP:
                events.append({'type':'stop',
                               'dir':KEY_MAP[event.key]})
    return events

def main_loop(screen):
    background = pygame.Surface(screen.get_size())
    background = background.convert()
    font = pygame.font.Font(None, 35)

    dogs = []
    nums = 5.0

    for i in range(int(nums)):
        space = screen.get_size()[0]/float(nums)
        y = 175 - 75*math.sin(i*math.pi/(nums-1))
        x = (space/2)+i*space
        dogs.append(Dog((x,y)))

    kitty = Cat((320,400))

    clock = pygame.time.Clock()
    run = True
    gameover = False
    gamewin = False

    while run:
        clock.tick(60)

        events = handle_events()
        if 'quit' in [event['type'] for event in events]:
            return

        background.fill((200,200,200))
        screen.blit(background, (0,0))

        if not gameover:
            for event in events:
                if event['type'] == 'move':
                    kitty.move(event['dir'])
                elif event['type'] == 'stop':
                    kitty.stop(event['dir'])

            kitty.update()

            if kitty.get_rect().collidelist([dog.get_rect() for dog in dogs]) >= 0:
                gameover = True
                gamewin = False

            kitty.draw(screen)
            for dog in dogs:
                dog.draw(screen)
        else:
            message = "YOU WON!" if gamewin else "YOU LOST!"
            text = font.render(message,True,[0,0,0])
            pos = text.get_rect(center = screen.get_rect().center)
            screen.blit(text,pos)


        pygame.display.flip()


pygame.init()

screen = pygame.display.set_mode((640,480))
pygame.display.set_caption('Dead End')

main_loop(screen)

pygame.quit()
