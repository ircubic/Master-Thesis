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


class GameState(object):
    """Contains the state of the game
    """

    def __init__(self, screen):
        """

        Arguments:
        - `screen`:
        """
        self.screen = screen
        self.reinitialize()

    def reinitialize(self, ):
        """Reinitialize the game
        """
        background = pygame.Surface(screen.get_size())
        self.background = background.convert()
        self.background.fill((200,200,200))

        self.font = pygame.font.Font(None, 35)
        self.subfont = pygame.font.Font(None, 25)

        self.dogs = []
        self.nums = 5.0

        space = self.screen.get_size()[0]/float(self.nums)
        for i in range(int(self.nums)):
            y = 175 - 75*math.sin(i*math.pi/(self.nums-1))
            x = (space/2)+i*space
            self.dogs.append(Dog((x,y)))

        self.kitty = Cat((320,400))

        self.clock = pygame.time.Clock()
        self.run = True
        self.gameover = False
        self.gamewin = False



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
        elif event.type == pygame.locals.MOUSEBUTTONDOWN:
            events.append({'type':'click'})
    return events


def main_loop(state):

    while state.run:
        state.clock.tick(60)

        events = handle_events()
        if 'quit' in [event['type'] for event in events]:
            return

        state.screen.blit(state.background, (0,0))

        if not state.gameover:
            for event in events:
                if event['type'] == 'move':
                    state.kitty.move(event['dir'])
                elif event['type'] == 'stop':
                    state.kitty.stop(event['dir'])

            state.kitty.update()

            state.kitty.draw(screen)
            for dog in state.dogs:
                dog.draw(screen)

            kitty_rect = state.kitty.get_rect()
            dog_rects = [dog.get_rect() for dog in state.dogs]
            if kitty_rect.collidelist(dog_rects) >= 0:
                state.gameover = True
                state.gamewin = False
                state.background.blit(state.screen, (0,0))
                state.background.fill((64,64,64,64),special_flags=pygame.locals.BLEND_SUB)

        else:
            for event in events:
                if event['type'] == 'click':
                    state.reinitialize()
                    continue

            message = "YOU WON!" if state.gamewin else "YOU LOST!"
            text = state.font.render(message,True,[0,0,0])
            pos = text.get_rect(center = screen.get_rect().center)
            state.screen.blit(text,pos)

            subtext = state.subfont.render("Click to restart", True, [0,0,0])
            pos = subtext.get_rect(midtop = pos.midbottom)
            state.screen.blit(subtext,pos)


        pygame.display.flip()


pygame.init()

screen = pygame.display.set_mode((640,480))
pygame.display.set_caption('Dead End')
state = GameState(screen)

main_loop(state)

pygame.quit()
