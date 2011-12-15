#!/usr/bin/env python
import pygame
import pygame.locals
from pygame.sprite import Group, spritecollide, collide_mask
import math

from chars import Cat, Dog, Goal


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
        background = pygame.Surface(self.screen.get_size())
        self.background = background.convert()
        self.background.fill((200,200,200))

        self.font = pygame.font.Font(None, 35)
        self.subfont = pygame.font.Font(None, 25)

        self.sprites = Group()
        self.dogs = Group()
        self.nums = 4.0

        size = self.screen.get_size()
        space = size[0]/float(self.nums)
        for i in range(int(self.nums)):
            y = 175 - int(75*math.sin(i*math.pi/(self.nums-1)))
            x = int((space/2)+i*space)
            self.dogs.add(Dog((x,y)))

        self.kitty = Cat((int(size[0]/2),int(size[1]*0.8)))
        self.goal = Goal((int(size[0]/2),0))
        self.sprites.add(self.dogs)
        self.sprites.add(self.kitty)
        self.sprites.add(self.goal)

        self.clock = pygame.time.Clock()
        self.run = True
        self.gameover = False
        self.gamewin = False


def preserve_screen(dest, screen):
    dest.blit(screen, (0,0))
    dest.fill((64,64,64,64),special_flags=pygame.locals.BLEND_SUB)

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

            state.sprites.draw(screen)

            kitty_rect = state.kitty.get_rect()

            if spritecollide(state.kitty, state.dogs, False, collided=collide_mask):
                state.gameover = True
                state.gamewin = False
                preserve_screen(state.background, state.screen)
            elif kitty_rect.colliderect(state.goal.get_rect()):
                state.gameover = True
                state.gamewin = True
                preserve_screen(state.background, state.screen)

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

screen = pygame.display.set_mode((800,600))
pygame.display.set_caption('Dead End')
state = GameState(screen)

main_loop(state)

pygame.quit()
