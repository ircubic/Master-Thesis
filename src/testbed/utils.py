import pygame
import math
import os.path

def load_png(name):
	""" Load image and return image object"""
	fullname = os.path.join('img', name)
	try:
		image = pygame.image.load(fullname)
		if image.get_alpha() is None:
			image = image.convert()
		else:
			image = image.convert_alpha()
	except pygame.error, message:
        	print 'Cannot load image:', fullname
        	raise SystemExit, message
	return image

def normalize(vector):
    """Return normalized version of the vector

    Arguments:
    - `vector`:
    """
    try:
        length = math.sqrt(vector[0]**2 + vector[1]**2)
        return (vector[0]/length, vector[1]/length)
    except ZeroDivisionError:
        return vector
