from math import *

def collideRectWithRect(rect1, rect2):
    """Check for collision between two rectangles

    The rectangles are passed in as tuples with the following format:
    (centerx, centery, width, height)

    Arguments:
    - `rect1`: Rectangle 1
    - `rect2`: Rectangle 2
    """
    if (rect1.getBottom() < rect2.getTop() or
        rect1.getTop() > rect2.getBottom() or
        rect1.getRight() < rect2.getLeft() or
        rect1.getLeft() > rect2.getRight()):
        return False
    else:
        return True

def collideCircleWithRect(circle, rect):
    """Check for collision between a circle and a rectongle.

    The rectangle has the following format:
    (centerx, centery, width, height)

    The circle has the following format:
    (centerx, centery, radius)

    Arguments:
    - `circle`: The circle to check
    - `rect`: The rectangle to check
    """
    (cx, cy) = circle.getPosition()
    radius = circle.getRadius()
    (rx, ry) = rect.getPosition()
    (rw, rh) = rect.getSize()

    distance_x = abs(cx - rx)
    distance_y = abs(cy - ry)
    collide_width = rw/2.
    collide_height = rh/2.

    if distance_x > (collide_width + radius):
        return False
    if distance_y > (collide_height + radius):
        return False

    if distance_x <= collide_width:
        return True
    if distance_y <= collide_height:
        return True

    square_corner_dist = ((distance_x - collide_width)**2 +
                          (distance_y - collide_height)**2)

    return (square_corner_dist <= radius**2)

def collideCircleWithCircle(circle1, circle2):
    sum_radius = circle1.getRadius() + circle2.getRadius()
    (x1, y1) = circle1.getPosition()
    (x2, y2) = circle2.getPosition()
    return (sum_radius <= sqrt((x1-x2)**2 + (y1-y2)**2))

def collideShapes(shape1, shape2):
    if hasattr(shape1,'getSize'):
        if hasattr(shape2, 'getSize'):
            return collideRectWithRect(shape1, shape2)
        elif hasattr(shape2, 'getRadius'):
            return collideCircleWithRect(shape2, shape1)
    elif hasattr(shape1, 'getRadius'):
        if hasattr(shape2, 'getSize'):
            return collideCircleWithRect(shape1, shape2)
        elif hasattr(shape2, 'getRadius'):
            return collidieCircleWithCircle(shape1, shape2)
    return false

def collide(entity1, entity2):
    return collideShapes(entity1.getShape(), entity2.getShape())

def getDistance(entity1, entity2):
    return (entity2.x-entity1.x,
            entity2.x-entity1.x)

def getQuadDistance(entity1, entity2):
    diffx, diffy = getDistance(entity1, entity2)
    return sqrt(diffx**2 + diffy**2)

def clamp(x, low, high):
    return min(high, max(x, low))

def ensureInside(shape, field):
    f_width, f_height = field
    pos = shape.getPosition()
    newpos = None
    if hasattr(shape, 'getSize'):
        width, height = shape.getSize()
        newpos = [
            clamp(pos[0], width*0.5, f_width-(width*0.5)),
            clamp(pos[1], height*0.5, f_height-(height*0.5))
        ]
    else:
        radius = shape.getRadius()
        newpos = [
            clamp(pos[0], radius, f_width-radius),
            clamp(pos[1], radius, f_height-radius)
        ]
    shape.setPosition(newpos)
