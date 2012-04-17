def collideRectWithRect(rect1, rect2):
    """Check for collision between two rectangles

    The rectangles are passed in as tuples with the following format:
    (centerx, centery, width, height)

    Arguments:
    - `rect1`: Rectangle 1
    - `rect2`: Rectangle 2
    """

    left1 = rect1[0]-(rect1[2]*0.5)
    right1 = rect1[0]+(rect1[2]*0.5)
    top1 = rect1[1]-(rect1[3]*0.5)
    bottom1 = rect1[1]+(rect1[3]*0.5)

    left2 = rect2[0]-(rect2[2]*0.5)
    right2 = rect2[0]+(rect2[2]*0.5)
    top2 = rect2[1]-(rect2[3]*0.5)
    bottom2 = rect2[1]+(rect2[3]*0.5)

    if (bottom1 < top2 or top1 > bottom2 or
        right1 < left2 or left1 > right2):
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
    distance_x = abs(circle[0] - rect[0])
    distance_y = abs(circle[1] - rect[1])
    collide_width = rect[2]/2
    collide_height = rect[3]/2

    if distance_x > (collide_width + circle[2]):
        return False
    if distance_y > (collide_height + circle[2]):
        return False

    if distance_x <= collide_width:
        return True
    if distance_y <= collide_height:
        return True

    square_corner_dist = ((distance_x - collide_width)**2 +
                          (distance_y - collide_height)**2)

    return (square_corner_dist <= circle[2]**2)
