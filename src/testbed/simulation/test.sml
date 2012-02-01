datatype point = point of real * real
datatype size = size of real * real
datatype entity = entity of point * size
datatype entity_list = entity_nil
                     | entity_cons of entity * entity_list

fun clamp ((value, lower, upper) : real*real*real) : real =
    case value < lower
     of true => lower
      | false =>
        case value > upper
         of true => upper
          | false => value

(* Ensures that an entity's position is within the field *)
fun ensureInside ((E as entity(point(x,y), size(width, height)),
                  F as size(field_width, field_height)) :
                 entity * size) : entity =
    entity(point(clamp(x, (width*0.5), field_width-(width*0.5)),
                 clamp(y, (height*0.5), field_height-(height*0.5))),
           size(width, height))


(* Check if two entities defined as rectangles collide *)
fun collideRectWithRect ((E1 as entity(point(x1, y1), size(w1, h1)),
                         E2 as entity(point(x2, y2), size(w2, h2))) :
                        entity * entity) : bool =
    case  (y1+(h1*0.5)) < (y2-(h2*0.5)) (* bottom1 < top2 *)
        of true => false
         | false =>
           case (y1-(h1*0.5)) > (y2+(h2*0.5)) (* top1 > bottom2 *)
            of true => false
             | false =>
               case (x1+(w1*0.5)) < (x2-(w2*0.5)) (* right1 < left2 *)
                of true => false
                 | false =>
                   case (x1-(w1*0.5)) > (x2+(w2*0.5)) (* left1 > right2 *)
                    of true => false
                     | false => true
