datatype point = point of real * real
datatype rect = rect of real * real
datatype circle = circle of real
datatype entity = cat of point * rect
                | dog of point * circle
datatype entity_list = entity_nil
                     | entity_cons of entity * entity_list
val pow = Math.pow
val abs = Math.abs

fun clamp ((value, lower, upper) : real*real*real) : real =
    case value < lower
     of true => lower
      | false =>
        case value > upper
         of true => upper
          | false => value

fun clampRect((P as point(x,y), S as rect(width, height),
               F as rect(field_width, field_height)) :
              point * rect * rect) : point =
    point(clamp(x, (width*0.5), field_width-(width*0.5)),
          clamp(y, (height*0.5), field_height-(height*0.5)))

(* Ensures that an entity's position is within the field *)
fun ensureInside ((e, f) : entity * rect) : entity =
    case e
     of cat(p,s) =>
        cat(clampRect(p,s,f), s)
      | dog(p, c as circle(r)) =>
        dog(clampRect(p, rect((r*2.0), (r*2.0)), f), c)


(* Check if two entities collide *)
fun collide ((E1, E2) : entity * entity) : bool =
    case (E1, E2)
     (* Two cats collide using rectangle collisions *)
     of (cat(point(x1,y1), rect(w1,h1)),
         cat(point(x2,y2), rect(w2,h2))) =>
        (case (y1+(h1*0.5)) < (y2-(h2*0.5)) (* bottom1 < top2 *)
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
                       | false => true)

      (* Cat and dog collide with circrle-rect collision *)
      | (cat(point(x1,y1), rect(w1,h1)),
         dog(point(x2,y2), circle(r))) =>
        (case (abs(x2 - x1), abs(y2 - y1), w1/2.0, h1/2.0)
          of (dist_x, dist_y, coll_width, coll_height) =>
             case (dist_x > (coll_width+r))
              of true => false
               | false =>
                 case (dist_y > (coll_height+r))
                  of true => false
                   | false =>
                     case (dist_x < coll_width)
                      of true => true
                       | false =>
                         case (dist_y < coll_height)
                          of true => true
                           | false => (pow((dist_x-coll_width), 2) +
                                       pow((dist_y - coll_height), 2)
                                       <= pow(r,2))

        )

      (* Flipped example *)
      | (dog(_,_), cat(_,_)) => collide(E2, E1)
      (* Dogs collide with circle collisions (for exhaustiveness) *)
      | (dog(point(x1,y1), circle(r1)),
         dog(point(x2,y2), circle(r2))) =>
        ((pow((x1-x2), 2) + pow((y1-y2),2)) <= pow((r1+r2),2))
