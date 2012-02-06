datatype point = point of real * real
datatype rect = rect of real * real
datatype circle = circle of real
datatype entity = cat of point * rect
                | dog of point * circle
datatype entity_list = entity_nil
                     | entity_cons of entity * entity_list
val pow = Math.pow
(*val abs = Math.abs*)

fun clamp ((Value, Lower, Upper) : real*real*real) : real =
    case Value < Lower
     of true => Lower
      | false =>
        case Value > Upper
         of true => Upper
          | false => Value

fun clampRect((P as point(X,Y), S as rect(Width, Height),
               F as rect(Field_width, Field_height)) :
              point * rect * rect) : point =
    point(clamp(X, (Width*0.5), Field_width-(Width*0.5)),
          clamp(Y, (Height*0.5), Field_height-(Height*0.5)))

(* Ensures that an entity's position is within the field *)
fun ensureInside ((E, F) : entity * rect) : entity =
    case E
     of cat(P,S) =>
        cat(clampRect(P,S,F), S)
      | dog(P, C as circle(R)) =>
        dog(clampRect(P, rect((R*2.0), (R*2.0)), F), C)


(* Check if two entities collide *)
fun collide ((E1, E2) : entity * entity) : bool =
    case (E1, E2)
     (* Two cats collide using rectangle collisions *)
     of (cat(point(X1,Y1), rect(W1,H1)),
         cat(point(X2,Y2), rect(W2,H2))) =>
        (case (Y1+(H1*0.5)) < (Y2-(H2*0.5)) (* bottom1 < top2 *)
          of true => false
           | false =>
             case (Y1-(H1*0.5)) > (Y2+(H2*0.5)) (* top1 > bottom2 *)
              of true => false
               | false =>
                 case (X1+(W1*0.5)) < (X2-(W2*0.5)) (* right1 < left2 *)
                  of true => false
                   | false =>
                     case (X1-(W1*0.5)) > (X2+(W2*0.5)) (* left1 > right2 *)
                      of true => false
                       | false => true)

      (* Cat and dog collide with circrle-rect collision *)
      | (cat(point(X1,Y1), rect(W1,H1)),
         dog(point(X2,Y2), circle(R))) =>
        (case (abs(X2 - X1), abs(Y2 - Y1), W1/2.0, H1/2.0)
          of (Dist_x, Dist_y, Coll_width, Coll_height) =>
             case (Dist_x > (Coll_width+R))
              of true => false
               | false =>
                 case (Dist_y > (Coll_height+R))
                  of true => false
                   | false =>
                     case (Dist_x < Coll_width)
                      of true => true
                       | false =>
                         case (Dist_y < Coll_height)
                          of true => true
                           | false => (pow((Dist_x - Coll_width), 2.0) +
                                       pow((Dist_y - Coll_height), 2.0)
                                       <= pow(R,2.0))

        )

      (* Flipped example *)
      | (dog(point(X1,Y1),circle(R)), cat(point(X2,Y2),rect(W,H))) => collide(E2, E1)
      (* Dogs collide with circle collisions (for exhaustiveness) *)
      | (dog(point(X1,Y1), circle(R1)),
         dog(point(X2,Y2), circle(R2))) =>
        ((pow((X1-X2), 2.0) + pow((Y1-Y2),2.0)) <= pow((R1+R2),2.0))
