datatype point = point of real * real
datatype size = size of real * real
datatype radius = radius of real
datatype direction = left | right | up | down
datatype direction_list = dir_nil
                        | dir_cons of direction * direction_list
datatype entity = rect of point * size
                | circle of point * radius
datatype entity_list = entity_nil
                     | entity_cons of entity * entity_list
(* cat, dogs, goal, fieldsize, gameover, win *)
datatype state = state of entity * entity_list * entity * size * bool * bool

val pow = Math.pow

fun f( (Self, Cat, Dogs, Goal) : entity * entity * entity_list * entity ) : direction =
    right

fun width((E) : entity) : real =
    case E
    of rect(point(X,Y), size(Width, Height)) => Width
     | circle(point(X,Y), radius(Radius)) => Radius*2.0

fun randReal() : real =
    (* Fairly unsure if this is correct actually (I'm assuming
     * an MLton word is 32 bits)
     *)
    MLton.Real.fromWord(MLton.Random.rand())/pow(2.0,32.0)-1.0

(* Generate randomly placed dogs inside the given field *)
fun randomDogs((Dogfield as size(Fieldwidth, Fieldheight), Dogsize as radius(Radius), Dognumber) : size * radius * real) : entity_list =
    let
       fun nextDog((Rest):real) : entity_list =
           entity_cons(
             circle(
               point(Radius + (randReal()*Fieldwidth),
                     Radius + (randReal()*Fieldheight)),
               Dogsize),
             (* If we are not on the last index, add another dog *)
             case Rest > 0.0
             of true => nextDog(Rest-1.0)
              | false => entity_nil
           )
    in
        (* Start counting down the indexes *)
        nextDog(Dognumber-1.0)
    end

fun initState((Fieldsize as size(Fieldwidth, Fieldheight), Catsize as size(Catwidth, Catheight), Dogradius, Goalsize as size(Goalwidth, Goalheight), Dognumber) : size * size * real * size * real) : state =
    state(
      (* Cat is placed on the bottom center. *)
      rect(point(Fieldwidth/2.0, Fieldheight - Catheight/2.0),
           Catsize),
      (* Dogs will be placed within the upper half of the 
       * simulation field, so we must make sure that the position
       * field compensates for the width of the dogs.
       *)
      randomDogs(size(Fieldwidth-(Dogradius*2.0),
                      Fieldheight/2.0),
                 radius(Dogradius),
                 Dognumber),
      (* Goal is placed on the top center *)
      rect(point(Fieldwidth/2.0, Goalheight/2.0), Goalsize),
      (*fieldsize*)
      Fieldsize,
      (*gameover*)
      false,
      (*win*)
      false
    )

fun clamp ((Value, Lower, Upper) : real*real*real) : real =
    case Value < Lower
     of true => Lower
      | false =>
        case Value > Upper
         of true => Upper
          | false => Value

(* Ensures that an entity's position is within the field *)
fun ensureInside ((E, Field as size(Field_width, Field_height)) : entity * size) : entity =
    case E
     of rect(point(X,Y), S as size(Width, Height)) =>
        rect(point(clamp(X, (Width*0.5), Field_width-(Width*0.5)),
                   clamp(Y, (Height*0.5), Field_height-(Height*0.5))),
             S)
      | circle(point(X,Y), R as radius(R_)) =>
        circle(point(clamp(X, R_, Field_width-R_),
                     clamp(Y, R_, Field_height-R_)),
               R)

(* Check if two entities collide *)
fun collide ((E1, E2) : entity * entity) : bool =
    case (E1, E2)
     (* Two cats collide using rectangle collisions *)
     of (rect(point(X1,Y1), size(W1,H1)),
         rect(point(X2,Y2), size(W2,H2))) =>
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
      | (rect(point(X1,Y1), size(W1,H1)),
         circle(point(X2,Y2), radius(R))) =>
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
      | (circle(P1,R), rect(P2,S)) => collide(E2, E1)
      (* Dogs collide with circle collisions (for exhaustiveness) *)
      | (circle(point(X1,Y1), radius(R1)),
         circle(point(X2,Y2), radius(R2))) =>
        ((pow((X1-X2), 2.0) + pow((Y1-Y2),2.0)) <= pow((R1+R2),2.0))

fun hasLost ((Cat, Dogs) : entity * entity_list) : bool =
    case Dogs
     of entity_nil => false
      | entity_cons(Dog, Rest) =>
        case collide(Cat, Dog)
         of true => true
          | false => hasLost(Cat, Rest)

fun hasWon ((Cat,Goal) : entity * entity) : bool =
    collide(Cat, Goal)

fun aiStep ((State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win)) : state) : direction_list =
    let
      fun stepDogs((Dogrest) : entity_list) : direction_list =
          case Dogs
           of entity_nil => dir_nil
            | entity_cons(Dog, Rest) =>
              dir_cons(f(Dog, Cat, Dogs, Goal), stepDogs(Rest))
    in
      dir_cons(f(Cat, Cat, Dogs, Goal), stepDogs(Dogs))
    end


(*fun main( (Dogs) : entity_list ) : bool =
    f(Self, Cat, Dogs, Goal)*)
