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


(*****
 * Helper methods
 *****)

(* Clamp the value of a number within the bounds *)
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


(*****
 * Functions directly relevant to the game that do not depend on f()
 *****)

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

(* Apply the given moves to the game's entities *)
fun applyMoves((State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win), Moves) : state * direction_list) : state =
    let
      (* Move a point in the given direction, the given amount *)
      fun movePoint((Point as point(X,Y), Move, Speed) : point * direction * real) : point =
          case Move
           of left => point(X-Speed, Y)
            | right => point(X+Speed, Y)
            | down => point(X, Y+Speed)
            | up => point(X, Y-Speed)

      (* Move a single entity the chosen direction *)
      fun moveEntity((Entity, Move, Fieldsize) : entity * direction * size) : entity =
          ensureInside(
            case Entity
             of rect(Point, Size as size(W,H)) =>
                rect(movePoint(Point, Move, 1.5), Size)
              | circle(Point, Radius) =>
                circle(movePoint(Point, Move, 1.5*3.0/4.0), Radius)
          , Fieldsize)

      (* Move a list of entities with their corresponding list of moves *)
      fun moveEntities((Entities, Moves, Fieldsize) : entity_list * direction_list * size) : entity_list =
          case Entities
           of entity_nil => entity_nil
            | entity_cons(Entity, Rest) =>
              (* Fetch the move corresponding to the entity. If there are no
                 more moves (Which really shouldn't happen), return an unmoved
                 entity.
               *)
              case Moves
               of dir_nil => entity_cons(Entity, moveEntities(Rest, Moves, Fieldsize))
                | dir_cons(Move, Moverest) =>
                  entity_cons(
                    moveEntity(Entity, Move, Fieldsize),
                    moveEntities(Rest, Moverest, Fieldsize)
                  )

    in
      case Moves
       of dir_nil => State
        (* We have to separate the cat's move (always the first) from the dogs
         * moves, to be able to put it into the state properly
         *)
        | dir_cons(Catmove, Rest) =>
          state(
            (* Move the cat separately *)
            moveEntity(Cat, Catmove, Fieldsize),
            (* Move all the dogs *)
            moveEntities(Dogs, Rest, Fieldsize),
            (* Rest stays the same *)
            Goal, Fieldsize, Gameover, Win
          )
    end

(* Check the win conditions of the game and update the game's state *)
fun checkWinCondition((State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win)) : state) : state =
    let
      fun hasLost((Cat, Dogs) : entity * entity_list) : bool =
        case Dogs
         of entity_nil => false
          | entity_cons(Dog, Rest) =>
            case collide(Cat, Dog)
             of true => true
              | false => hasLost(Cat, Rest)

      fun hasWon() : bool =
          collide(Cat, Goal)

      fun newWinState() : bool * bool =
          case hasWon()
           of true => (true, true)
            | false => (hasLost(Cat, Dogs), false)
    in
      case newWinState()
       of (newGameover, newWin) =>
           state(Cat, Dogs, Goal, Fieldsize, newGameover, newWin)
    end


(*****
 * The function that will be induced and its dependencies
 *****)
fun f( (Self, Cat, Dogs, Goal) : entity * entity * entity_list * entity ) : direction =
    right

fun catAI( (Self, Cat, Dogs, Goal) : entity * entity * entity_list * entity) : direction =
    left

(* Do the ai steps for all the entities and generate a list of moves *)
fun aiStep ((State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win)) : state) : direction_list =
    let
      fun stepDogs((Dogrest) : entity_list) : direction_list =
          case Dogrest
           of entity_nil => dir_nil
            | entity_cons(Dog, Rest) =>
              dir_cons(f(Dog, Cat, Dogs, Goal), stepDogs(Rest))
    in
      dir_cons(
        catAI(Cat, Cat, Dogs, Goal),
        stepDogs(Dogs)
      )
    end

(* Do one tick of the simulation. This does one AI step, applies all the
 * moves, then checks the if the game has been won or lost
 *)
fun simtick((State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win)) : state) : state =
    case Gameover
     of false => State
      | true => checkWinCondition(applyMoves(State, aiStep(State)))

(* Initialize the state of the game based on the given sizes and passed in dogs *)
fun initState((Fieldsize as size(Fieldwidth, Fieldheight), Catsize as size(Catwidth, Catheight), Dogs, Goalsize as size(Goalwidth, Goalheight)) : size * size * entity_list * size) : state =
    state(
      (* Cat is placed on the bottom center. *)
      rect(point(Fieldwidth/2.0, Fieldheight - Catheight/2.0),
           Catsize),
      (* Dogs will be placed within the upper half of the
       * simulation field, so we must make sure that the position
       * field compensates for the width of the dogs.
       *)
      Dogs,
      (* Goal is placed on the top center *)
      rect(point(Fieldwidth/2.0, Goalheight/2.0), Goalsize),
      (*fieldsize*)
      Fieldsize,
      (*gameover*)
      false,
      (*win*)
      false
    )


(*****
 * The main driver function of the simulation
 *****)
fun main( (Dogs) : entity_list ) : bool =
    let
      fun mainLoop((Tick, State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win)) : int * state) : bool =
          case Tick >= 50
           of true => Win
            | false =>
              case Gameover
               of true => Win
                | false => mainLoop(Tick+1, simtick(State))
    in
      mainLoop(1,
               initState(size(16.0,16.0),
                         size(1.5,1.5),
                         Dogs,
                         size(5.0, 2.0)
               )
      )
    end

(*****
 * Some stuff to be in the spec part in the spec
 *****)

(*%%*)

(* Generate a random real between 0.0 and 1.0 *)
fun randReal() : real =
    (* Fairly unsure if this is correct actually (I'm assuming
     * an MLton word is 32 bits)
     *)
    MLton.Real.fromWord(MLton.Random.rand())/4294967295.0

(* Generate randomly placed dogs inside the given field *)
fun randomDogs((Dogfield as size(Fieldwidth, Fieldheight), Dogsize as radius(Radius), Dognumber) : size * radius * int) : entity_list =
    let
       fun nextDog((Rest):int) : entity_list =
           entity_cons(
             circle(
               point(Radius + (randReal()*Fieldwidth),
                     Radius + (randReal()*Fieldheight)),
               Dogsize),
             (* If we are not on the last index, add another dog *)
             case Rest > 0
             of true => nextDog(Rest-1)
              | false => entity_nil
           )
    in
        (* Start counting down the indexes *)
        nextDog(Dognumber-1)
    end

datatype entity_list_list = entity_list_nil
                          | entity_list_cons of entity_list * entity_list_list

fun generateDogLists((Amount, Dognumber, Dogradius, Fieldsize as size(Fieldwidth, Fieldheight)) : int * int * real * size) : entity_list_list =
    let
      fun nextDogs((Left, Dogfield as size(Dogfieldwidth, Dogfieldheight)) : int * size) : entity_list_list =
        case Left <= 0
         of true => entity_list_nil
          | false => entity_list_cons(
              randomDogs(Dogfield,
                     radius(Dogradius),
                     Dognumber),
              nextDogs(Left-1, Dogfield)
            )
    in
      nextDogs(Amount, size(Fieldwidth-(Dogradius*2.0), Fieldheight/2.0))
    end
val Inputs = generateDogLists(50, 4, 0.75, size(16.0,16.0))
val Outputs = []

(*SPECSTART
val Validation_inputs = []
val Validation_outputs = []

val All_outputs =  Vector.fromList( Outputs @ Validation_outputs )

val Funs_to_use = [
  "false", "true",
  "realLess", "realAdd", "realSubtract", "realMultiply",
  "realDivide", "sigmoid"
  ]

val Reject_funs = []
fun restore_transform D = D

structure Grade : GRADE =
struct

type grade = unit
val zero = ()
val op+ = fn(_,_) => ()
val comparisons = [ fn _ => EQUAL ]
val toString = fn _ => ""
val fromString = fn _ => SOME()

val pack = fn _ => ""
val unpack = fn _ =>()

val post_process = fn _ => ()

val toRealOpt = NONE

end

val Abstract_types = []

fun output_eval_fun( I : int, _ , Y  ) =
    { numCorrect = 0, numWrong = 1, grade = () }


val Max_output_genus_card = 2

val Max_time_limit = 1024
val Time_limit_base = 1024.0
SPECEND*)
