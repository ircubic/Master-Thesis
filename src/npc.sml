(* This defines functions available in ADATE-ML to be available to MLton proper,
 * must be removed before turning into a spec.
 *)
val pow = Math.pow
val realEqual = Real.==
val realLess = Real.<
val sqrt = Math.sqrt
val realUnaryMinus = Real.~
val log10 = Math.ln
val realFloor = Real.realFloor
val realAdd = Real.+
val realSubtract = Real.-
val fromInt = Real.fromInt
val trunc = Real.trunc
val realDivide = Real./

signature GRADE =
sig

type grade
val zero : grade
val + : grade * grade -> grade
val toRealOpt : ( grade -> real )option
val post_process : grade -> grade
val comparisons : ( grade * grade -> order ) list
val toString : grade -> string
val pack : grade -> string
val unpack : string -> grade

end
exception D0
exception D1
exception D2
exception D3
(*CUT BEFORE*)

fun rMinus(X : real) : real = 0.0 - X

fun abs((X) : real) : real =
    case realLess(X, 0.0)
     of true => rMinus(X)
      | false => X


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

datatype ticks = tick_nil
               | tick_cons of real * ticks
datatype cells = cell_nil
               | cell_cons of real * cells
datatype visits = visit_nil
                | visit_cons of cells * visits
datatype result = result of real * ticks * visits

datatype dir_cost = dir_cost of real * direction
datatype cost_list = cost_nil | cost_cons of dir_cost * cost_list

datatype cat_ai_list = cat_ai_nil | cat_ai_cons of int * cat_ai_list


(*****
 * Helper methods
 *****)

fun realGreaterOrEqual((Num1, Num2) : real * real) : bool =
    case realLess(Num1, Num2)
     of true => false
      | false => true

fun realLessOrEqual((Num1, Num2) : real * real) : bool =
    case realLess(Num2, Num1)
     of true => false
      | false => true

fun realGreater((Num1, Num2) : real * real) : bool =
    case realLess(Num2, Num1)
     of true => true
      | false => false

(* Clamp the value of a number within the bounds *)
fun clamp ((Value, Lower, Upper) : real*real*real) : real =
    case realLess(Upper, Lower)
     of true => clamp(Value, Upper, Lower)
      | false =>
        case realLess(Value, Lower)
         of true => Lower
          | false =>
            case realGreater(Value, Upper)
             of true => Upper
              | false => Value

(* Ensures that an entity's position is within the field *)
fun ensureInside ((E, Field as size(Field_width, Field_height)) : entity * size)
    : entity =
    case E
     (* Rects must be clamped to have their center within half their width and
      * height of edges of the field.
      *)
     of rect(P as point(X,Y), S as size(Width, Height)) =>
        rect(point(clamp(X, (Width*0.5), Field_width-(Width*0.5)),
                   clamp(Y, (Height*0.5), Field_height-(Height*0.5))),
             S)
     (* Circles must have their centers at least a radius away from the edges
      * on both X and Y axes
      *)
      | circle(P as point(X,Y), R as radius(Radius)) =>
        circle(point(clamp(X, Radius, Field_width-Radius),
                     clamp(Y, Radius, Field_height-Radius)),
               R)

fun getPointDistance((Point1 as point(X1, Y1),
                      Point2 as point(X2, Y2))
                     : point * point) : point =
    point(X2-X1, Y2-Y1)

fun getDistance((Entity1, Entity2) : entity * entity) : point =
    case Entity1
     of rect(Point1, Size1) => (
        case Entity2
         of rect(Point2, Size2) => getPointDistance(Point1, Point2)
          | circle(Point2, Radius2) => getPointDistance(Point1, Point2))
      | circle(Point1, Radius1) => (
        case Entity2
         of rect(Point2, Size2) => getPointDistance(Point1, Point2)
          | circle(Point2, Radius2) => getPointDistance(Point1, Point2))

fun getQuadDistance((Entity1, Entity2) : entity * entity) : real =
    case getDistance(Entity1, Entity2)
     of point(Xd, Yd) => sqrt(pow(Xd,2.0) + pow(Yd, 2.0))

fun increaseCell((Cells, Point as point(X, Y), Width) : cells * point * real) : cells =
    let
        fun delve((Cells, GoalI, I) : cells * real * real) : cells =
            case Cells
             of cell_nil => cell_nil
              | cell_cons(Cell, CellRest) =>
                case realEqual(GoalI, I)
                 of true => cell_cons(Cell+1.0, CellRest)
                  | false => cell_cons(Cell, delve(CellRest, GoalI, I+1.0))
    in
        delve(Cells, (realFloor X + realFloor Y * Width), 0.0)
    end

fun initCells((W, H) : real * real) : cells =
    let
        fun driver((Goal, I) : real * real) : cells =
            case realEqual(Goal, I)
             of true => cell_nil
              | false => cell_cons(0.0, driver(Goal, I+1.0))
    in
        driver((W*H), 0.0)
    end


(*****
 * Functions directly relevant to the game that do not depend on f()
 *****)

(* Check if two entities collide *)
fun collide ((E1, E2) : entity * entity) : bool =
    case E1
     of rect(P1 as point(X1, Y1), S1 as size(W1, H1)) =>
       (case E2
         of rect(P2 as point(X2, Y2), S2 as size(W2, H2)) =>
            (* Two dogs collide using rectangle collisions *)
           (case realLess((Y1+(H1*0.5)), (Y2-(H2*0.5))) (* bottom1 < top2 *)
             of true => false
              | false =>
                case realGreater((Y1-(H1*0.5)), (Y2+(H2*0.5))) (* top1 > bottom2 *)
                 of true => false
                  | false =>
                    case realLess((X1+(W1*0.5)), (X2-(W2*0.5))) (* right1 < left2 *)
                     of true => false
                      | false =>
                        case realGreater((X1-(W1*0.5)), (X2+(W2*0.5))) (* left1 > right2 *)
                         of true => false
                          | false => true)
          | circle(P2 as point(X2, Y2), Ra2 as radius R2) =>
            (* Dog and cat collide with circle-rect collision *)
           (case abs(realSubtract(X2, X1)) of Dist_x =>
            case abs(realSubtract(Y2, Y1)) of Dist_y =>
            case W1/2.0 of Coll_width =>
            case H1/2.0 of Coll_height =>
                case realGreater(Dist_x, (Coll_width+R2))
                 of true => false
                  | false =>
                    case realGreater(Dist_y, (Coll_height+R2))
                     of true => false
                      | false =>
                        case realLess(Dist_x, Coll_width)
                         of true => true
                          | false =>
                            case realLess(Dist_y, Coll_height)
                             of true => true
                              | false => realLessOrEqual(pow((Dist_x - Coll_width), 2.0) +
                                                         pow((Dist_y - Coll_height), 2.0),
                                                         pow(R2, 2.0))))
      | circle(P1 as point(X1, Y1), Ra1 as radius R1) =>
       (case E2
         of rect(P2, S2) => collide(E2, E1) (* Flipped example *)
          | circle(P2 as point(X2, Y2), Ra2 as radius R2) =>
            (* Cats collide with circle collisions (for exhaustiveness) *)
            realLessOrEqual((pow((X1-X2), 2.0) + pow((Y1-Y2), 2.0)), pow((R1+R2),2.0)))

(* Apply the given moves to the game's entities *)
fun applyMoves((State as state(Cat, Dogs, Goal, Fieldsize as size(FW,FH), Gameover, Win),
                Moves, Cells)
               : state * direction_list * cells) : state * cells =
    let
      (* Move a point in the given direction, the given amount *)
        fun movePoint((Point as point(X,Y),
                       Move,
                       Speed)
                      : point * direction * real) : point =
            case Move
             of left => point(X-Speed, Y)
              | right => point(X+Speed, Y)
              | down => point(X, Y+Speed)
              | up => point(X, Y-Speed)

        (* Count these entities cell visits, but only if they're dogs *)
        and countCellVisits((Entities, Cells) : entity_list * cells) : cells =
            case Entities
             of entity_nil => Cells
              | entity_cons(Entity, Rest) =>
                case Entity
                 of rect(Point, S as size(W,H)) =>
                    countCellVisits(Rest, increaseCell(Cells, Point, FW))
                  | circle(Point, Radius as radius(R)) =>
                    countCellVisits(Rest, Cells)

        (* Move a single entity the chosen direction *)
        and moveEntity((Entity, Move, Fieldsize) : entity * direction * size)
            : entity =
            ensureInside((case Entity
                           of rect(Point, S as size(W,H)) =>
                              (* Dogs move 1.5 units per tick *)
                              rect(movePoint(Point, Move, 1.5), S)
                            | circle(Point, Radius) =>
                              (* Cats move 2.0 units per tick *)
                              circle(movePoint(Point, Move, 2.0), Radius)),
                         Fieldsize)

        (* Move a list of entities with their corresponding list of moves *)
        and moveEntities((Entities, Moves, Fieldsize)
                         : entity_list * direction_list * size) : entity_list =
            case Entities
             of entity_nil => entity_nil
              | entity_cons(Entity, Rest) => (
                (* Fetch the move corresponding to the entity, then apply. If
                 * there are no more moves (Which really shouldn't happen),
                 * return an unmoved entity.
                 *)
                case Moves
                 of dir_nil => entity_cons(Entity, Rest)
                  | dir_cons(Move, Moverest) =>
                    entity_cons(
                      moveEntity(Entity, Move, Fieldsize),
                      moveEntities(Rest, Moverest, Fieldsize)))

    in
        case Moves
         (* In case there are no passed in moves (should not happen), we just
          * return the same state
          *)
         of dir_nil => (State, Cells)

          (* We have to separate the cat's move (always the first) from the dogs
           * moves, to be able to put it into the state properly
           *)
          | dir_cons(Catmove, Rest) =>
            case moveEntities(Dogs, Rest, Fieldsize)
             of NewDogs =>
                (state(
                  (* Move the cat separately *)
                  moveEntity(Cat, Catmove, Fieldsize),
                  (* Move all the dogs *)
                  NewDogs,
                  (* Rest stays the same *)
                  Goal, Fieldsize, Gameover, Win),
                 countCellVisits(NewDogs, Cells))
    end

(* Check the win conditions of the game and update the game's state *)
fun checkWinCondition((State as state(Cat, Dogs, Goal,
                                      Fieldsize, Gameover, Win))
                      : state) : state =
    let
        fun hasLost((Cat, Dogs) : entity * entity_list) : bool =
            case Dogs
             of entity_nil => false
              | entity_cons(Dog, Rest) => (
                case collide(Cat, Dog)
                 of true => true
                  | false => hasLost(Cat, Rest))

        and hasWon((Cat, Goal) : entity * entity) : bool = collide(Cat, Goal)

        and newWinState((Cat, Dogs, Goal) : entity * entity_list * entity) : bool * bool =
            case hasWon(Cat, Goal)
             of true => (true, true)
              (* If we haven't won, then the value of Win is irrelevant, and
               * whether the game is over depends on if the game has been lost,
               * so this is a small shortcut.
               *)
              | false => (hasLost(Cat, Dogs), false)
    in
        case newWinState(Cat, Dogs, Goal)
         of (NewGameover, NewWin) =>
            state(Cat, Dogs, Goal, Fieldsize, NewGameover, NewWin)
    end


(*****
 * The function that will be induced and its dependencies
 *****)

(* The induced function *)
fun f( (Self, Cat, Dogs, Goal, Field) : entity * entity * entity_list * entity * size) : direction =
    right (* Placeholder, induction startpoint *)

(* The potential field based cat *)
fun potentialFieldCat( (Self, Cat, Dogs, Goal, Field as size(W,H))
                       : entity * entity * entity_list * entity * size) : direction =
    let
        fun cost((X, Y) : real * real) : real =
            let
                fun dogCost((X,Y,DogX,DogY) : real * real * real * real) : real =
                    realDivide(75.0,
                               sqrt(pow(realSubtract(DogX, X), 2.0) + pow(realSubtract(DogY, Y),2.0)))
                and dogsCost((X, Y, Dogs) : real * real * entity_list) : real =
                    case Dogs
                     of entity_nil => 0.0
                      | entity_cons(Entity, Rest) => realAdd(
                        case Entity
                         of rect(P as point(EntX, EntY), S as size(W,H)) =>
                            dogCost(X,Y,EntX,EntY)
                          | circle(P as point(EntX, EntY), Ra as radius(R)) =>
                            dogCost(X,Y,EntX,EntY),
                        dogsCost(X,Y,Rest))
                and goalCost((X,Y,GoalX, GoalY) : real * real * real *real) : real =
                    pow(abs(realSubtract(GoalX,X)) + abs(realSubtract(GoalY,Y)), 2.0)
            in
                dogsCost(X,Y, Dogs) + (case Goal
                   of rect(P as point(GoalX, GoalY), S as size(W,H)) =>
                      goalCost(X,Y,GoalX,GoalY)
                    | circle(P as point(GoalX, GoalY), Ra as radius(R)) =>
                      goalCost(X,Y,GoalX,GoalY))
            end

        and directionCosts((X, Y, Distance, Stepsize, Padding) : real * real * real * real * real) : cost_list =
            let
                fun costDriver((CurrentX, CurrentY, DeltaX, DeltaY, TargetDistance, Cost, Num)
                               : real * real * real * real * real * real * real) : real =
                    (* Check if we've travelled the desired distance (plus a small fudge factor) *)
                    case realGreater(sqrt(pow(X-CurrentX, 2.0) + pow(Y-CurrentY, 2.0)), TargetDistance + 0.0001)
                     of true => realDivide(Cost,Num)
                      | false => costDriver(CurrentX+DeltaX,
                                           CurrentY+DeltaY,
                                           DeltaX,
                                           DeltaY,
                                           TargetDistance,
                                           realAdd(Cost, cost(CurrentX, CurrentY)),
                                           realAdd(Num, 1.0))
                and getEndPoint((Coord, Delta, Dist) : real * real * real) : real =
                    case realEqual(Delta, 0.0)
                     of true => Coord
                      | false =>
                        case realLess(Delta, 0.0)
                         of true => Coord - Dist
                          | false => Coord + Dist
                and getEndDistance((X, Y, DeltaX, DeltaY) : real * real * real * real) : real =
                    sqrt(pow(X - clamp(getEndPoint(X, DeltaX, Distance), Padding, W-Padding), 2.0) +
                         pow(Y - clamp(getEndPoint(Y, DeltaY, Distance), Padding, H-Padding), 2.0))
                and directionCost((DeltaX, DeltaY) : real * real) : real =
                    costDriver(X, Y, DeltaX, DeltaY,
                               getEndDistance(X, Y, DeltaX, DeltaY),
                               0.0, 0.0)
            in
                cost_cons(dir_cost(directionCost(Stepsize, 0.0), right),
                cost_cons(dir_cost(directionCost(rMinus(Stepsize), 0.0), left),
                cost_cons(dir_cost(directionCost(0.0, rMinus(Stepsize)), up),
                cost_cons(dir_cost(directionCost(0.0, Stepsize), down), cost_nil))))
            end
        and dirToString((Dir) : direction) : string =
            case Dir
             of left => "left"
              | right => "right"
              | up => "up"
              | down => "down"
        and chooseDirection((CostList) : cost_list) : direction =
            let
                fun findMin((CostRest,
                             CurrMin as dir_cost(MinCost, MinDirection))
                            : cost_list * dir_cost) : dir_cost =
                    case CostRest
                     of cost_nil => CurrMin
                      | cost_cons(DirCost as dir_cost(Cost, Direction),
                                  Rest) =>
                        case realLess(Cost, MinCost)
                         of true => findMin(Rest, DirCost)
                          | false => findMin(Rest, CurrMin)
            in
                case findMin(CostList, dir_cost(10000.0, up))
                 of (MinDirCost as dir_cost(MinCost, MinDir)) => MinDir
            end
    in
        case Self
         of rect(P as point(X,Y), S as size(W,H)) => raise D1
          | circle(P as point(X,Y), Ra as radius(R)) =>
            chooseDirection(directionCosts(X,Y, 2.0, 0.5, R))
    end

(* The exit-achieving cat *)
fun exitAchiever( (Self, Cat, Dogs, Goal, Field)
                  : entity * entity * entity_list * entity * size) : direction =
    case getDistance(Self, Goal)
     of point(Dist_x, Dist_y) =>
        case realGreater(abs(Dist_x), abs(Dist_y))
         of true => (
            case realLess(Dist_x, 0.0)
             of true => left
              | false => right)
          | false => (
            case realGreater(Dist_y, 0.0)
             of true => down
              | false => up)

(* The AI of the cat *)
fun catAI( (Self, Cat, Dogs, Goal, Field, AI) : entity * entity * entity_list * entity * size * int)
    : direction =
    case AI = 1
      of true => exitAchiever(Self, Cat, Dogs, Goal, Field)
       | false => potentialFieldCat(Self, Cat, Dogs, Goal, Field)

(* Choose the k nearest dogs to a given dog *)
fun kNearest((Self, Dogs, K, SelfIndex) : entity * entity_list * real * real) : entity_list =
    let
        (* Trim the remainder of the Nearest list to k length *)
        fun trimNearest((Nearest, I) : entity_list * real) : entity_list =
            case realEqual(I, K)
             of true => entity_nil
              | false =>
                case Nearest
                 of entity_nil => entity_nil
                  | entity_cons(Head, Tail) => entity_cons(Head, trimNearest(Tail, I+1.0))

        (* Attempt to insert this dog into the list of nearest dogs *)
        and attemptInsertion((InsDog, Nearest, I) : entity * entity_list * real) : entity_list =
            (* If we're past the k amount of dogs in the nearest list, this dog is gone *)
            case realEqual(I, K)
             of true => entity_nil
              | false =>
                (* If we're not past k yet, but we're at the end of the Nearest list, this dog is added by default *)
                case Nearest
                 of entity_nil => entity_cons(InsDog, entity_nil)
                  (* If we're not past k, and there are still dogs in the Nearest list, compare *)
                  | entity_cons(Head, Tail) =>
                    case realLess(getQuadDistance(Self, InsDog), getQuadDistance(Self, Head))
                    (* If this dog is nearer than the current dog in the nearest
                     * list, then insert before and trim the remainder of the
                     * Nearest list
                     *)
                     of true => entity_cons(InsDog, trimNearest(Nearest, I+1.0))
                     (* If not, then keep going! *)
                      | false => entity_cons(Head, attemptInsertion(InsDog, Tail, I+1.0))

        (* Start checking the given dog and return the new Nearest list *)
        and checkDog((CheckDog, Nearest) : entity * entity_list) : entity_list =
            attemptInsertion(CheckDog, Nearest, 0.0)

        (* Iterate over the dogs and check them for insertion into the Nearest list *)
        and checkDogs((DogRest, Nearest, I): entity_list * entity_list * real) : entity_list =
            case DogRest
             of entity_nil => Nearest
              | entity_cons(Current, Rest) =>
                case realEqual(I, SelfIndex)
                 of true => checkDogs(Rest, Nearest, I+1.0)
                  | false => checkDogs(Rest, checkDog(Current, Nearest), I+1.0)

    in
      checkDogs(Dogs, entity_nil, 0.0)
    end

(* Do the ai steps for all the entities and generate a list of moves *)
fun aiStep ((State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win), AI) : state * int)
    : direction_list =
    let
        fun stepDogs((Dogrest, I) : entity_list * real) : direction_list =
            case Dogrest
             of entity_nil => dir_nil
              | entity_cons(Dog, Rest) =>
                dir_cons(f(Dog, Cat, kNearest(Dog, Dogs, 2.0, I), Goal, Fieldsize),
                         stepDogs(Rest, I+1.0))
    in
      dir_cons(
        catAI(Cat, Cat, Dogs, Goal, Fieldsize, AI),
        stepDogs(Dogs, 0.0))
    end

(* Do one tick of the simulation. This does one AI step, applies all the
 * moves, then checks the if the game has been won or lost
 *)
fun simtick((State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win), Cells, AI) : state * cells * int)
    : state * cells =
    case Gameover
     of true => (State, Cells)
      | false =>
        case applyMoves(State, aiStep(State, AI), Cells)
         of (NewState, NewCells) => (checkWinCondition(NewState), NewCells)

(* Initialize the state of the game based on the given sizes and passed in dogs *)
fun initState((Fieldsize as size(Fieldwidth, Fieldheight),
               Catradius as radius(Radius),
               Dogs,
               Goalsize as size(Goalwidth, Goalheight)
              ) : size * radius * entity_list * size) : state =
    state(
      (* Cat is placed on the bottom center. *)
      circle(point(Fieldwidth/2.0, Fieldheight - Radius),
             Catradius),
      (* Use the passed in dogs *)
      Dogs,
      (* Goal is placed on the top center *)
      rect(point(Fieldwidth/2.0, Goalheight/2.0), Goalsize),
      (*fieldsize*)
      Fieldsize,
      (*gameover*)
      false,
      (*win*)
      false)


(*****
 * The main driver function of the simulation
 *****)
fun main( (Dogs, CatAIs) : entity_list * cat_ai_list ) : result  =
    let
        fun mainLoop((Tick,
                      State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win),
                      Cells,
                      AI
                     ) : real * state * cells * int) : real * cells =
            case realGreater(Tick, 50.0)
             of true => (Tick-1.0, Cells)
              | false =>
                case Gameover
                 of true => (Tick-1.0, Cells)
                  | false =>
                    case simtick(State, Cells, AI)
                     of (NewState, NewCells) => mainLoop(Tick+1.0, NewState, NewCells, AI)
        and runSimsForCats((Ticks, Visits, RunAIs) : ticks * visits * cat_ai_list) : ticks * visits =
            case RunAIs
             of cat_ai_nil => (Ticks, Visits)
              | cat_ai_cons(AI, Rest) =>
                case mainLoop(1.0,
                             initState(size(16.0,16.0),
                                       radius(0.75),
                                       Dogs,
                                       size(5.0, 2.0)),
                             initCells(16.0, 16.0),
                             AI)
                 of (Tick, Cells) => runSimsForCats(tick_cons(Tick, Ticks), visit_cons(Cells, Visits), Rest)
        and countCats((AIs, N) : cat_ai_list * real) : real =
            case AIs
             of cat_ai_nil => N
              | cat_ai_cons(A,R) => countCats(R, N+1.0)
        and runSims((N, I, Ticks, Visits) : real * real * ticks * visits) : result =
            case realEqual(N,I)
             of true => result(N*countCats(CatAIs, 0.0), Ticks, Visits)
              | false =>
                case runSimsForCats(Ticks, Visits, CatAIs)
                 of (NewTicks, NewVisits) =>
                    runSims(N, I+1.0, NewTicks, NewVisits)
    in
        runSims(50.0, 0.0, tick_nil, visit_nil)
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
    (Real.fromLargeInt(Word.toLargeInt(MLton.Random.rand()))/4294967295.0)

(* Generate randomly placed dogs inside the given field *)
fun randomDogs((Dogfield as size(Fieldwidth, Fieldheight),
                Dogsize as size(Dogwidth, Dogheight),
                Dognumber
               ) : size * size * int) : entity_list =
    let
        fun nextDog((Rest):int) : entity_list =
            entity_cons(
              rect(
                point(Dogwidth/2.0 + (randReal()*Fieldwidth),
                      Dogheight/2.0 + (randReal()*Fieldheight)),
                Dogsize),
              (* If we are not on the last index, add another dog *)
              case Rest > 0
               of true => nextDog(Rest-1)
                | false => entity_nil
            )
    in
        (* Start counting down the indexes *)
        case MLton.Random.useed()
         of NONE => ()
          | SOME(seed) => MLton.Random.srand(seed);
        nextDog(Dognumber-1)
    end

fun generateDogLists((Amount,
                      Dognumber,
                      Dogsize as size(Dogwidth, Dogheight),
                      Fieldsize as size(Fieldwidth, Fieldheight),
                      CatAIs
                     ) : int * int * size * size * cat_ai_list)  =
    let
      fun nextDogs((Left, Dogfield as size(Dogfieldwidth, Dogfieldheight))
                   : int * size) =
          case Left <= 0
           of true => nil
            | false =>
                (randomDogs(Dogfield, Dogsize, Dognumber), CatAIs) :: nextDogs(Left-1, Dogfield)
    in
      (* Dogs will be placed within the upper half of the
       * simulation field, so we must make sure that the position
       * field compensates for the width of the dogs.
       *)
      nextDogs(Amount, size(Fieldwidth-Dogwidth, (Fieldheight/2.0)-Dogheight))
    end

fun interest((Result as result(N, Ticks, Visits)) : result) : real =
    let
      fun tickMax((Ticks, Max): ticks * real)  : real =
          case Ticks
           of tick_nil => Max
            | tick_cons(Tick, TickRest) =>
              case realLess(Max, Tick)
               of true => tickMax(TickRest, Tick)
                | false => tickMax(TickRest, Max)
      and tickSum((Ticks, Sum) : ticks * real) : real =
          case Ticks
           of tick_nil => Sum
            | tick_cons(Tick, TickRest) => tickSum(TickRest, Sum + Tick)
      and tickStd((Ticks, Avg, Acc) : ticks * real * real) : real =
          case Ticks
           of tick_nil => sqrt(Acc/N)
            | tick_cons(Tick, TickRest) => tickStd(TickRest, Avg, Acc + pow(Tick - Avg, 2.0))
      and T((Weight) : real) : real =
          pow((1.0 - ((tickSum(Ticks, 0.0) / N) / tickMax(Ticks, 0.0))), Weight)
      and S((Weight, TMax, TMin) : real * real * real) : real =
        case (tickSum(Ticks, 0.0)/N)
         of Avg =>
            pow((tickStd(Ticks, Avg, 0.0) / ((0.5 * sqrt(N / (N - 1.0))) * (TMax - TMin))),
                Weight)
      and cellSum((Cells, Sum) : cells * real) : real =
          case Cells
           of cell_nil => Sum
            | cell_cons(Cell, CellRest) => cellSum(CellRest, Sum + Cell)
      and Hn((Weight, Cells, VisitSum, Acc) : real * cells * real * real) : real =
          case realLess(VisitSum, 2.0)
           of true => 0.0
            | false =>
              case Cells
               of cell_nil => pow((~1.0 / log10(VisitSum))*Acc, Weight)
                | cell_cons(Cell, CellRest) =>
                  case realEqual(Cell, 0.0)
                   of true => Hn(Weight, CellRest, VisitSum, Acc)
                    | false => Hn(Weight, CellRest, VisitSum, Acc + ((Cell/VisitSum)*log10(Cell/VisitSum)))
      and H((Weight, Visits, Sum) : real * visits * real) : real =
          case Visits
           of visit_nil => (Sum/N)
            | visit_cons(Cells, VisitRest) =>
              case Hn(Weight, Cells, cellSum(Cells, 0.0), 0.0) of NewHn =>
                H(Weight, VisitRest, Sum + NewHn)
    in
      case realGreater(N, 0.0)
        of true => (
        case (1.0, 1.0, 1.0, 0.5, 1.0, 4.0) of (Gamma, Delta, Epsilon, P1, P2, P3) =>
        case (T(P1), S(P2, 50.0, 3.0), H(P3, Visits, 0.0)) of (M1, M2, M3) =>
            case ((Gamma*M1 + Delta*M2 + Epsilon*M3)/(Gamma+Delta+Epsilon)) of
                 Interest => Interest)
         | false => 0.0
    end

(* Used to compare two results in new version of ADATE *)
fun resultEqual( Result1 : result, Result2 : result ) : bool =
let
  val result( N1, Ts1, Vs1 ) = Result1
  val result( N2, Ts2, Vs2 ) = Result1

  fun ticksEqual( Ticks1 : ticks, Ticks2 : ticks ) : bool =
    case ( Ticks1, Ticks2 ) of
      ( tick_nil, tick_nil ) => true
    | ( tick_cons( Tick1, RTicks1 ), tick_cons( Tick2, RTicks2 ) ) => (
        case realEqual( Tick1, Tick2 ) of
          true => ticksEqual( RTicks1, RTicks2 )
        | false => false )
    | _ => false

  fun cellsEqual( Cells1 : cells, Cells2 : cells ) : bool =
    case ( Cells1, Cells2 ) of
      ( cell_nil, cell_nil ) => true
    | ( cell_cons( Cell1, RCells1 ), cell_cons( Cell2, RCells2 ) ) => (
        case realEqual( Cell1, Cell2 ) of
          true => cellsEqual( RCells1, RCells2 )
        | false => false )
    | _ => false

  fun visitsEqual( Visits1 : visits, Visits2 : visits ) : bool =
    case ( Visits1, Visits2 ) of
      ( visit_nil, visit_nil ) => true
    | ( visit_cons( Cells1, RVisits1 ), visit_cons( Cells2, RVisits2 ) ) => (
        case cellsEqual( Cells1, Cells2 ) of
          true => visitsEqual( RVisits1, RVisits2 )
        | false => false )
    | _ => false

in
  realEqual(N1, N2) andalso ticksEqual( Ts1, Ts2 ) andalso visitsEqual( Vs1, Vs2 )
end

val Inputs = generateDogLists(50, 4, size(1.5, 1.5), size(16.0,16.0),
                              cat_ai_cons(1, cat_ai_cons(2, cat_ai_nil)))
val Outputs = []

val Validation_inputs = []
val Validation_outputs = []

val Test_inputs = Validation_inputs

(*val All_outputs =  Vector.fromList( Outputs @ Validation_outputs )*)

val Funs_to_use = [
  "false", "true",
  "realLess", "realAdd", "realSubtract", "realMultiply",
  "realDivide", "tanh",
  "point", "size", "radius",
  "left", "right", "up", "down",
  "dir_nil", "dir_cons",
  "rect", "circle",
  "entity_nil", "entity_cons",
  "getDistance", "getQuadDistance",
  "clamp", "collide", "ensureInside"
  ]


val Reject_funs = []

fun restore_transform D = D

structure Grade : GRADE =
struct

type grade = LargeInt.int
val NONE = LargeInt.maxInt (* To check that LargeInt has infinite precision. *)
val zero = LargeInt.fromInt 0
val op+ = LargeInt.+
val comparisons = [ LargeInt.compare ]

fun toString( G : grade ) : string =
  Real.toString( Real.fromLargeInt G / 1.0E14 )

val pack = LargeInt.toString

fun unpack( S : string ) : grade =
  case LargeInt.fromString S of SOME G => G

val post_process = fn X => X

val toRealOpt = NONE

end

val Abstract_types = []

fun sqr( X : real ) = X * X

fun to( G : real ) : LargeInt.int =
  Real.toLargeInt IEEEReal.TO_NEAREST ( G * 1.0e10 )

fun output_eval_fun( I : int, _ , Y : result  ) =
  case interest Y of G =>
  if G > 1.0E30 orelse not( Real.isFinite G ) then
    { numCorrect = 0, numWrong = 1, grade = to 1.0e30 }
  else
    { numCorrect = 1, numWrong = 0, grade = to G }


val Max_output_genus_card = 8

val Max_time_limit = 1073741824
val Time_limit_base = 1073741824.0

(* New ADATE *)
(*
val AllAtOnce = false
fun compile_transform D = D
val print_synted_program  = Print.print_dec'

fun output_eval_fun( exactlyOne( I : int, _ , Y : result ) )
    : { grade: Grade.grade, numCorrect : int, numWrong : int } list  =
  case interest Y of G =>
  if G > 1.0E30 orelse not( Real.isFinite G ) then
    [{ numCorrect = 1, numWrong = 0, grade = to 1.0e30 }]
  else
    [{ numCorrect = 1, numWrong = 0, grade = to G }]


val OnlyCountCalls = false
val max_time_limit  = fn () => 262144
val max_test_time_limit = fn () => 262144
val time_limit_base = fn () => 262144.0

fun max_syntactic_complexity() = 500.0
fun min_syntactic_complexity() = 0.0
val Use_test_data_for_max_syntactic_complexity = false

val main_range_eq = resultEqual
val File_name_extension = ""
val Resolution = NONE
val StochasticMode = false

val Number_of_output_attributes : int = 4

fun terminate( Nc, G )  = false
*)
