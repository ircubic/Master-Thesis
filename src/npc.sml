(* This defines functions available in ADATE-ML to be available to MLton proper,
 * must be removed before turning into a spec.
 *)
val pow = Math.pow
val realEqual = Real.==
val realLess = Real.<
val sqrt = Math.sqrt
val realUnaryMinus = Real.~
val log10 = Math.log10
val realFloor = Real.realFloor

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


(*****
 * Helper methods
 *****)

fun realGreaterOrEqual((Num1, Num2) : real * real) : bool =
    case realLess(Num1, Num2)
     of true => false
      | false => true

fun realLessOrEqual((Num1, Num2) : real * real) : bool =
    case realLess(Num1, Num2)
     of true => true
      | false => realEqual(Num1, Num2)

fun realGreater((Num1, Num2) : real * real) : bool =
    case realLess(Num1, Num2)
     of true => false
      | false =>
        case realEqual(Num1, Num2)
         of true => false
          | false => true

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
     of rect(point(X,Y), S as size(Width, Height)) =>
        rect(point(clamp(X, (Width*0.5), Field_width-(Width*0.5)),
                   clamp(Y, (Height*0.5), Field_height-(Height*0.5))),
             S)
     (* Circles must have their centers at least a radius away from the edges
      * on both X and Y axes
      *)
      | circle(point(X,Y), R as radius(Radius)) =>
        circle(point(clamp(X, Radius, Field_width-Radius),
                     clamp(Y, Radius, Field_height-Radius)),
               R)

fun getPointDistance((Point1 as point(X1, Y1),
                      Point2 as point(X2, Y2))
                     : point * point) : point =
    point(X2-X1, Y2-Y1)

fun getDistance((Entity1, Entity2) : entity * entity) : point =
    case (Entity1, Entity2)
     of (rect(Point1, Size1), rect(Point2, Size2)) =>
        getPointDistance(Point1, Point2)
      | (rect(Point1, Size1), circle(Point2, Radius)) =>
        getPointDistance(Point1, Point2)
      | (circle(Point1, Radius), rect(Point2, Size1)) =>
        getPointDistance(Point1, Point2)
      | (circle(Point1, Radius1), circle(Point2, Radius2)) =>
        getPointDistance(Point1, Point2)

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
        delve(Cells, (realFloor(X) + realFloor(Y) * Width), 0.0)
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
    case (E1, E2)
     (* Two dogs collide using rectangle collisions *)
     of (rect(point(X1,Y1), size(W1,H1)),
         rect(point(X2,Y2), size(W2,H2))) =>
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

      (* Dog and cat collide with circle-rect collision *)
      | (rect(point(X1,Y1), size(W1,H1)),
         circle(point(X2,Y2), radius(R))) =>
        (case (abs(X2 - X1), abs(Y2 - Y1), W1/2.0, H1/2.0)
          of (Dist_x, Dist_y, Coll_width, Coll_height) =>
             case realGreater(Dist_x, (Coll_width+R))
              of true => false
               | false =>
                 case realGreater(Dist_y, (Coll_height+R))
                  of true => false
                   | false =>
                     case realLess(Dist_x, Coll_width)
                      of true => true
                       | false =>
                         case realLess(Dist_y, Coll_height)
                          of true => true
                           | false => realLessOrEqual(pow((Dist_x - Coll_width), 2.0) +
                                                      pow((Dist_y - Coll_height), 2.0),
                                                      pow(R,2.0))

        )

      (* Flipped example *)
      | (circle(P1,R), rect(P2,S)) => collide(E2, E1)
      (* Cats collide with circle collisions (for exhaustiveness) *)
      | (circle(point(X1,Y1), radius(R1)),
         circle(point(X2,Y2), radius(R2))) =>
        realLessOrEqual((pow((X1-X2), 2.0) + pow((Y1-Y2),2.0)), pow((R1+R2),2.0))

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
                 of rect(Point, Size as size(W,H)) =>
                    countCellVisits(Rest, increaseCell(Cells, Point, FW))
                  | circle(Point, Radius as radius(R)) =>
                    countCellVisits(Rest, Cells)

        (* Move a single entity the chosen direction *)
        and moveEntity((Entity, Move, Fieldsize) : entity * direction * size)
            : entity =
            ensureInside((case Entity
                           of rect(Point, Size as size(W,H)) =>
                              (* Dogs move 1.5 units per tick *)
                              rect(movePoint(Point, Move, 1.5), Size)
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
         of (newGameover, newWin) =>
            state(Cat, Dogs, Goal, Fieldsize, newGameover, newWin)
    end


(*****
 * The function that will be induced and its dependencies
 *****)

(* The induced function *)
fun f( (Self, Cat, Dogs, Goal) : entity * entity * entity_list * entity ) : direction =
    right (* Placeholder, induction startpoint *)

datatype dir_cost = dir_cost of real * direction
datatype cost_list = cost_nil | cost_cons of dir_cost * cost_list

(* The potential field based cat *)
fun potentialFieldCat( (Self, Cat, Dogs, Goal)
                       : entity * entity * entity_list * entity) : direction =
    let
        fun cost((X, Y) : real * real) : real =
            let
                fun dogCost((X,Y,DogX,DogY) : real * real * real * real) : real =
                    1000.0 / (abs(DogX-X) + abs(DogY-Y))
                and dogsCost((X, Y, Dogs) : real * real * entity_list) : real =
                    case Dogs
                     of entity_nil => 0.0
                      | entity_cons(Entity, Rest) => (
                        case Entity
                         of rect(point(EntX, EntY), size(W,H)) =>
                            dogCost(X,Y,EntX,EntY)
                          | circle(point(EntX, EntY), radius(R)) =>
                            dogCost(X,Y,EntX,EntY)
                        ) + dogsCost(X,Y,Rest)
                and goalCost((X,Y,GoalX, GoalY) : real * real * real *real) : real =
                    sqrt(pow(GoalX-X, 2.0) + pow(GoalY-Y, 2.0))
            in
                dogsCost(X,Y, Dogs) + (
                case Goal
                 of rect(point(GoalX, GoalY), size(W,H)) =>
                    goalCost(X,Y,GoalX,GoalY)
                  | circle(point(GoalX, GoalY), radius(R)) =>
                    goalCost(X,Y,GoalX,GoalY)
            )
            end

        and directionCosts((X, Y, Distance) : real * real * real) : cost_list =
            let
                fun costDriver((CurrentX, CurrentY, DeltaX, DeltaY, Cost, Num)
                               : real * real * real * real * real * real) : real =
                    case realEqual(CurrentX, X)
                     of true => (
                        case realEqual(CurrentY, Y)
                         of true => Cost/Num
                          | false => costDriver(CurrentX+DeltaX,
                                                CurrentY+DeltaY,
                                                DeltaX,
                                                DeltaY,
                                                Cost+cost(CurrentX, CurrentY),
                                                Num+1.0)
                        )
                      | false => costDriver(CurrentX+DeltaX,
                                            CurrentY+DeltaY,
                                            DeltaX,
                                            DeltaY,
                                            Cost+cost(CurrentX, CurrentY),
                                            Num+1.0)
                and getEndPoint((Coord, DeltaCoord, Distance) : real * real * real) : real =
                    case realEqual(DeltaCoord, 0.0)
                     of true => Coord
                      | false => Coord + (DeltaCoord*Distance/abs(DeltaCoord))
                and directionCost((DeltaX, DeltaY) : real * real) : real =
                    costDriver(
                      getEndPoint(X,DeltaX,Distance),
                      getEndPoint(Y,DeltaY,Distance),
                      realUnaryMinus(DeltaX), realUnaryMinus(DeltaY), 0.0, 0.0)
            in
                cost_cons(dir_cost(directionCost(0.5, 0.0), right),
                cost_cons(dir_cost(directionCost(~0.5, 0.0), left),
                cost_cons(dir_cost(directionCost(0.0, ~0.5), up),
                cost_cons(dir_cost(directionCost(0.0, 0.5), down), cost_nil))))
            end

        and chooseDirection((CostList) : cost_list) : direction =
            let
                fun findMin((CostRest,
                             CurrMin as dir_cost(MinCost, MinDirection))
                            : cost_list * dir_cost) : dir_cost =
                    case CostRest
                     of cost_nil => CurrMin
                      | cost_cons(DirCost as dir_cost(Cost, Direction),
                                  Rest) => (
                        case realLess(Cost, MinCost)
                         of true => findMin(Rest, DirCost)
                          | false => findMin(Rest, CurrMin))
            in
                case findMin(CostList, dir_cost(10000.0, up))
                 of (MinDirCost as dir_cost(MinCost, MinDir)) => MinDir
            end
    in
        case Self
         of rect(point(X,Y), size(W,H)) =>
            chooseDirection(directionCosts(X,Y, 2.0))
          | circle(point(X,Y), radius(R)) =>
            chooseDirection(directionCosts(X,Y, 2.0))
    end

(* The exit-achieving cat *)
fun exitAchiever( (Self, Cat, Dogs, Goal)
                  : entity * entity * entity_list * entity) : direction =
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
fun catAI( (Self, Cat, Dogs, Goal) : entity * entity * entity_list * entity)
    : direction =
    exitAchiever(Self, Cat, Dogs, Goal)

(* Choose the k nearest dogs to a given dog *)
fun kNearest((Self, Dogs, k, SelfIndex) : entity * entity_list * real * real) : entity_list =
    let
        (* Trim the remainder of the Nearest list to k length *)
        fun trimNearest((Nearest, i) : entity_list * real) : entity_list =
            case realEqual(i, k)
             of true => entity_nil
              | false =>
                case Nearest
                 of entity_nil => entity_nil
                  | entity_cons(Head, Tail) => entity_cons(Head, trimNearest(Tail, i+1.0))

        (* Attempt to insert this dog into the list of nearest dogs *)
        and attemptInsertion((InsDog, Nearest, i) : entity * entity_list * real) : entity_list =
            (* If we're past the k amount of dogs in the nearest list, this dog is gone *)
            case realEqual(i, k)
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
                     of true => entity_cons(InsDog, trimNearest(Nearest, i+1.0))
                     (* If not, then keep going! *)
                      | false => entity_cons(Head, attemptInsertion(InsDog, Tail, i+1.0))

        (* Start checking the given dog and return the new Nearest list *)
        and checkDog((CheckDog, Nearest) : entity * entity_list) : entity_list =
            attemptInsertion(CheckDog, Nearest, 0.0)

        (* Iterate over the dogs and check them for insertion into the Nearest list *)
        and checkDogs((DogRest, Nearest, i): entity_list * entity_list * real) : entity_list =
            case DogRest
             of entity_nil => Nearest
              | entity_cons(Current, Rest) =>
                case realEqual(i, SelfIndex)
                 of true => checkDogs(Rest, Nearest, i+1.0)
                  | false => checkDogs(Rest, checkDog(Current, Nearest), i+1.0)

    in
      checkDogs(Dogs, entity_nil, 0.0)
    end

(* Do the ai steps for all the entities and generate a list of moves *)
fun aiStep ((State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win)) : state)
    : direction_list =
    let
        fun stepDogs((Dogrest, i) : entity_list * real) : direction_list =
            case Dogrest
             of entity_nil => dir_nil
              | entity_cons(Dog, Rest) =>
                dir_cons(f(Dog, Cat, kNearest(Dog, Dogs, 2.0, i), Goal),
                         stepDogs(Rest, i+1.0))
    in
      dir_cons(
        catAI(Cat, Cat, Dogs, Goal),
        stepDogs(Dogs, 0.0))
    end

(* Do one tick of the simulation. This does one AI step, applies all the
 * moves, then checks the if the game has been won or lost
 *)
fun simtick((State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win), Cells) : state * cells)
    : state * cells =
    case Gameover
     of false => (State, Cells)
      | true =>
        case applyMoves(State, aiStep(State), Cells)
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
fun main( (Dogs) : entity_list ) : result  =
    let
        fun mainLoop((Tick,
                      State as state(Cat, Dogs, Goal, Fieldsize, Gameover, Win),
                      Cells
                     ) : real * state * cells) : real * cells =
            case realGreater(Tick, 50.0)
             of true => (Tick-1.0, Cells)
              | false =>
                case Gameover
                 of true => (Tick-1.0, Cells)
                  | false =>
                    case simtick(State, Cells)
                     of (NewState, NewCells) => mainLoop(Tick+1.0, NewState, NewCells)
        and runSims((N, I, Ticks, Visits) : real * real * ticks * visits) : result =
            case realEqual(N,I)
             of true => result(N, Ticks, Visits)
              | false =>
                case mainLoop(1.0,
                             initState(size(16.0,16.0),
                                       radius(0.75),
                                       Dogs,
                                       size(5.0, 2.0)),
                             initCells(16.0, 16.0))
                 of (Tick, Cells) => runSims(N, I+1.0, tick_cons(Tick, Ticks), visit_cons(Cells, Visits))
    in
        runSims(50.0, 0.0, tick_nil, visit_nil)
    end;

(*****
 * Some stuff to be in the spec part in the spec
 *****)

(*%%*)

case MLton.Random.useed()
 of NONE => ()
  | SOME(seed) => MLton.Random.srand(seed);

(* Generate a random real between 0.0 and 1.0 *)
fun randReal() : real =
    (* Fairly unsure if this is correct actually (I'm assuming
     * an MLton word is 32 bits)
     *)
    MLton.Real.fromWord(MLton.Random.rand())/4294967295.0

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
        nextDog(Dognumber-1)
    end

fun generateDogLists((Amount,
                      Dognumber,
                      Dogsize as size(Dogwidth, Dogheight),
                      Fieldsize as size(Fieldwidth, Fieldheight)
                     ) : int * int * size * size)  =
    let
      fun nextDogs((Left, Dogfield as size(Dogfieldwidth, Dogfieldheight))
                   : int * size) =
          case Left <= 0
           of true => nil
            | false =>
                randomDogs(Dogfield, Dogsize, Dognumber) :: nextDogs(Left-1, Dogfield)
    in
      (* Dogs will be placed within the upper half of the
       * simulation field, so we must make sure that the position
       * field compensates for the width of the dogs.
       *)
      nextDogs(Amount, size(Fieldwidth-Dogwidth, Fieldheight/2.0))
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
        pow((1.0 - ((tickSum(Ticks, 0.0)/N)/tickMax(Ticks, 0.0))), Weight)
      and S((Weight, TMax, TMin) : real * real * real) : real =
        case (tickSum(Ticks, 0.0)/N)
         of Avg => pow(tickStd(Ticks, Avg, 0.0)/(0.5*sqrt(N/(N-1.0))*(TMax-TMin)), Weight)
      and cellSum((Cells, Sum) : cells * real) : real =
          case Cells
           of cell_nil => Sum
            | cell_cons(Cell, CellRest) => cellSum(CellRest, Sum + Cell)
      and Hn((Weight, Cells, VisitSum, Acc) : real * cells * real * real) : real =
          case Cells
           of cell_nil => pow((~1.0 / log10(VisitSum))*Acc, Weight)
            | cell_cons(Cell, CellRest) =>
              Hn(Weight, CellRest, VisitSum, Acc + ((Cell/VisitSum)*log10(Cell/VisitSum)))
      and H((Weight, Visits, Sum) : real * visits * real) : real =
          case Visits
           of visit_nil => (Sum/N)
            | visit_cons(Cells, VisitRest) =>
              H(Weight, VisitRest, Sum + Hn(Weight, Cells, cellSum(Cells, 0.0), 0.0))
    in
      case (1.0, 1.0, 1.0, 0.5, 1.0, 4.0)
       of (Gamma, Delta, Epsilon, P1, P2, P3) =>
          ((Gamma*T(P1) + Delta*S(P2, 50.0, 3.0) + Epsilon*H(P3, Visits, 0.0))/(Gamma+Delta+Epsilon))
    end

val Inputs = generateDogLists(50, 4, size(1.5, 1.5), size(16.0,16.0))
val Outputs = []

val Validation_inputs = []
val Validation_outputs = []

val Test_inputs = Validation_inputs

(*val All_outputs =  Vector.fromList( Outputs @ Validation_outputs )*)

val Funs_to_use = [
  "false", "true",
  "realLess", "realAdd", "realSubtract", "realMultiply",
  "realDivide", "sigmoid"
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
  Real.toLargeInt IEEEReal.TO_NEAREST ( G * 1.0e14 )

fun output_eval_fun( I : int, _ , Y : result  ) =
  { numCorrect = 1, numWrong = 0, grade = to( interest Y ) }


val Max_output_genus_card = 8

val Max_time_limit = 1024
val Time_limit_base = 1024.0
