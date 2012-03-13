(*****
 * Unit testing functions
 *****)

fun realSigmaEqual (x:real, y:real) =
    if Real.==(x, y) then
        true
    else
        (abs((x-y)/y)) < 0.000001

fun assert expr1 expr2 (desc:string) function = (
  (if function (expr1, expr2) then
    ()
  else (
    print ("*********** " ^ desc ^ " ***********\n" );
    print "FAIL\n"
  ));
  [expr1, expr2]
)

fun assertEqual expr1 expr2 (desc:string) = assert expr1 expr2 desc (fn (x, y) => x = y);
fun assertRealEqual (expr1:real) (expr2:real) (desc:string) = assert expr1 expr2 desc Real.==;
fun assertRealSigmaEqual (expr1:real) (expr2:real) (desc:string) = assert expr1 expr2 desc realSigmaEqual;

fun assertTrue expr desc = (assertEqual expr true desc);
fun assertFalse expr desc = (assertEqual expr false desc);


(*****
 * Project specific asserts
 *****)

fun comparePoints (point1 as point(x1,y1), point2 as point(x2,y2)) = (realSigmaEqual(x1,x2) andalso realSigmaEqual(y1,y2));
fun assertPointsEqual (p1:point) (p2:point) (desc:string) = assertTrue (comparePoints(p1, p2)) desc;

assertPointsEqual(point(1.0, 1.0)) (point(1.0, 1.0)) "Points were not equal";
assertFalse (comparePoints(point(1.0, 1.0), point(1.0, 0.0))) "Points were equal";

fun compareSizes (size1 as size(w1,h1), size2 as size(w2,h2)) = (realSigmaEqual(w1,w2) andalso realSigmaEqual(h1,h2));
fun assertSizesEqual (s1:size) (s2:size) (desc:string) = assertTrue (compareSizes(s1, s2)) desc;

assertSizesEqual(size(1.0, 1.0)) (size(1.0, 1.0)) "Sizes were not equal";
assertFalse (compareSizes(size(1.0, 1.0), size(1.0, 0.0))) "Sizes were equal";

fun compareEntities (entity1 as rect(point1, size1), entity2 as rect(point2, size2)) =
    (comparePoints(point1, point2) andalso compareSizes(size1, size2))
  | compareEntities (entity1 as circle(point1, radius(radius1)), entity2 as circle(point2, radius(radius2))) =
    (comparePoints(point1, point2) andalso realSigmaEqual(radius1, radius2))
  | compareEntities (entity1 as rect(point1, size1), entity2 as circle(point2, radius2)) = false
  | compareEntities (entity1 as circle(point1, radius1), entity2 as rect(point2, radius2)) = false
fun assertEntitiesEqual (e1:entity) (e2:entity) (desc:string) = assertTrue (compareEntities(e1, e2)) desc;

assertEntitiesEqual (rect(point(1.0, 1.0), size(5.0, 5.0))) (rect(point(1.0, 1.0), size(5.0, 5.0))) "Rects not equal";
assertEntitiesEqual (circle(point(1.0, 1.0), radius(5.0))) (circle(point(1.0, 1.0), radius(5.0))) "Circles not equal";
assertFalse(compareEntities(rect(point(1.0, 1.0), size(5.0, 5.0)), circle(point(1.0, 1.0), radius(5.0)))) "Rect is equal to circle";
assertFalse(compareEntities(circle(point(1.0, 1.0), radius(5.0)), rect(point(1.0, 1.0), size(5.0, 5.0)))) "Rect is equal to circle";

fun compareStates (state1 as state(cat1, dogs1, goal1, size1, g1, w1),
                   state2 as state(cat2, dogs2, goal2, size2, g2, w2)) =
    let
      fun compareDogs(doglist1 as entity_cons(dog1, rest1), doglist2 as entity_cons(dog2, rest2)) =
          if compareEntities(dog1, dog2) then
            compareDogs(rest1, rest2)
          else
            false

        | compareDogs(doglist1 as entity_nil, doglist2 as entity_nil) = true
        | compareDogs(doglist1 as entity_cons(_,_), doglist2 as entity_nil) = false
        | compareDogs(doglist1 as entity_nil, doglist2 as entity_cons(_,_)) = false
    in
      (compareEntities(cat1, cat2) andalso compareDogs(dogs1, dogs2) andalso
       compareEntities(goal1, goal2) andalso compareSizes(size1, size2) andalso
       (g1 = g2) andalso (w1 = w2))
    end
fun assertStatesEqual (s1:state) (s2:state) (desc:string) = assertTrue(compareStates(s1, s2)) desc;

fun assertWon (s1 as state(cat, dogs, goal, field, gameover, won)) (desc:string) = assertTrue(gameover = true andalso won = true) desc;
fun assertLost (s1 as state(cat, dogs, goal, field, gameover, won)) (desc:string) = assertTrue(gameover = true andalso won = false) desc;
fun assertNotOver (s1 as state(cat, dogs, goal, field, gameover, won)) (desc:string) = assertTrue(gameover = false) desc;

fun compareDirections (dir1 as left, dir2 as left) = true
  | compareDirections (dir1 as up, dir2 as up) = true
  | compareDirections (dir1 as down, dir2 as down) = true
  | compareDirections (dir1 as right, dir2 as right) = true
  | compareDirections (dir1, dir2) = false
fun assertDirectionsEqual (d1:direction) (d2:direction) (desc:string) = assertTrue(compareDirections(d1, d2)) desc;


fun printDirection (dir1 as left) = print "left"
  | printDirection (dir1 as up) = print "up"
  | printDirection (dir1 as down) = print "down"
  | printDirection (dir1 as right) = print "right"

fun compareDirectionLists (dirlist1 as dir_cons(dir1, rest1), dirlist2 as dir_cons(dir2, rest2)) =
    if compareDirections(dir1, dir2) then
      compareDirectionLists(rest1, rest2)
    else
      false
  | compareDirectionLists(dirlist1 as dir_nil, dirlist2 as dir_nil) = true
  | compareDirectionLists(dirlist1 as dir_nil, dirlist2 as dir_cons(d2,_)) = false
  | compareDirectionLists(dirlist1 as dir_cons(d1,_), dirlist2 as dir_nil) = false;

fun assertDirectionListsEqual (dl1:direction_list) (dl2:direction_list) (desc:string) = assertTrue(compareDirectionLists(dl1,dl2)) desc;

fun checkCell(Cells as cell_nil, Goal, I, Value) = false
  | checkCell(Cells as cell_cons(Cell, Rest), Goal, I, Value) =
    if I = Goal then realSigmaEqual(Cell, Value)
    else checkCell(Rest, Goal, I+1, Value);

fun printCells(Cells as cell_nil, W, I) : unit = (print "\n")
  | printCells(Cells as cell_cons(Cell, Rest), W, I) : unit = (
      (if (I mod W) = 0 then (print("\n" ^ (Real.toString(Cell)) ^ " "))
      else (print((Real.toString(Cell)) ^ " ")));
      printCells(Rest, W, I+1)
    );

(*****
 * Unittests for helper methods
 *****)

(* realGreaterOrEqual *)
assertTrue (realGreaterOrEqual(2.0, 1.0)) "2.0 >= 1.0";
assertTrue (realGreaterOrEqual(2.0, 2.0)) "2.0 >= 2.0";
assertFalse (realGreaterOrEqual(2.0, 3.0)) "! 2.0 >= 3.0";
assertTrue (realGreaterOrEqual(~1.0, ~2.0)) "-1.0 >= -2.0";
assertFalse (realGreaterOrEqual(~2.0, ~1.0)) "! -2.0 >= -1.0";
assertTrue (realGreaterOrEqual(10000.0, ~10000.0)) "10000.0 >= -10000.0";
assertFalse (realGreaterOrEqual(~10000.0, 10000.0)) "! -10000.0 >= 10000.0";

(* realLessOrEqual *)
assertFalse (realLessOrEqual(2.0, 1.0)) "!2.0 <= 1.0";
assertTrue (realLessOrEqual(2.0, 2.0)) "2.0 >= 2.0";
assertTrue (realLessOrEqual(2.0, 3.0)) "3.0 <= 2.0";
assertFalse (realLessOrEqual(~1.0, ~2.0)) "! -1.0 <= -2.0";
assertTrue (realLessOrEqual(~2.0, ~1.0)) "-2.0 <= -1.0";
assertFalse (realLessOrEqual(10000.0, ~10000.0)) "! 10000.0 <= -10000.0";
assertTrue (realLessOrEqual(~10000.0, 10000.0)) "-10000.0 <= 10000.0";

(* realGreater *)

assertTrue (realGreater(2.0, 1.0)) "2.0 > 1.0";
assertFalse (realGreater(2.0, 2.0)) "2.0 > 2.0";
assertFalse (realGreater(2.0, 3.0)) "! 3.0 > 2.0";
assertTrue (realGreater(~1.0, ~2.0)) "-1.0 > -2.0";
assertFalse (realGreater(~2.0, ~1.0)) "! -2.0 > -1.0";
assertTrue (realGreater(10000.0, ~10000.0)) "10000.0 > -10000.0";
assertFalse (realGreater(~10000.0, 10000.0)) "! -10000.0 > 10000.0";

(* clamp *)
assertRealEqual (clamp(5.0, 0.0, 10.0)) 5.0 "clamp(5.0, 0.0, 10.0) != 5.0";
assertRealEqual (clamp(~5.0, 0.0, 10.0)) 0.0 "clamp(~5.0, 0.0, 10.0) != 0.0";
assertRealEqual (clamp(5.0, 0.0, 1.0)) 1.0 "clamp(5.0, 0.0, 1.0) != 1.0";
assertRealEqual (clamp(~5.0, ~10.0, 0.0)) ~5.0 "clamp(-5.0, -10.0, 0.0) != -5.0";
assertRealEqual (clamp(20000.0, 0.0, 1.0)) 1.0 "clamp(20000.0, 0.0, 1.0) != 1.0";
assertRealEqual (clamp(5.0, 10.0, 0.0)) 5.0 "Clamp did not switch bounds";


val point1 = point(1.0, 1.0);
val point2 = point(0.0, 0.0);
val point3 = point(0.4, 0.0);
val point4 = point(5.0, 5.0);
val point5 = point(0.9, 0.9)
val dogsize = size(1.0, 1.0);
val catradius = radius(0.5);
val goalsize = size(5.0, 2.0);

val dog1 = rect(point1, dogsize);
val dog2 = rect(point2, dogsize);
val dog3 = rect(point3, dogsize);
val dog4 = rect(point4, dogsize);
val dog5 = rect(point5, dogsize);
val cat1 = circle(point1, catradius);
val cat2 = circle(point2, catradius);
val cat3 = circle(point3, catradius);
val cat4 = circle(point4, catradius);
val cat5 = circle(point5, catradius);
val goal = rect(point1, goalsize);

(* ensureInside *)
assertEntitiesEqual (ensureInside(dog1, size(10.0, 10.0))) dog1 "Dog was moved";
assertEntitiesEqual (ensureInside(dog1, size(1.0, 10.0))) (rect(point(0.5, 1.0), dogsize)) "Dog was not moved";
assertEntitiesEqual (ensureInside(dog2, size(10.0, 10.0))) (rect(point(0.5, 0.5), dogsize)) "Dog was not moved";
assertEntitiesEqual (ensureInside(dog1, size(10.0, 1.0))) (rect(point(1.0, 0.5), dogsize)) "Dog was not moved";
assertEntitiesEqual (ensureInside(cat1, size(10.0, 10.0))) cat1 "Cat was moved";
assertEntitiesEqual (ensureInside(cat2, size(10.0, 10.0))) (circle(point(0.5, 0.5), catradius)) "Cat was not moved";


(* getPointDistance *)
assertPointsEqual (getPointDistance(point(0.0, 0.0), point(1.0, 1.0))) (point(1.0, 1.0)) "getPointDistance 1";
assertPointsEqual (getPointDistance(point(0.0, 0.0), point(~1.0, 1.0))) (point(~1.0, 1.0)) "getPointDistance 2";
assertPointsEqual (getPointDistance(point(~5.0, 0.0), point(5.0, 0.0))) (point(10.0, 0.0)) "getPointDistance 3";
assertPointsEqual (getPointDistance(point(1000.0, 1000.0), point(~1000.0, ~1000.0))) (point(~2000.0, ~2000.0)) "getPointDistance 4";
assertPointsEqual (getPointDistance(point(2.5, 30.0), point(0.2, 41.7))) (point(~2.3, 11.7)) "getPointDistance 5";

(* getDistance *)
assertPointsEqual (getDistance(dog1, dog2)) (getPointDistance(point1, point2)) "Dog-Dog distance wrong";
assertPointsEqual (getDistance(cat1, dog2)) (getPointDistance(point1, point2)) "Cat-Dog distance wrong";
assertPointsEqual (getDistance(dog1, cat2)) (getPointDistance(point1, point2)) "Dog-Cat distance wrong";
assertPointsEqual (getDistance(cat1, cat2)) (getPointDistance(point1, point2)) "Cat-Cat distance wrong";

(* getQuadDistance *)
val quadDistance = sqrt(1.0 + 1.0);
assertRealSigmaEqual (getQuadDistance(dog1, dog2)) quadDistance "Dog-Dog distance wrong";
assertRealSigmaEqual (getQuadDistance(cat1, dog2)) quadDistance "Cat-Dog distance wrong";
assertRealSigmaEqual (getQuadDistance(dog1, cat2)) quadDistance "Dog-Cat distance wrong";
assertRealSigmaEqual (getQuadDistance(cat1, cat2)) quadDistance "Cat-Cat distance wrong";

(* initCells *)
fun countCell(Cells as cell_cons(Cell, Rest), I) = countCell(Rest, I+1.0)
  | countCell(Cells as cell_nil, I) = I;

val testcells1 = initCells(16.0, 16.0);
val testcells2 = initCells(3.0, 3.0);
assertRealSigmaEqual (countCell(testcells1, 0.0)) (16.0*16.0) "Cell count wrong for 16x16";
assertRealSigmaEqual (countCell(testcells2, 0.0)) (3.0*3.0) "Cell count wrong for 3x3";

(* increaseCell *)

val checkcell = increaseCell(testcells1, point(1.0, 1.0), 16.0);
assertTrue (checkCell(checkcell, 17, 0, 1.0)) "Did not increase correct cell";
val checkcell = increaseCell(testcells1, point(15.5, 2.5), 16.0);
assertTrue (checkCell(checkcell, 47, 0, 1.0)) "Partial coordinates did not increase correct cell";

(*****
 * Unit tests for game methods
 *****)
(* collide *)
assertTrue (collide(dog1, dog1)) "No dog self-collision";
assertTrue (collide(cat1, cat1)) "No cat self-collision";
assertTrue (collide(dog1, dog3)) "No dog-dog collision";
assertFalse (collide(dog1, dog4)) "Spurious dog-dog collision";
assertFalse (collide(cat1, cat4)) "Spurious cat-cat collision";
assertTrue (collide(dog1, cat1)) "No dog-cat same-point collision";
assertTrue (collide(cat1, dog1)) "No cat-dog same-point collision";
assertFalse (collide(cat2, cat5)) "Wrong cat-cat corner collision";
assertFalse (collide(dog2, cat5)) "Wrong dog-cat corner collision";
assertTrue (collide(cat2, dog3)) "No cat-dog edge collision";
assertTrue (collide(cat2, cat3)) "No cat-cat edge collision";

(* applyMoves *)
val dogs = entity_cons(dog4, entity_cons(dog4, entity_cons(dog4, entity_nil)));
val state1 = state(cat4, dogs, goal, size(16.0, 16.0), false, false);
val movedcat = circle(point(5.0, 3.0), catradius);
val movedupdog = rect(point(5.0, 3.5), dogsize);
val moveddowndog = rect(point(5.0, 6.5), dogsize);
val movedleftdog = rect(point(3.5, 5.0), dogsize);
val moveddogs = entity_cons(movedupdog, entity_cons(movedleftdog, entity_cons(moveddowndog, entity_nil)));
val incompletemoveddogs = entity_cons(movedupdog, entity_cons(movedupdog, entity_cons(dog4, entity_nil)));

val moves = dir_cons(up, dir_cons(up, dir_cons(left, dir_cons(down, dir_nil))));
val incompletemoves = dir_cons(up, dir_cons(up, dir_cons(up, dir_nil)));

val movedstate = state(movedcat, moveddogs, goal, size(16.0, 16.0), false, false);
val incompletemovedstate = state(movedcat, incompletemoveddogs, goal, size(16.0, 16.0), false, false);
val justcatmovedstate = state(movedcat, dogs, goal, size(16.0, 16.0), false, false);

val (teststate, movedcells) = applyMoves(state1, moves, testcells1);
assertStatesEqual teststate movedstate "Did not move correctly";
assertTrue (checkCell(movedcells, (3*16)+5, 0, 1.0)) "Dogs did not visit proper cell, up";
assertTrue (checkCell(movedcells, (6*16)+5, 0, 1.0)) "Dogs did not visit proper cell, down";
assertTrue (checkCell(movedcells, (5*16)+3, 0, 1.0)) "Dogs did not visit proper cell, left";

val (teststate, _) = applyMoves(state1, incompletemoves, testcells1);
assertStatesEqual teststate incompletemovedstate "Did not move correctly with incomplete moves";
val (teststate, _) = applyMoves(state1, dir_cons(up, dir_nil), testcells1);
assertStatesEqual teststate justcatmovedstate "Did not move correctly with just cat moves";
val (teststate, _) = applyMoves(state1, dir_nil, testcells1);
assertStatesEqual teststate state1 "Moved with no moves";

(* checkWinCondition *)
val topdogs = entity_cons(dog2, entity_cons(dog2, entity_cons(dog2, entity_nil)));
val centerdogs = dogs;
val topcat = cat2;
val centercat = cat4;
val cornercat = cat1;
val winstate = state(centercat, topdogs, centercat, size(16.0, 16.0), false, false);
assertWon (checkWinCondition(winstate)) "Did not win";
val losestate = state(topcat, topdogs, centercat, size(16.0, 16.0), false, false);
assertLost (checkWinCondition(losestate)) "Did not lose";
val winlosestate = state(centercat, centerdogs, centercat, size(16.0, 16.0), false, false);
assertWon (checkWinCondition(winlosestate)) "Did not tiebreak to win";
val neutralstate = state(cornercat, topdogs, centercat, size(16.0, 16.0), false, false);
assertNotOver (checkWinCondition(neutralstate)) "Game was over";

(* initState *)

val expectedcat = circle(point(8.0, 15.5), catradius); (* Cat on bottom center *)
val expectedgoal = rect(point(8.0, 1.0), goalsize);
val expectedstate = state(expectedcat, dogs, expectedgoal, size(16.0, 16.0), false, false);
assertStatesEqual (initState(size(16.0, 16.0), catradius, dogs, goalsize)) expectedstate "Init wrong state";

(* exitAchiever *)

val eagoal = rect(point(8.0, 1.0), goalsize);
val eacat1 = circle(point(0.0, 4.0), catradius);

assertDirectionsEqual (exitAchiever(eacat1, eacat1, dogs, eagoal)) right "Did not move right";

val eacat2 = circle(point(8.0, 16.0), catradius);
assertDirectionsEqual (exitAchiever(eacat2, eacat2, dogs, eagoal)) up "Did not move up";

val eacat3 = circle(point(16.0, 4.0), catradius);
assertDirectionsEqual (exitAchiever(eacat3, eacat3, dogs, eagoal)) left "Did not move left";

(* aiStep *)
val expecteddirections = dir_cons(up, dir_cons(right, dir_cons(right, dir_cons(right, dir_nil))));
assertDirectionListsEqual (aiStep(state1)) expecteddirections "Directions were wrong";
