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

fun compareDogs(doglist1 as entity_cons(dog1, rest1), doglist2 as entity_cons(dog2, rest2)) =
    if compareEntities(dog1, dog2) then
      compareDogs(rest1, rest2)
    else
      false

  | compareDogs(doglist1 as entity_nil, doglist2 as entity_nil) = true
  | compareDogs(doglist1 as entity_cons(_,_), doglist2 as entity_nil) = false
  | compareDogs(doglist1 as entity_nil, doglist2 as entity_cons(_,_)) = false
fun assertDogsEqual (el1:entity_list) (el2:entity_list) (desc:string) = assertTrue(compareDogs(el1, el2)) desc;

fun compareStates (state1 as state(cat1, dogs1, goal1, size1, g1, w1),
                   state2 as state(cat2, dogs2, goal2, size2, g2, w2)) =
    (compareEntities(cat1, cat2) andalso compareDogs(dogs1, dogs2) andalso
     compareEntities(goal1, goal2) andalso compareSizes(size1, size2) andalso
     (g1 = g2) andalso (w1 = w2))
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

fun countCell(Cells as cell_cons(Cell, Rest), I) = countCell(Rest, I+1.0)
  | countCell(Cells as cell_nil, I) = I;

fun sumCell(Cells as cell_cons(Cell, Rest), I) = sumCell(Rest, I+Cell)
  | sumCell(Cells as cell_nil, I) = I;

fun sumCellList(List as cell_list_cons(Cells, Rest), I) =
    sumCellList(Rest, I+sumCell(Cells, 0.0))
  | sumCellList(List as cell_list_nil, I) = I;

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
assertRealSigmaEqual (getQuadDistance(dog1, dog4)) (sqrt(pow(4.0, 2.0) * 2.0)) "(1.0, 1.0) -> (5.0, 5.0) distance wrong";
assertRealSigmaEqual (getQuadDistance(dog4, dog1)) (sqrt(pow(4.0, 2.0) * 2.0)) "(5.0, 5.0) -> (1.0, 1.0) distance wrong";
assertRealSigmaEqual (getQuadDistance(dog2, dog3)) (sqrt(pow(0.4, 2.0))) "(0.0, 0.0) -> (0.4, 0.0) distance wrong";

(* initCells *)
val testcells1 = initCells(16.0, 16.0);
val testcells2 = initCells(3.0, 2.0);
assertRealSigmaEqual (countCell(testcells1, 0.0)) (16.0*16.0) "Cell count wrong for 16x16";
assertRealSigmaEqual (sumCell(testcells1, 0.0)) 0.0 "Wrong sum for 16x16";
assertRealSigmaEqual (countCell(testcells2, 0.0)) (3.0*2.0) "Cell count wrong for 3x2";
assertRealSigmaEqual (sumCell(testcells2, 0.0)) 0.0 "Wrong sum for 3x2";

(* increaseCell *)

val checkcell = increaseCell(testcells1, point(1.0, 1.0), 16.0);
assertTrue (checkCell(checkcell, 17, 0, 1.0)) "Did not increase correct cell";
val checkcell = increaseCell(testcells1, point(15.5, 2.5), 16.0);
assertTrue (checkCell(checkcell, 47, 0, 1.0)) "Partial coordinates did not increase correct cell";
val checkcell = increaseCell(increaseCell(increaseCell(increaseCell(increaseCell(testcells1, point(15.5, 2.5), 16.0), point(15.5, 4.5), 16.0), point(12.5, 2.5), 16.0), point(3.5, 2.5), 16.0), point(15.5, 2.5), 16.0);
assertRealSigmaEqual (sumCell(checkcell, 0.0)) 5.0 "Sum of visited cells != 5.0";

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

val testlist1 = initCellList((16.0, 16.0, dogs));

val (teststate, movedlist) = applyMoves(state1, moves, testlist1);
assertStatesEqual teststate movedstate "Did not move correctly";

case movedlist
 of cell_list_cons(cells1, cell_list_cons(cells2, cell_list_cons(cells3, _))) =>
    (
     assertTrue (checkCell(cells1, (3*16)+5, 0, 1.0)) "Dogs did not visit proper cell, up";
     assertTrue (checkCell(cells2, (5*16)+3, 0, 1.0)) "Dogs did not visit proper cell, left";
     assertTrue (checkCell(cells3, (6*16)+5, 0, 1.0)) "Dogs did not visit proper cell, down"
    );

val (teststate, _) = applyMoves(state1, incompletemoves, testlist1);
assertStatesEqual teststate incompletemovedstate "Did not move correctly with incomplete moves";
val (teststate, _) = applyMoves(state1, dir_cons(up, dir_nil), testlist1);
assertStatesEqual teststate justcatmovedstate "Did not move correctly with just cat moves";
val (teststate, _) = applyMoves(state1, dir_nil, testlist1);
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

(* simtick *)
val (tickstate, tickcells) = simtick(state1, initCellList(16.0, 16.0, dogs), 1);
assertFalse (compareStates(state1, tickstate)) "State did not change during tick";
assertTrue (realGreater(sumCellList(tickcells, 0.0), 0.0)) "Did not visit any cells";

(* initState *)
val expectedgoal = rect(point(8.0, 1.0), goalsize);
val teststate = initState(size(16.0, 16.0), catradius, dogs, goalsize, circle(point(8.0, 15.5), catradius));
case teststate
 of state(circle(point(X,Y), radius(R)), _,_,_,_,_) => (
    assertStatesEqual (teststate) (state(circle(point(X, 15.5), catradius), dogs, expectedgoal, size(16.0, 16.0), false, false)) "Init wrong state")
 | _ => assertTrue (false) "Wrong cat";

(* exitAchiever *)
val eagoal = rect(point(8.0, 1.0), goalsize);
val eacat1 = circle(point(0.0, 4.0), catradius);
val fieldsize = size(16.0, 16.0);

assertDirectionsEqual (exitAchiever(eacat1, eacat1, dogs, eagoal, fieldsize)) right "Did not move right";

val eacat2 = circle(point(8.0, 16.0), catradius);
assertDirectionsEqual (exitAchiever(eacat2, eacat2, dogs, eagoal, fieldsize)) up "Did not move up";

val eacat3 = circle(point(16.0, 4.0), catradius);
assertDirectionsEqual (exitAchiever(eacat3, eacat3, dogs, eagoal, fieldsize)) left "Did not move left";

(* potentialFieldCat *)
(* TODO: regenerate tests

val pfbcat = circle(point(12.678196, 15.250000), radius(0.750000));
val pfbgoal = rect(point(8.000000, 1.000000), size(5.0, 2.0));
val pfbdogs = entity_cons(rect(point(5.202365, 6.007207), size(1.500000, 1.500000)),
              entity_cons(rect(point(3.906677, 2.837593), size(1.500000, 1.500000)),
              entity_cons(rect(point(4.624321, 3.300351), size(1.500000, 1.500000)),
              entity_cons(rect(point(6.350092, 4.929192), size(1.500000, 1.500000)), entity_nil))));
assertEqual (potentialFieldCat(pfbcat, pfbcat, pfbdogs, pfbgoal, size(16.000000, 16.000000))) (left) "Did not generate the right direction 0";

val pfbcat = circle(point(1.122862, 15.250000), radius(0.750000));
val pfbgoal = rect(point(8.000000, 1.000000), size(5.0, 2.0));
val pfbdogs = entity_cons(rect(point(4.832332, 2.547553), size(1.500000, 1.500000)),
              entity_cons(rect(point(4.362792, 3.075027), size(1.500000, 1.500000)),
              entity_cons(rect(point(9.642768, 1.773553), size(1.500000, 1.500000)),
              entity_cons(rect(point(2.083688, 6.908737), size(1.500000, 1.500000)), entity_nil))));
assertEqual (potentialFieldCat(pfbcat, pfbcat, pfbdogs, pfbgoal, size(16.000000, 16.000000))) (right) "Did not generate the right direction 1";

val pfbcat = circle(point(8.637816, 15.250000), radius(0.750000));
val pfbgoal = rect(point(8.000000, 1.000000), size(5.0, 2.0));
val pfbdogs = entity_cons(rect(point(9.683803, 6.768601), size(1.500000, 1.500000)),
              entity_cons(rect(point(2.187700, 4.529318), size(1.500000, 1.500000)),
              entity_cons(rect(point(3.113659, 6.400661), size(1.500000, 1.500000)),
              entity_cons(rect(point(10.948328, 0.999753), size(1.500000, 1.500000)), entity_nil))));
assertEqual (potentialFieldCat(pfbcat, pfbcat, pfbdogs, pfbgoal, size(16.000000, 16.000000))) (up) "Did not generate the right direction 2";

val pfbcat = circle(point(4.100925, 15.250000), radius(0.750000));
val pfbgoal = rect(point(8.000000, 1.000000), size(5.0, 2.0));
val pfbdogs = entity_cons(rect(point(14.055367, 3.616983), size(1.500000, 1.500000)),
              entity_cons(rect(point(7.974229, 3.511837), size(1.500000, 1.500000)),
              entity_cons(rect(point(13.118111, 5.650652), size(1.500000, 1.500000)),
              entity_cons(rect(point(8.523403, 5.312626), size(1.500000, 1.500000)), entity_nil))));
assertEqual (potentialFieldCat(pfbcat, pfbcat, pfbdogs, pfbgoal, size(16.000000, 16.000000))) (right) "Did not generate the right direction 3";

val pfbcat = circle(point(12.307397, 15.250000), radius(0.750000));
val pfbgoal = rect(point(8.000000, 1.000000), size(5.0, 2.0));
val pfbdogs = entity_cons(rect(point(4.699375, 1.229002), size(1.500000, 1.500000)),
              entity_cons(rect(point(0.915708, 2.398130), size(1.500000, 1.500000)),
              entity_cons(rect(point(6.937085, 5.007756), size(1.500000, 1.500000)),
              entity_cons(rect(point(8.335199, 0.887783), size(1.500000, 1.500000)), entity_nil))));
assertEqual (potentialFieldCat(pfbcat, pfbcat, pfbdogs, pfbgoal, size(16.000000, 16.000000))) (left) "Did not generate the right direction 4";

val pfbcat = circle(point(9.836667, 15.250000), radius(0.750000));
val pfbgoal = rect(point(8.000000, 1.000000), size(5.0, 2.0));
val pfbdogs = entity_cons(rect(point(8.630828, 1.720254), size(1.500000, 1.500000)),
              entity_cons(rect(point(10.703360, 3.176901), size(1.500000, 1.500000)),
              entity_cons(rect(point(8.377096, 3.071316), size(1.500000, 1.500000)),
              entity_cons(rect(point(4.704751, 6.242350), size(1.500000, 1.500000)), entity_nil))));
assertEqual (potentialFieldCat(pfbcat, pfbcat, pfbdogs, pfbgoal, size(16.000000, 16.000000))) (left) "Did not generate the right direction 5";

val pfbcat = circle(point(1.087577, 15.250000), radius(0.750000));
val pfbgoal = rect(point(8.000000, 1.000000), size(5.0, 2.0));
val pfbdogs = entity_cons(rect(point(11.529223, 5.941070), size(1.500000, 1.500000)),
              entity_cons(rect(point(11.695907, 5.679923), size(1.500000, 1.500000)),
              entity_cons(rect(point(6.048183, 5.391677), size(1.500000, 1.500000)),
              entity_cons(rect(point(8.763626, 5.110556), size(1.500000, 1.500000)), entity_nil))));
assertEqual (potentialFieldCat(pfbcat, pfbcat, pfbdogs, pfbgoal, size(16.000000, 16.000000))) (right) "Did not generate the right direction 6";

val pfbcat = circle(point(9.259264, 15.250000), radius(0.750000));
val pfbgoal = rect(point(8.000000, 1.000000), size(5.0, 2.0));
val pfbdogs = entity_cons(rect(point(8.079236, 3.131562), size(1.500000, 1.500000)),
              entity_cons(rect(point(11.103833, 7.135100), size(1.500000, 1.500000)),
              entity_cons(rect(point(6.370609, 2.821092), size(1.500000, 1.500000)),
              entity_cons(rect(point(10.037263, 6.902399), size(1.500000, 1.500000)), entity_nil))));
assertEqual (potentialFieldCat(pfbcat, pfbcat, pfbdogs, pfbgoal, size(16.000000, 16.000000))) (up) "Did not generate the right direction 7";

val pfbcat = circle(point(15.125602, 15.250000), radius(0.750000));
val pfbgoal = rect(point(8.000000, 1.000000), size(5.0, 2.0));
val pfbdogs = entity_cons(rect(point(12.800516, 2.391220), size(1.500000, 1.500000)),
              entity_cons(rect(point(14.076107, 4.653940), size(1.500000, 1.500000)),
              entity_cons(rect(point(0.936711, 2.482010), size(1.500000, 1.500000)),
              entity_cons(rect(point(0.906935, 5.614521), size(1.500000, 1.500000)), entity_nil))));
assertEqual (potentialFieldCat(pfbcat, pfbcat, pfbdogs, pfbgoal, size(16.000000, 16.000000))) (left) "Did not generate the right direction 8";

val pfbcat = circle(point(8.546024, 15.250000), radius(0.750000));
val pfbgoal = rect(point(8.000000, 1.000000), size(5.0, 2.0));
val pfbdogs = entity_cons(rect(point(6.370148, 4.593856), size(1.500000, 1.500000)),
              entity_cons(rect(point(6.452047, 3.473053), size(1.500000, 1.500000)),
              entity_cons(rect(point(14.455731, 3.100848), size(1.500000, 1.500000)),
              entity_cons(rect(point(11.563294, 1.895398), size(1.500000, 1.500000)), entity_nil))));
assertEqual (potentialFieldCat(pfbcat, pfbcat, pfbdogs, pfbgoal, size(16.000000, 16.000000))) (up) "Did not generate the right direction 9";*)

(* aiStep *)
val expecteddirections = dir_cons(up, dir_cons(right, dir_cons(right, dir_cons(right, dir_nil))));
assertDirectionListsEqual (aiStep(state1, 1)) expecteddirections "Directions were wrong";

(* kNearest *)
val knearest_dogs = entity_cons(dog4, entity_cons(dog1, entity_cons(dog2, entity_cons(dog3, entity_nil))));
val expected_nearest_1 = entity_cons(dog3, entity_nil);
val found = kNearest(dog2, knearest_dogs, 1.0, 2);
assertDogsEqual (found) expected_nearest_1 "Not the right nearest dogs; k=1";

val expected_nearest_2 = entity_cons(dog3, entity_cons(dog1, entity_nil));
val found = kNearest(dog2, knearest_dogs, 2.0, 2);
assertDogsEqual (found) expected_nearest_2 "Not the right nearest dogs; k=2";

val knearest_with_dup = entity_cons(dog2, knearest_dogs);
val found = kNearest(dog2, knearest_with_dup, 2.0, 3);
val expected_nearest_dup = entity_cons(dog2, entity_cons(dog3, entity_nil));
assertDogsEqual (found) expected_nearest_dup "Not the right nearest dogs with dup; k=2";

(* main *)
(* TODO: rewrite tests

fun countTicks(Ticks as tick_cons(T,R), N) =
    countTicks(R, N+1.0)
  | countTicks(Ticks as tick_nil, N) = N;
fun countVisits(Visits as visit_cons(V,R), N) =
    countVisits(R, N+1.0)
  | countVisits(Visits as visit_nil, N) = N;

fun checkResult(derp as result(N, Ticks, Visits), CheckN) =
  let
     val tick_count = countTicks(Ticks, 0.0);
     val visit_count = countVisits(Visits, 0.0);
   in
    assertRealSigmaEqual CheckN N ("Did not run the necessary amount of times (" ^ (Real.toString(N)) ^ " != " ^ (Real.toString(CheckN)) ^ ")");
    assertRealSigmaEqual CheckN tick_count ("Did not create the necessary amount of ticks (" ^ (Real.toString(tick_count)) ^ " != " ^ (Real.toString(CheckN)) ^ ")");
    assertRealSigmaEqual CheckN visit_count ("Did not create the necessary amount of visits (" ^ (Real.toString(visit_count)) ^ " != " ^ (Real.toString(CheckN)) ^ ")")
  end;
checkResult(main(knearest_dogs, cat_ai_cons(1, cat_ai_cons(2, cat_ai_nil))), 100.0);
checkResult(main(knearest_dogs, cat_ai_cons(1, cat_ai_nil)), 50.0);*)


(* aRand *)
sRand(0);

fun testRands (N:int, Max:real, Min:real) =
  if N > 0 then
    let val rand = aRand(0.0) in (
      assertTrue (rand >= 0.0 andalso rand <= 1.0) "Rand not within bounds";
      testRands(N-1,
                (if rand > Max then rand else Max),
                (if rand < Min then rand else Min)))
    end
  else
    assertTrue (Min < 0.05 andalso Max > 0.95) "Rand did not seem uniform";

testRands(10000, 0.0, 2.0);

(* interest *)
(* FIXME
fun sanityTestResults(N:int) =
  let
    fun genGame (N, I, Ticks, Visits) =
      if I < N then
        genGame(N, I+1.0, tick_cons(realFloor(aRand(0.0)*49.0+1.0), Ticks), visit_cons(genRandCells(), Visits))
      else
        result(N, Ticks, Visits)
    and genRandCells () =
      genRandCell(realFloor(aRand(0.0)*16.0*16.0+1.0), 0.0, cell_nil)
    and genRandCell(N, I, Cells) =
      if N < I then
        Cells
      else
        genRandCell(N, I+1.0, cell_cons(realFloor(aRand(0.0) * 50.0), Cells))
    and genResult () =
      genGame(realFloor((aRand(0.0) * 99.0) + 1.0), 0.0, tick_nil, visit_nil)
  in
    if 0 < N then
      let val I = interest(genResult()) in
        (
          assertTrue (I >= 0.0 andalso Real.isFinite(I)) ("Interest value " ^ (Real.toString(I)) ^ " not within bounds");
          sanityTestResults(N-1)
        )
      end
    else ()
  end;
sanityTestResults(1000);


fun makeTicks(T :: Rest) =
    tick_cons(T, makeTicks(Rest))
  | makeTicks(nil) = tick_nil

fun makeCells(C :: Rest) =
    cell_cons(C, makeCells(Rest))
  | makeCells(nil) = cell_nil

fun makeVisits(V :: Rest) =
    visit_cons(makeCells(V), makeVisits(Rest))
  | makeVisits(nil) = visit_nil
*)

val I = interest(
result(
	3.0,
	tick_cons(20.0, tick_cons(26.0, tick_cons(28.0, tick_nil))),
		visit_cons(		cell_list_cons(cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(23.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(14.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(3.0, cell_cons(0.0, cell_cons(19.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(26.0, cell_cons(3.0, cell_cons(21.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(13.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(9.0, cell_cons(0.0, cell_cons(16.0, cell_cons(27.0, cell_cons(0.0, cell_cons(24.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_cons(cell_cons(0.0, cell_cons(23.0, cell_cons(28.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(25.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(18.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(18.0, cell_cons(11.0, cell_cons(13.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(18.0, cell_cons(16.0, cell_cons(0.0, cell_cons(20.0, cell_cons(24.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(7.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(17.0, cell_cons(13.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(15.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(22.0, cell_cons(23.0, cell_cons(0.0, cell_cons(23.0, cell_cons(2.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_cons(cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(15.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(6.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(29.0, cell_cons(4.0, cell_cons(0.0, cell_cons(1.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(27.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(3.0, cell_cons(1.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(1.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(7.0, cell_cons(18.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(18.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(9.0, cell_cons(10.0, cell_cons(0.0, cell_cons(1.0, cell_cons(10.0, cell_cons(22.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(11.0, cell_cons(10.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(22.0, cell_cons(12.0, cell_cons(0.0, cell_cons(30.0, cell_cons(15.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(19.0, cell_cons(29.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_cons(cell_cons(2.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(12.0, cell_cons(14.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(15.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(30.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(28.0, cell_cons(29.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(2.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(6.0, cell_cons(23.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(30.0, cell_cons(18.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(10.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_nil)))),
	visit_cons(		cell_list_cons(cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(26.0, cell_cons(24.0, cell_cons(11.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(15.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(22.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(19.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(2.0, cell_cons(0.0, cell_cons(27.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(28.0, cell_cons(7.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(16.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(18.0, cell_cons(30.0, cell_cons(4.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_cons(cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(2.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(25.0, cell_cons(24.0, cell_cons(0.0, cell_cons(17.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(27.0, cell_cons(0.0, cell_cons(13.0, cell_cons(23.0, cell_cons(0.0, cell_cons(23.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(27.0, cell_cons(19.0, cell_cons(20.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(11.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(24.0, cell_cons(3.0, cell_cons(0.0, cell_cons(15.0, cell_cons(6.0, cell_cons(0.0, cell_cons(3.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(19.0, cell_cons(0.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(14.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_cons(cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(19.0, cell_cons(0.0, cell_cons(22.0, cell_cons(4.0, cell_cons(15.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(3.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(30.0, cell_cons(0.0, cell_cons(16.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(27.0, cell_cons(0.0, cell_cons(4.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(3.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(27.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(13.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(6.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(30.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(22.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_cons(cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(17.0, cell_cons(22.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(27.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(30.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_cons(27.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(16.0, cell_cons(1.0, cell_cons(0.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(27.0, cell_cons(28.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(9.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(8.0, cell_cons(18.0, cell_cons(0.0, cell_cons(5.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(4.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(9.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(26.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(25.0, cell_cons(29.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_nil)))),
	visit_cons(		cell_list_cons(cell_cons(0.0, cell_cons(15.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(11.0, cell_cons(1.0, cell_cons(0.0, cell_cons(10.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(26.0, cell_cons(25.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(15.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(25.0, cell_cons(0.0, cell_cons(9.0, cell_cons(2.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(10.0, cell_cons(0.0, cell_cons(20.0, cell_cons(4.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(22.0, cell_cons(25.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(19.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(29.0, cell_cons(24.0, cell_cons(28.0, cell_cons(0.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_cons(cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(17.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(18.0, cell_cons(11.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(29.0, cell_cons(1.0, cell_cons(21.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(28.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(22.0, cell_cons(0.0, cell_cons(27.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(4.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(7.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_cons(cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(22.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(10.0, cell_cons(11.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(9.0, cell_cons(22.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(15.0, cell_cons(10.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(3.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(28.0, cell_cons(12.0, cell_cons(11.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(1.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(18.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(30.0, cell_cons(29.0, cell_cons(3.0, cell_cons(17.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(26.0, cell_cons(0.0, cell_cons(2.0, cell_cons(19.0, cell_cons(4.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(30.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(16.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(27.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(9.0, cell_cons(12.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(13.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(22.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(0.0, cell_cons(21.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_cons(cell_cons(0.0, cell_cons(7.0, cell_cons(0.0, cell_cons(2.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(5.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(20.0, cell_cons(0.0, cell_cons(28.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(27.0, cell_cons(0.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(26.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(10.0, cell_cons(0.0, cell_cons(3.0, cell_cons(18.0, cell_cons(0.0, cell_cons(9.0, cell_cons(6.0, cell_cons(2.0, cell_cons(16.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(17.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(15.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(17.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(25.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(5.0, cell_cons(0.0, cell_cons(0.0, cell_cons(4.0, cell_cons(14.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(30.0, cell_cons(12.0, cell_cons(20.0, cell_cons(0.0, cell_cons(4.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(22.0, cell_cons(30.0, cell_cons(0.0, cell_cons(9.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(2.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(25.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(11.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(16.0, cell_cons(6.0, cell_cons(9.0, cell_cons(0.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(23.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(6.0, cell_cons(0.0, cell_cons(0.0, cell_cons(1.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(29.0, cell_cons(10.0, cell_cons(3.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(8.0, cell_cons(0.0, cell_cons(0.0, cell_cons(12.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(19.0, cell_cons(5.0, cell_cons(0.0, cell_cons(29.0, cell_cons(0.0, cell_cons(12.0, cell_cons(7.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_cons(21.0, cell_cons(0.0, cell_cons(28.0, cell_cons(0.0, cell_cons(0.0, cell_cons(30.0, cell_cons(0.0, cell_cons(15.0, cell_cons(29.0, cell_cons(10.0, cell_cons(0.0, cell_cons(0.0, cell_cons(14.0, cell_cons(0.0, cell_cons(24.0, cell_cons(0.0, cell_cons(27.0, cell_cons(15.0, cell_cons(13.0, cell_cons(30.0, cell_cons(0.0, cell_cons(0.0, cell_cons(0.0, cell_nil)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))),
		cell_list_nil)))),
	visit_nil)))
)
);


assertRealSigmaEqual (I) (0.193934342597) ("The interest value returned (" ^ (Real.toString(I)) ^ ") was not correct");
