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
