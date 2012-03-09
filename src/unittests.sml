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

(* getPointDistance *)
assertPointsEqual (getPointDistance(point(0.0, 0.0), point(1.0, 1.0))) (point(1.0, 1.0)) "getPointDistance 1";
assertPointsEqual (getPointDistance(point(0.0, 0.0), point(~1.0, 1.0))) (point(~1.0, 1.0)) "getPointDistance 2";
assertPointsEqual (getPointDistance(point(~5.0, 0.0), point(5.0, 0.0))) (point(10.0, 0.0)) "getPointDistance 3";
assertPointsEqual (getPointDistance(point(1000.0, 1000.0), point(~1000.0, ~1000.0))) (point(~2000.0, ~2000.0)) "getPointDistance 4";
assertPointsEqual (getPointDistance(point(2.5, 30.0), point(0.2, 41.7))) (point(~2.3, 11.7)) "getPointDistance 5";
