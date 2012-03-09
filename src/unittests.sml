(* Unit testing functions *)

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
fun assertRealSigmaEqual (expr1:real) (expr2:real) (desc:string) = assert expr1 expr2 desc realSigmaEqual;

fun assertTrue expr desc = (assertEqual expr true desc);
fun assertFalse expr desc = (assertEqual expr false desc);

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
