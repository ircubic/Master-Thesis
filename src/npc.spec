
fun f( (val, val2) : real * real ) : real =
    raise D1

fun main( (val, val2) : real * real ) : real =
    f(val, val2)

%%

val Inputs = []
val Outputs = []

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
  if Vector.sub( All_outputs, I ) <> Y then
    { numCorrect = 0, numWrong = 1, grade = () }
  else
    { numCorrect = 1, numWrong = 0, grade = () }


val Max_output_genus_card = 2

val Max_time_limit = 1024
val Time_limit_base = 1024.0
