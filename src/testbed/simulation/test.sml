fun collideRectWithRect (x1:real, y1:real, w1:real, h1:real,x2:real,y2:real,w2:real,h2:real) : bool =
    case  (y1+(h1*0.5)) < (y2-(h2*0.5)) (* bottom1 < top2 *)
        of true => false
         | false =>
           case (y1-(h1*0.5)) > (y2+(h2*0.5)) (* top1 > bottom2 *)
            of true => false
             | false =>
               case (x1+(w1*0.5)) < (x2-(w2*0.5)) (* right1 < left2 *)
                of true => false
                 | false =>
                   case (x1-(w1*0.5)) > (x2+(w2*0.5)) (* left1 > right2 *)
                    of true => false
                     | false => true


val e = _export "coll": (real * real * real * real * real * real * real * real
                          -> bool) -> unit;
val _ = e (collideRectWithRect);
