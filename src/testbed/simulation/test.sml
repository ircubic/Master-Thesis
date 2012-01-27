val collideRectWithRect =
 fn (x1, y1, w1, h1,x2,y2,w2,h2) =>
    let
        val left1 = x1-(w1*0.5)
        val right1 = x1+(w1*0.5)
        val top1 = y1-(h1*0.5)
        val bottom1 = y1+(h1*0.5)
        val left2 = x2-(w2*0.5)
        val right2 = x2+(w2*0.5)
        val top2 = y2-(h2*0.5)
        val bottom2 = y2+(h2*0.5)
    in
        not (bottom1 < top2 orelse top1 > bottom2 orelse
             right1 < left2 orelse left1 > right2)
    end

val e = _export "coll": (real * real * real * real * real * real * real * real
                          -> bool) -> unit;
val _ = e (collideRectWithRect)
