
module TShapes(longShape)
       where

longShape startSqr = 
  let ((sx,sy),col) = startSqr in
  [((sx,y),col) | y <- [sy-5 .. sy]]
  