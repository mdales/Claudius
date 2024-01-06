type point = {
    x : int ;
    y : int ;
}

type t = 
| Circle of point * float * int 
| Line of point * point * int
| Pixel of point * int
| Polygon of point list * int
