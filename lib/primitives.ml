type point = {
    x : int ;
    y : int ;
}

type t =
| Circle of point * float * int
| FilledCircle of point * float * int
| Line of point * point * int
| Pixel of point * int
| Polygon of point list * int
| Rect of point * point * int
| FilledRect of point * point * int
| Char of point * Font.t * char * int
| String of point * Font.t * string * int

