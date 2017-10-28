quad1 <- data.frame(x=0, y=5, wid=10, hei=5)
point1 <- data.frame(x=3, y=4)

quad_area <- function(quad) 
  quad$hei * quad$wid

point_sum <- function(p1, p2)
  data.frame(x=(p1$x+p2$x), y=(p1$y+p2$y))

point_in_quad <- function(quad, point)
  point$x >= quad$x && point$x <= (quad$x + quad$wid) &&
  point$y >= quad$y && point$y <= (quad$y + quad$hei)

#  MEEEEU CUIDADO NESTED IF ELSE TEM QUE SER NA MESMA LINHA!!!
split_quad <- function(quad, mode)
  if(mode == 1) { # top-left
    return(data.frame(
      x = quad$x, y=quad$y,
      wid = quad$wid/2, hei=quad$hei/2
    ))
  } else if(mode == 2) { # top-right
    return(data.frame(
      x = (quad$x + quad$wid/2), y=quad$y,
      wid = quad$wid/2, hei=quad$hei/2
    ))
  } else if(mode == 3) { # do you even
    return(data.frame(
      x = quad$x, y=(quad$y + quad$hei/2),
      wid = quad$wid/2, hei=quad$hei/2
    ))
  } else if (mode == 4) { # infer
    return(data.frame(
      x = (quad$x + quad$wid/2), y=(quad$y + quad$hei/2),
      wid = quad$wid/2, hei=quad$hei/2
    ))
  }

apply_to_ellipsi <- function (f, ...)
  f(...)

apply_splitting_quad <- function(quad, ewise_func, merge_func)
  merge_func(
    ewise_func(split_quad(quad, 1)),
    ewise_func(split_quad(quad, 2)),
    ewise_func(split_quad(quad, 3)),
    ewise_func(split_quad(quad, 4))
  )

quad_area(quad1)
point2 <- point_sum(point1, point1)
point_in_quad(quad1, point1)
point_in_quad(quad1, point2)

apply_to_ellipsi(c, 'a', 'b')

split_quad(quad1, 1)
split_quad(quad1, 2)
split_quad(quad1, 3)
split_quad(quad1, 4)

apply_splitting_quad(quad1, function(i) i, list)
apply_splitting_quad(quad1, function(i) point_in_quad(i, point2), list)