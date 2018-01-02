source("Point.R")

Quad <- setRefClass("Quad",
  fields = list(
    top_left = "Point",
    size = "numeric"
  ),
  methods = list(
    get_top_left = function() return(.self$top_left),
    get_size = function() return(.self$size),
    quad_center = function() {
      shift <- .self$size/2
      Point(x = .self$get_top_left()$get_x()+shift, y = .self$get_top_left()$get_y()+shift)
    },
    # scale a quad preserving its center
    quad_scale = function(s) {
      shift <- (s-1)*.self$get_size()
      Quad(top_left = Point(x = .self$get_top_left()$get_x()+shift, y = .self$get_top_left()$get_y()+shift) , size = s*.self$get_size())
    },
    quad_sub = function(sub_index) {
      half_size <- .self$get_size()/2
      if (sub_index==1) Quad(top_left = Point(x = .self$get_top_left()$get_x(), y = .self$get_top_left()$get_y()), size = half_size) else
        if (sub_index==2) Quad(top_left = Point(x = .self$get_top_left()$get_x(), y = half_size+.self$get_top_left()$get_y()), size = half_size) else
          if (sub_index==3) Quad(top_left = half_size+Point(x = .self$get_top_left()$get_x(), y = .self$get_top_left()$get_y()), size = half_size) else
            if (sub_index==4) Quad(top_left = half_size+Point(x = .self$get_top_left()$get_x(), y = half_size+.self$get_top_left()$get_y()), size = half_size)
      }
  )
  
)


