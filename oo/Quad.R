source("Point.R")

Quad <- setRefClass("Quad",
  fields = list(
    top_left = "Point",
    size = "numeric"
  ),
  methods = list(
    get_top_left = function() return(.self$top_left),
    get_size = function() return(.self$size),
    set_top_left = function(x) .self$top_left <- x,
    set_size = function(x) .self$size <- max(x, 0),
    update_top_left = function(x) {
      # given a new top_left point, adjusts .self in order to fit it
      diff = x$pointwise_op_bin(.self$top_left, function(a,b)b-a)
      diff_max = diff$get_max()
      .self$set_size(.self$size + diff_max);
      
      .self$set_top_left(.self$top_left$pointwise_op_bin(x, function(a,b)min(a,b)))
    },
    update_bottom = function(x) {
      # same as update_top_left
      
      diff = x$pointwise_op_bin(.self$bottom_point(), function(a,b)a-b)
      .self$set_size(.self$size + diff$get_max())
    },
    quad_center = function() {
      shift <- .self$size/2
      Point(x = .self$get_top_left()$get_x()+shift, y = .self$get_top_left()$get_y()+shift)
    },
    bottom_point = function() return(.self$top_left$pointwise_op_un(function(x)x+.self$size)),
    # scale a quad preserving its center
    quad_scale = function(s) {
      shift <- (s-1)*.self$get_size()
      Quad(top_left = Point(x = .self$get_top_left()$get_x()+shift, y = .self$get_top_left()$get_y()+shift) , size = s*.self$get_size())
    },
    quad_sub = function(sub_index) {
      half_size <- .self$get_size()/2
      if (sub_index==1) Quad(top_left = Point(x = .self$get_top_left()$get_x(), y = .self$get_top_left()$get_y()), size = half_size) else
        if (sub_index==2) Quad(top_left = Point(x = .self$get_top_left()$get_x(), y = half_size+.self$get_top_left()$get_y()), size = half_size) else
          if (sub_index==3) Quad(top_left = Point(x = half_size+.self$get_top_left()$get_x(), y = .self$get_top_left()$get_y()), size = half_size) else
            if (sub_index==4) Quad(top_left = oint(x = half_size+P.self$get_top_left()$get_x(), y = half_size+.self$get_top_left()$get_y()), size = half_size)
    },
    fit_point = function(point) {
      bottom = .self$bottom_point();
      #print(bottom)
      
      diff_top = point$pointwise_op_bin(.self$top_left, function(a,b)b-a)
      #print(diff_top)
      if (diff_top$get_max() > 0) .self$update_top_left(point)
      
      #print(.self)
      
      diff_bot = point$pointwise_op_bin(.self$bottom_point(), function(a,b)a-b)
      #print(diff_bot)
      if (diff_bot$get_max() > 0) .self$update_bottom(point)
      
    }
  )
  
)


