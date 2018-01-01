source("new_point.R")

new_quad <- setRefClass("new_quad",
                        fields = list(top_left = "new_point", size = "numeric"))

new_quad$methods(get_top_left = function() return(.self$top_left))
new_quad$methods(get_size = function() return(.self$size))

new_quad$methods(quad_center = function() {
  shift <- .self$size/2
  new_point(x = .self$get_top_left()$get_x()+shift, y = .self$get_top_left()$get_y()+shift)
}
)

# scale a quad preserving its center
new_quad$methods( quad_scale = function(s) {
  shift <- (s-1)*.self$get_size()
  new_quad(top_left = new_point(x = .self$get_top_left()$get_x()+shift, y = .self$get_top_left()$get_y()+shift) , size = s*.self$get_size())
})

new_quad$methods(quad_sub = function(sub_index) {
  half_size <- .self$get_size()/2
  if (sub_index==1) new_quad(top_left = new_point(x = .self$get_top_left()$get_x(), y = .self$get_top_left()$get_y()), size = half_size) else
    if (sub_index==2) new_quad(top_left = new_point(x = .self$get_top_left()$get_x(), y = half_size+.self$get_top_left()$get_y()), size = half_size) else
      if (sub_index==3) new_quad(top_left = half_size+new_point(x = .self$get_top_left()$get_x(), y = .self$get_top_left()$get_y()), size = half_size) else
        if (sub_index==4) new_quad(top_left = half_size+new_point(x = .self$get_top_left()$get_x(), y = half_size+.self$get_top_left()$get_y()), size = half_size)}
)

