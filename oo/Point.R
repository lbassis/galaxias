# definiçao da classe
Point <- setRefClass("Point",
  fields = list(
    x = "numeric",
    y = "numeric"
  ),
  methods = list(
    initialize = function(.Object,x=0,y=0) {
      .self$x = x
      .self$y = y
    },
    get_x = function() return(.self$x),
    get_y = function() return(.self$y),
    sum_op = function(p) {
      Point(x = .self$x + p$x, y = .self$y + p$y)
    },
    sub_op = function(p) {
      Point(x = .self$x - p$x, y = .self$y - p$y)
    },
    pointwise_op_bin = function(p, op) { # returns new
      return(Point$new(
        x=op(.self$get_x(), p$get_x()),
        y=op(.self$get_y(), p$get_y())
      ))
    },
    pointwise_op_un = function(op) {
      return(Point$new(
        x=op(.self$get_x()),
        y=op(.self$get_y())
      ))
    },
    quadrant_i = function(quad_center) {# supondo que quad_center é um ponto tb
      if (.self$x <= quad_center$x) {
        if(.self$y <= quad_center$y) 1 else 2
      } else {
        if(.self$y <= quad_center$y) 3 else 4
      }
    },
    subquadrant = function(quad) {
      quad$quad_sub(.self$quadrant_i(quad$quad_center()))
    },
    set_x = function(x) .self$x = x,
    set_y = function(y) .self$y = y
  )
)
