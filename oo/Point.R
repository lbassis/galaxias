# definiçao da classe
Point <- setRefClass("Point",
  fields = list(
    x = "numeric",
    y = "numeric"
  ),
  methods = list(
    get_x = function() return(.self$x),
    get_y = function() return(.self$y),
    sum_op = function(p) {
      Point(x = .self$x + p$x, y = .self$y + p$y)
    },
    sub_op = function(p) {
      Point(x = .self$x - p$x, y = .self$y - p$y)
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

