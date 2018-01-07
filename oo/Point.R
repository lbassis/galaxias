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

setMethod("+", c("Point", "Point"), function(e1, e2) {
  Point(x = e1$get_x() + e2$get_x(), y = e1$get_y() + e2$get_y())
})

setMethod("-", c("Point", "Point"), function(e1, e2) {
  Point(x = e1$get_x() - e2$get_x(), y = e1$get_y() - e2$get_y())
})
