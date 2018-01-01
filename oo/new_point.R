# definiçao da classe
new_point <- setRefClass("new_point",
                         fields = list(x = "numeric", y = "numeric"))


# definiçao dos metodos da classe

new_point$methods(get_x = function() return(.self$x))
new_point$methods(get_y = function() return(.self$y))

new_point$methods(point_sum_op = function(p) {
  new_point(x = .self$x + p$x, y = .self$y + p$y)
})

new_point$methods(point_sub_op = function(p) {
  new_point(x = .self$x - p$x, y = .self$y - p$y)
})

new_point$methods(point_quadrant_i = function(quad_center) # supondo que quad_center é um ponto tb
  if (.self$x <= quad_center$x) {
    if(.self$y <= quad_center$y) 1 else 2
  } else {
    if(.self$y <= quad_center$y) 3 else 4
  })

new_point$methods(point_subquadrant = function(quad) {
  quad$quad_sub(.self$point_quadrant_i(quad$quad_center()))
}
)

