

# definiçao da classe
new_point <- setRefClass("new_point",
                          fields = list(x = "numeric", y = "numeric"))

# definiçao dos metodos da classe
new_point$methods(point_sum_op = function(p) {
  new_point(x = .self$x[[1]] + p$x[[1]], y = .self$y[[1]] + p$y[[1]])
})

new_point$methods(point_sub_op = function(p) {
  new_point(x = .self$x[[1]] - p$x[[1]], y = .self$y[[1]] - p$y[[1]])
})

new_point$methods(point_quadrant_i = function(quad_center) # supondo que quad_center é um ponto tb
  if (.self$x[[1]] <= quad_center$x[[1]]) {
    if(.self$y[[1]] <= quad_center$y[[1]]) 1 else 2
  } else {
    if(.self$y[[1]] <= quad_center$y[[1]]) 3 else 4
  })






new_quad <- setRefClass("new_quad",
                        fields = list(top_left = "new_point", size = "new_point"))

new_particle <- setRefClass("new_particle", 
                            fields = list(point = "new_point", mass = "numeric", velocity = "new_point",
                            force = "new_point", quadrant_size = "new_point"))

new_qnode <- setRefClass("new_qnode",
                         fields = list(particle = "new_particle", first_child = "list", second_child = "list",
                                       third_child = "list", fourth_child = "list"))


# testando uns objetos aqui
x_point = new_point(x = 1, y = 2)
x_quad = new_quad(top_left = x_point, size = x_point)
x_particle = new_particle(point = x_point, mass = 10, velocity = x_point, force = x_point, quadrant_size = x_point)
x_qnode = new_qnode(particle = x_particle, first_child = list(x_particle), second_child = list(), third_child = list(), fourth_child = list())