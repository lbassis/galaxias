# definiçao da classe
new_point <- setRefClass("new_point",
                          fields = list(x = "numeric", y = "numeric"))

# definiçao dos metodos da classe
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

########################################################################################################

new_quad <- setRefClass("new_quad",
                        fields = list(top_left = "new_point", size = "numeric"))

new_quad$methods(quad_center = function() {
  shift <- .self$size/2
  new_point(x = .self$top_left$x+shift, y = .self$top_left$y+shift)
}
)

# scale a quad preserving its center
new_quad$methods( quad_scale <- function(s) {
    shift <- (s-1)*.self$size
    new_quad(top_left = new_point(x = .self$top_left$x+shift, y = .self$top_left$y+shift) , size = s*.self$size)
  })

new_quad$methods(quad_sub = function(sub_index) {
  half_size <- .self$size/2
  if (sub_index==1) new_quad(top_left = new_point(x = .self$top_left$x, y = .self$top_left$y), size = half_size) else
    if (sub_index==2) new_quad(top_left = new_point(x = .self$top_left$x, y = half_size+.self$top_left$y), size = half_size) else
      if (sub_index==3) new_quad(top_left = half_size+new_point(x = .self$top_left$x, y = .self$top_left$y), size = half_size) else
        if (sub_index==4) new_quad(top_left = half_size+new_point(x = .self$top_left$x, y = half_size+.self$top_left$y), size = half_size)}
)


########################################################################################################

new_particle <- setRefClass("new_particle", 
                            fields = list(point = "new_point", mass = "numeric", velocity = "new_point",
                            force = "new_point", quadrant_size = "new_point"))

new_qnode <- setRefClass("new_qnode",
                         fields = list(particle = "new_particle", first_child = "list", second_child = "list",
                                       third_child = "list", fourth_child = "list"))


# testando uns objetos aqui
x_point = new_point(x = 1, y = 2)
x_quad = new_quad(top_left = x_point, size = 10)
x_particle = new_particle(point = x_point, mass = 10, velocity = x_point, force = x_point, quadrant_size = x_point)
x_qnode = new_qnode(particle = x_particle, first_child = list(x_particle), second_child = list(), third_child = list(), fourth_child = list())