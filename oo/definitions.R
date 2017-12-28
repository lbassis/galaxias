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

new_particle$methods(particle_to_point = function() {
  new_point(x = .self$point$x, y = .self$point$y)
})

new_particle$methods(particle_set_qsize = function(qsize) {
  new_particle(point = particle_to_point(), mass = .self$mass, 
              velocity = new_point(x = .self$velocity$x., y = .self$velocity$y), 
              force = new_point(x = .self$force$x, y = .self$force$y),
              quadrant_size = qsize[[1]])
})

########################################################################################################

new_qnode <- setRefClass("new_qnode",
                         fields = list(particle = "new_particle", first_child = "list", second_child = "list",
                                       third_child = "list", fourth_child = "list"))

new_qnode$methods(qnode_empty = function() length(.self) == 0)

new_qnode$methods(qnode_childs = function() if (!.self$qnode_empty()) tail(.self, length(.self)-1))

new_qnode$methods(qnode_degree = function() Reduce("+", lapply(.self$qnode_childs, function(c) as.integer(!c$qnode_empty()))))

new_qnode$methods(qnode_external = function() .self$qnode_degree() == 0)

new_qnode$methods(new_child <- function(index, child_index, child) if (index==child_index) child else .self[[index]])

# given a qnode (parent), a child index [[2..5]], and another qnode (child)
#   returns a qnode similar to the parent, overwriting the child_index with child
new_qnode$methods(qnode_force_insert = function (child_index, child) {
    list(.self[[1]], .self$new_child(2, child_index, child), .self$new_child(3, child_index, child), .self$new_child(4, child_index, child), .self$new_child(5, child_index, child))
  })


########################################################################################################


# testando uns objetos aqui
x_point = new_point(x = 1, y = 2)
x_quad = new_quad(top_left = x_point, size = 10)
x_particle = new_particle(point = x_point, mass = 10, velocity = x_point, force = x_point, quadrant_size = x_point)
x_qnode = new_qnode(particle = x_particle, first_child = list(x_particle), second_child = list(), third_child = list(), fourth_child = list())