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

########################################################################################################

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


########################################################################################################

new_particle <- setRefClass("new_particle", 
                            fields = list(point = "new_point", mass = "numeric", velocity = "new_point",
                            force = "new_point", quadrant_size = "new_point"))

new_particle$methods(get_point = function() return(.self$point))
new_particle$methods(get_mass = function() return(.self$mass))
new_particle$methods(get_velocity = function() return (.self$velocity))
new_particle$methods(get_force = function() return (.self$force))
new_particle$methods(get_quadrant_size = function() return (.self$quadrant_size))

new_particle$methods(get_x = function() return(.self$get_point()$get_x()))
new_particle$methods(get_y = function() return(.self$get_point()$get_y()))
new_particle$methods(get_vx = function() return(.self$get_velocity()$get_x()))
new_particle$methods(get_vy = function() return(.self$get_velocity()$get_y()))
new_particle$methods(get_fx = function() return(.self$get_force()$get_x()))
new_particle$methods(get_fy = function() return(.self$get_force()$get_y()))



new_particle$methods(particle_to_point = function() {
  new_point(x = .self$get_point()$get_x(), y = .self$get_point()$get_y())
})

new_particle$methods(particle_set_qsize = function(qsize) {
  new_particle(point = particle_to_point(), mass = .self$get_mass(), 
              velocity = new_point(x = .self$get_vx, y = .self$get_vy()), 
              force = new_point(x = .self$get_fx(), y = .self$get_fy()),
              quadrant_size = qsize[[1]])
})

########################################################################################################


# o qnode herdando de particle faz com que nao precise mais que ele tenha o atributo particle

new_qnode <- setRefClass("new_qnode",
                         contains = "new_particle",
                         fields = list(first_child = "list", second_child = "list",
                                       third_child = "list", fourth_child = "list"))


new_qnode$methods(initialize = function(.Object, particle, ...) {
  .self$point = particle$get_point()
  .self$mass = particle$get_mass()
  .self$velocity = particle$get_velocity()
  .self$force = particle$get_force()
  .self$quadrant_size = particle$get_quadrant_size()
  callSuper(...)
  #.self$first_child = first
  #.self$second_child = second
  #.self$third_child = third
  #.self$fourth_child = fourth
})

new_qnode$methods(get_first_child = function() return(.self$first_child))
new_qnode$methods(get_second_child = function() return(.self$second_child))
new_qnode$methods(get_third_child = function() return(.self$third_child))
new_qnode$methods(get_fourth_child = function() return(.self$fourth_child))


new_qnode$methods(empty = function() length(.self) == 0)

new_qnode$methods(childs = function() if (!.self$empty()) tail(.self, length(.self)-1))

new_qnode$methods(degree = function() Reduce("+", lapply(.self$childs, function(c) as.integer(!c$empty()))))

new_qnode$methods(external = function() .self$degree() == 0)

new_qnode$methods(new_child = function(index, child_index, child) if (index==child_index) child else .self[[index]])

# given a qnode (parent), a child index [[2..5]], and another qnode (child)
#   returns a qnode similar to the parent, overwriting the child_index with child
new_qnode$methods(qnode_force_insert = function (child_index, child) {
    list(.self[[1]], .self$new_child(2, child_index, child), .self$new_child(3, child_index, child), .self$new_child(4, child_index, child), .self$new_child(5, child_index, child))
  })


new_qnode$methods(draw = function() {
  draw.circle(.self$get_x(), .self$get_y(), .self$get_mass(), col="red", nv=1000, border = NA,lty=1,lwd=1)
})


########################################################################################################


# testando uns objetos aqui
x_point = new_point(x = 1, y = 2)
x_quad = new_quad(top_left = x_point, size = 10)
x_particle = new_particle(point = x_point, mass = 10, velocity = x_point, force = x_point, quadrant_size = x_point)
x_qnode = new_qnode(particle = x_particle, first_child = list(x_particle), second_child = list(), third_child = list(), fourth_child = list())