source("new_particle.R")

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
