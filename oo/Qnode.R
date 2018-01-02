library(plotrix)
source("Particle.R")

# o qnode herdando de particle faz com que nao precise mais que ele tenha o atributo particle

Qnode <- setRefClass("Qnode",
  contains = "Particle",
  fields = list(
    first_child = "list",
    second_child = "list",
    third_child = "list",
    fourth_child = "list"
  ),
  methods = list(
    initialize = function(.Object, particle, ...) {
      .self$x = particle$get_x()
      .self$y = particle$get_y()
      .self$mass = particle$get_mass()
      .self$velocity = particle$get_velocity()
      .self$force = particle$get_force()
      .self$quadrant_size = particle$get_quadrant_size()
      callSuper(...)
      #.self$first_child = first
      #.self$second_child = second
      #.self$third_child = third
      #.self$fourth_child = fourth
    },
    get_first_child = function() .self$first_child,
    get_second_child = function() .self$second_child,
    get_third_child = function() .self$third_child,
    get_fourth_child = function() .self$fourth_child,
    empty = function() length(.self) == 0,
    childs = function() if (!.self$empty()) tail(.self, length(.self)-1),
    degree = function() Reduce("+", lapply(.self$childs, function(c) as.integer(!c$empty()))),
    external = function() .self$degree() == 0,
    new_child = function(index, child_index, child) if (index==child_index) child else .self[[index]],
    # given a qnode (parent), a child index [[2..5]], and another qnode (child)
    #   returns a qnode similar to the parent, overwriting the child_index with child
    qnode_force_insert = function (child_index, child) {
      list(.self[[1]], .self$new_child(2, child_index, child), .self$new_child(3, child_index, child), .self$new_child(4, child_index, child), .self$new_child(5, child_index, child))
    },
    draw = function() {
      draw.circle(.self$get_x(), .self$get_y(), .self$get_mass(), col="red", nv=1000, border = NA,lty=1,lwd=1)
    }
  )
)
