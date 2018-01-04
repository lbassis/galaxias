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
    initialize = function(.Object, first_child=list(), second_child=list(), third_child=list(), fourth_child=list(), ...) {
      .self$first_child = first_child
      .self$second_child = second_child
      .self$third_child = third_child
      .self$fourth_child = fourth_child
      callSuper(...)
    },
    get_first_child = function() .self$first_child,
    get_second_child = function() .self$second_child,
    get_third_child = function() .self$third_child,
    get_fourth_child = function() .self$fourth_child,
    empty = function() {
      length(.self$first_child) + length(.self$second_child) + length(.self$third_child) + length(.self$fourth_child) == 0
    },
    childs = function() {
      c(first_child, second_child, third_child, fourth_child)
    },
    degree = function() Reduce("+", lapply(.self$childs, function(c) as.integer(!c$empty()))),
    external = function() .self$empty(),
    new_child = function(index, child_index, child) if (index==child_index) child else .self[[index]],
    # given a qnode (parent), a child index [[2..5]], and another qnode (child)
    #   returns a qnode similar to the parent, overwriting the child_index with child
    force_insert = function (child_index, child) {
      list(.self[[1]], .self$new_child(2, child_index, child), .self$new_child(3, child_index, child), .self$new_child(4, child_index, child), .self$new_child(5, child_index, child))
    },
    draw = function() {
      draw.circle(.self$get_x(), .self$get_y(), .self$get_mass(), col="red", nv=1000, border = NA,lty=1,lwd=1)
    },

    compute_center_of_mass = function() {

    }
  )
)

## TESTS
qnode = Qnode$new(first_child=list(Qnode$new()), second_child=list(Qnode$new()))
qnode$external()
childs = qnode$childs()
for(i in childs){
  print(i)
}