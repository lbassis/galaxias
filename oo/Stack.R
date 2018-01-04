
Stack <- setRefClass("Stack",
  fields = list(
    stack = "list"
  ),
  methods = list(
    push = function(e) {
      .self$stack = c(e, .self$stack)
    },
    pop = function() {
      first = head(.self$stack, 1)
      .self$stack = tail(.self$stack, length(.self$stack) - 1)
      return(first)
    },
    is_empty = function() {
      length(.self$stack) == 0
    }
  )
)
