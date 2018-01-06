
Stack <- setRefClass("Stack",
  fields = list(
    stack = "list",
    type = "character"
  ),
  methods = list(
    push = function(e) {
      if (class(e) != type) {
        stop("Argument type should be ",type," but received argument has type ",class(e))
      } else {
        .self$stack = c(e, .self$stack)
      }
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
