source("Qnode.R")
source("Stack.R")

Universe <- setRefClass("Universe",
  fields = list(
    qnode = "Qnode"
  ),
  methods = list(
    initialize = function(.Object, qnode=Qnode$new()) {
      .self$qnode = qnode
    }
  )
)
