source("Qnode.R")
source("Stack.R")

Universe <- setRefClass("Universe",
  fields = list(
    qnode = "Qnode"
  ),
  methods = list(
    initialize = function(.Object, qnode=Qnode$new()) {
      .self$qnode = qnode
    },
    parte_do_reges = function() {
      qnode$compute_mass_distribution()
      qnode$compute_forces()
      qnode$compute_accelerations()
      qnode$update_state()
      l = qnode$to_list()
    }
  )
)