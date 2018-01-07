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
    tree_step = function(qnode_list) {
      .self$qnode = Qnode$new(initial_list=qnode_list)
    },
    physics_step = function() {
      .self$qnode$compute_mass_distribution()
      .self$qnode$compute_forces()
      .self$qnode$compute_accelerations()
      .self$qnode$update_state()
      l = .self$qnode$to_list()
    },
    drawing_step = function(qnode_list) {
      plot.new()
      for (q in qnode_list) {
        q$draw()
      }
    },
    start = function() {
      qnode_list = list()
      i <- 0
      while(i < 20) {
        q = Qnode$new(x=runif(1,0.01,0.99),y=runif(1,0.01,0.99),mass=runif(1, 0.01, 0.99)*10^7,velocity=Point$new(x=runif(1, 0.001, 0.01),y=runif(1, 0.001, 0.01)))
        qnode_list = c(qnode_list, q)
        i <- i+1
      }
      .self$tree_step(qnode_list)
      qnode_list = .self$physics_step()
      .self$drawing_step(qnode_list)
    }
  )
)

u = Universe$new()
u$start()