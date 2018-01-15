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
    greatest_mass = function(qnode_list) {
      
      greatest = 0
      i <- 0
      for (q in qnode_list) {
        mass = q$get_mass()

        if (mass > greatest) {
          greatest = mass
        }
        i <- i+1
      }
      print(greatest)
      return(greatest)
    },
    drawing_step = function(qnode_list, name, greatest) {
      
      
      filename <- paste(name, ".jpg")
      png(filename = filename)
      frame()
      
      plot.new()
      for (q in qnode_list) {
        q$draw(greatest)
      }
      dev.off()
      
      print(filename)
    },
    
    start = function(iterations, n_particles, out_file) {
      qnode_list = list()
      i <- 0
      while(i < n_particles) {
        q = Qnode$new(x=runif(1,0.01,0.99),y=runif(1,0.01,0.99),mass=runif(1, 0.01, 0.99)*10^7)
        qnode_list = c(qnode_list, q)
        i <- i+1
      }
      
      greatest <- 10*.self$greatest_mass(qnode_list)
      i <- 0
      name <- "a"
      
      while (i < iterations) {
        .self$tree_step(qnode_list)
        qnode_list = .self$physics_step()
        .self$drawing_step(qnode_list, name, greatest)
        name <- paste(name, "a")
        i <- i+1
      }
      
      system(paste("convert -delay 10 *.jpg ", out_file, ".gif", sep=""))
      file.remove(list.files(pattern=".jpg"))
      
    }
  )
)

u = Universe$new()
u$start(3, 20, "result1")
u$start(30, 10, "result2")