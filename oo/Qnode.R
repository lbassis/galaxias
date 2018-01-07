library(plotrix)
source("Particle.R")
source("Stack.R")

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
    initialize = function(.Object, initial_list=list(), first_child=list(), second_child=list(), third_child=list(), fourth_child=list(), ...) {

      if (length(initial_list) > 0) { # from_list
        particles = initial_list
        
        root_quad = Quad$new(top_left=Point$new(x=0, y=0), size=0)
        for (p in particles) { root_quad$fit_point(p$to_point())}
        #print(root_quad)
        
        
        p_head = particles[[1]]
        p_head$set_quadrant_size(root_quad$size)
        .self <<- p_head$copy()
        #print(.self)
        
        p_tail = tail(particles, n=length(particles)-1)
        #print(length(p_tail))
        
        for (p in p_tail) .self$quad_insert(root_quad, p) 
      }
      else { # keyword init      
        .self$first_child = first_child
        .self$second_child = second_child
        .self$third_child = third_child
        .self$fourth_child = fourth_child
      
        callSuper(...)
      }
    },
    finalize = function() {
      set_first_child(list())
      set_second_child(list())
      set_third_child(list())
      set_fourth_child(list())
      print("objeto finalizado")
    },
    
    get_first_child = function() .self$first_child,
    get_second_child = function() .self$second_child,
    get_third_child = function() .self$third_child,
    get_fourth_child = function() .self$fourth_child,
    get_child_from_index = function(index) {
      if(index==1) return(.self$get_first_child());
      if(index==2) return(.self$get_second_child());
      if(index==3) return(.self$get_third_child());
      if(index==4) return(.self$get_fourth_child());
    },
    set_first_child = function(child) .self$first_child <- list(child),
    set_second_child = function(child) .self$second_child <- list(child),
    set_third_child = function(child) .self$third_child <- list(child),
    set_fourth_child = function(child) .self$fourth_child <- list(child),
    childs = function() {
      c(.self$first_child, .self$second_child, .self$third_child, .self$fourth_child)
    },
    
    external = function() .self$degree() == 0,
    degree = function() {
      #print(.self$childs());
      return(length(.self$childs()))
      #Reduce("+", lapply(list(.self$childs()), function(c) as.integer(TRUE))) #as.integer(!c[[1]]$empty()))),
    },
    force_insert = function(child_index, child) {
      # overwrites

      if(child_index==1)       .self$set_first_child(child)
      else if (child_index==2) .self$set_second_child(child)
      else if (child_index==3) .self$set_third_child(child)
      else if (child_index==4) .self$set_fourth_child(child)
    },
    force_quad_insert = function(quad, child) {
      child_index = child$quadrant_i(quad$quad_center())
      child$set_quadrant_size(quad$size * 0.5)
      .self$force_insert(child_index, child)
    },
    intern_node = function(quad) {
      if (.self$external()) {
        child = .self$copy()
        child$set_quadrant_size(quad$size/2.0)
        
        force_quad_insert(quad, child)
      }
    },
    quad_insert = function(root_quad, particle) {
      qnode_ptr <- .self
      quad_ptr <- root_quad
      found = FALSE
      
      while(found == FALSE) {
        while(qnode_ptr$external() == FALSE) { # attempts to insert at every internal node, untils stalemate
            quad_index = particle$quadrant_i(quad_ptr$quad_center());
            possible_child = qnode_ptr$get_child_from_index(quad_index)

            if(length(possible_child)==0) { # succesful attempt
              #print("Empty child, inserting")
              #print(possible_child)
              qnode_ptr$force_quad_insert(quad_ptr, particle)
              
              found = TRUE
            }
            
            else { # stalemate
              #print("ACTUALLY ADVANCING")
              qnode_ptr <- possible_child[[1]]
              quad_ptr <- particle$subquadrant(quad_ptr)
            }
        }

        if(found == FALSE) { # solves stalemate and try again
          #print(" internalizing--")
          #print(qnode_ptr$degree())
          #print(quad_ptr)
          
          qnode_ptr$intern_node(quad_ptr)
        }
      }
    },
   
    draw = function() {
      draw.circle(.self$get_x(), .self$get_y(), .self$get_mass(), col="red", nv=1000, border = NA,lty=1,lwd=1)
    },
    
    euclidean_distance = function(node) {
      ((.self$get_x() - node$get_x())^2 + (.self$get_y() - node$get_y())^2)^(1/2)
    },
    to_list = function() {
      s <- Stack$new(type="Qnode")
      t <- Stack$new(type="Qnode")
      s$push(.self)
      while (!s$is_empty()) {
        node <- s$pop()[[1]]
        children = node$childs()
        for (i in children) {
          if (!i$external()) {
            s$push(i)
          } else {
            t$push(i)
          }
        }
      }
      qnode_list = list()
      while(!t$is_empty()) {
        node <- t$pop()[[1]]
        qnode_list = c(node, qnode_list)
      }
      return(qnode_list)
    },
    compute_center_of_mass = function() {
      childs = .self$childs()
      total_mass = 0
      total_x_times_mass = 0
      total_y_times_mass = 0
      for(i in childs){
        total_mass = total_mass + i$get_mass()
        total_x_times_mass = total_x_times_mass + i$get_x()*i$get_mass()
        total_y_times_mass = total_y_times_mass + i$get_y()*i$get_mass()
      }
      .self$set_mass(total_mass)
      if (total_mass != 0) {
        .self$set_x(total_x_times_mass/total_mass)
        .self$set_y(total_y_times_mass/total_mass)
      }
    },
    compute_mass_distribution = function() {
      s <- Stack$new(type="Qnode")
      t <- Stack$new(type="Qnode")
      s$push(.self)
      while (!s$is_empty()) {
        node <- s$pop()[[1]]
        children = node$childs()
        t$push(node)
        for (i in children) {
          if (!i$external()) {
            s$push(i)
          }
        }
      }
      while(!t$is_empty()) {
        node <- t$pop()[[1]]
        node$compute_center_of_mass()
      }
    },
    compute_single_force = function(node, distance_delegate) {
      G = 6.67408*(10^(-11))
      # G = 1
      m1 = .self$get_mass()
      m2 = node$get_mass()
      d = distance_delegate(node)
      if (d > 0) {
        dx = .self$get_x() - node$get_x()
        dy = .self$get_y() - node$get_y()
        f = (G*m1*m2)/d^2
        fx = f*dx/d
        fy = f*dy/d
        return(Point$new(x=fx,y=fy))
      } else {
        return(Point$new())
      }
    },
    compute_resultant_force = function(root) {
      s <- Stack$new(type="Qnode")
      t <- Stack$new(type="Point")
      s$push(root)
      while (!s$is_empty()) {
        node <- s$pop()[[1]]
        if (node$external()){
          t$push(.self$compute_single_force(node, .self$euclidean_distance))
        } else {
          r = .self$euclidean_distance(node)
          d = node$get_quadrant_size()
          theta = 1
          if (d/r < theta) {
            t$push(.self$compute_single_force(node, .self$euclidean_distance))
          } else {
            children = node$childs()
            for (i in children) {
              s$push(i)
            }
          }
        }
      }
      fx = 0
      fy = 0
      while(!t$is_empty()) {
        force <<- t$pop()[[1]]
        fx = fx + force$get_x()
        fy = fy + force$get_y()
      }
      .self$set_force(Point$new(x=fx,y=fy))
    },
    compute_forces = function() {
      qnode_list = .self$to_list()
      for(qext in qnode_list){
        qext$compute_resultant_force(.self)
      }
    },
    compute_acceleration = function() {
      mass <<- .self$get_mass()
      if (mass != 0) {
        ax = .self$get_fx()/mass
        ay = .self$get_fy()/mass
        .self$set_acceleration(Point$new(x=ax,y=ay))
      }
    },
    compute_accelerations = function() {
      qnode_list = .self$to_list()
      for(qext in qnode_list){
        qext$compute_acceleration()
      }
    },
    update_position_and_velocity = function() {
      t = 1
      ax = .self$get_ax()
      ay = .self$get_ay()
      vx = .self$get_vx()
      vy = .self$get_vy()
      x <<- .self$get_x()
      y <<- .self$get_y()
      new_vx = vx + ax*t
      new_vy = vy + ay*t
      new_x = ax*(t^2)/2 + vx*t + x
      new_y = ay*(t^2)/2 + vy*t + y
      .self$set_velocity(Point$new(x=new_vx,y=new_vy))
      .self$set_x(new_x)
      .self$set_y(new_y)
    },
    update_state = function() {
      qnode_list = .self$to_list()
      for(qext in qnode_list){
        qext$update_position_and_velocity()
      }
    }
  )
)

Particle$methods(to_qnode = function() {
  Qnode$new(
    x=.self$get_x(),
    y=.self$get_y(),
    mass=.self$get_mass(),
    velocity=.self$get_velocity(),
    force=.self$get_force(),
    acceleration=.self$get_acceleration(),
    quadrant_size=.self$get_quadrant_size()
  )
})

## TESTS
qnode = Qnode$new(quadrant_size=40)
first_child=Qnode$new(x=1,y=3,mass=1)
second_child=Qnode$new(x=2,y=-2,mass=2)
third_child = Qnode$new(x=-2, y=4, mass=3)

childs = c(first_child, second_child, third_child)

# load childs
qnode = Qnode$new(initial_list=childs)


#qnode2 = Qnode$new(quadrant_size=80, first_child=list(Qnode$new(x=1,y=2,mass=2), second_child=qnode))

# physics test
qnode$compute_mass_distribution()
qnode$compute_forces()
qnode$compute_accelerations()
qnode$update_state()

l = qnode$to_list()
print(l); 
print(length(l))

