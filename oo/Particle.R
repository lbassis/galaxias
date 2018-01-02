source("Quad.R")

Particle <- setRefClass("Particle",
  contains = "Point",
  fields = list(
    mass = "numeric",
    velocity = "Point",
    force = "Point",
    quadrant_size = "Point"
  ),
  methods = list(
    get_mass = function() .self$mass,
    get_velocity = function() .self$velocity,
    get_force = function() .self$force,
    get_quadrant_size = function() .self$quadrant_size,
    get_vx = function() .self$get_velocity()$get_x(),
    get_vy = function() .self$get_velocity()$get_y(),
    get_fx = function() .self$get_force()$get_x(),
    get_fy = function() .self$get_force()$get_y(),
    particle_to_point = function() {
      Point(x = .self$get_point()$get_x(), y = .self$get_point()$get_y())
    },
    particle_set_qsize = function(qsize) {
      Particle(point = particle_to_point(), mass = .self$get_mass(), 
                   velocity = Point(x = .self$get_vx, y = .self$get_vy()), 
                   force = Point(x = .self$get_fx(), y = .self$get_fy()),
                   quadrant_size = qsize[[1]])
    }
  )
      
)