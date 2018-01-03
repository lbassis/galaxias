source("Quad.R")
library(data.table)

Particle <- setRefClass("Particle",
  contains = "Point",
  fields = list(
    mass = "numeric",
    velocity = "Point",
    force = "Point",
    quadrant_size = "numeric"
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
    to_point = function() {
      Point(x = .self$get_x(), y = .self$get_y())
    },
    set_mass = function(mass) .self$mass = mass,
    set_velocity = function(velocity) .self$velocity = velocity,
    set_force = function(force) .self$force = force,
    set_quadrant_size = function(quadrant_size) .self$quadrant_size = quadrant_size
  )
      
)
