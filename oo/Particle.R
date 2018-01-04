source("Quad.R")
library(data.table)

Particle <- setRefClass("Particle",
  contains = "Point",
  fields = list(
    mass = "numeric",
    velocity = "Point",
    force = "Point",
    acceleration = "Point",
    quadrant_size = "numeric"
  ),
  methods = list(
    initialize = function(.Object,mass=0,velocity=Point$new(),force=Point$new(),acceleration=Point$new(),quadrant_size=0,...) {
      .self$mass = mass
      .self$velocity = velocity$copy()
      .self$force = force$copy()
      .self$acceleration = acceleration$copy()
      .self$quadrant_size = quadrant_size
      callSuper(...)
    },
    get_mass = function() .self$mass,
    get_velocity = function() .self$velocity,
    get_force = function() .self$force,
    get_acceleration = function() .self$acceleration,
    get_quadrant_size = function() .self$quadrant_size,
    get_vx = function() .self$get_velocity()$get_x(),
    get_vy = function() .self$get_velocity()$get_y(),
    get_fx = function() .self$get_force()$get_x(),
    get_fy = function() .self$get_force()$get_y(),
    get_ax = function() .self$get_acceleration()$get_x(),
    get_ay = function() .self$get_acceleration()$get_y(),
    get_quadrant_size = function() .self$quadrant_size,
    to_point = function() {
      Point(x = .self$get_x(), y = .self$get_y())
    },
    set_mass = function(mass) .self$mass = mass,
    set_velocity = function(velocity) .self$velocity = velocity$copy(),
    set_force = function(force) .self$force = force$copy(),
    set_acceleration = function(acceleration) .self$acceleration = acceleration$copy(),
    set_quadrant_size = function(quadrant_size) .self$quadrant_size = quadrant_size
  )
      
)
