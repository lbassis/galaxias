source("new_quad.R")

new_particle <- setRefClass("new_particle", 
                            fields = list(point = "new_point", mass = "numeric", velocity = "new_point",
                                          force = "new_point", quadrant_size = "new_point"))

new_particle$methods(get_point = function() return(.self$point))
new_particle$methods(get_mass = function() return(.self$mass))
new_particle$methods(get_velocity = function() return (.self$velocity))
new_particle$methods(get_force = function() return (.self$force))
new_particle$methods(get_quadrant_size = function() return (.self$quadrant_size))

new_particle$methods(get_x = function() return(.self$get_point()$get_x()))
new_particle$methods(get_y = function() return(.self$get_point()$get_y()))
new_particle$methods(get_vx = function() return(.self$get_velocity()$get_x()))
new_particle$methods(get_vy = function() return(.self$get_velocity()$get_y()))
new_particle$methods(get_fx = function() return(.self$get_force()$get_x()))
new_particle$methods(get_fy = function() return(.self$get_force()$get_y()))



new_particle$methods(particle_to_point = function() {
  new_point(x = .self$get_point()$get_x(), y = .self$get_point()$get_y())
})

new_particle$methods(particle_set_qsize = function(qsize) {
  new_particle(point = particle_to_point(), mass = .self$get_mass(), 
               velocity = new_point(x = .self$get_vx, y = .self$get_vy()), 
               force = new_point(x = .self$get_fx(), y = .self$get_fy()),
               quadrant_size = qsize[[1]])
})
