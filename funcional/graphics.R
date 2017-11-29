library(plotrix)
library(functional)
library(magrittr)

draw_particle <- function(node) {
  draw.circle(qnode_x(node), qnode_y(node), qnode_mass(node), col="red", nv=1000, border = NA,lty=1,lwd=1)
}

draw_particles <- function(particles) {
  invisible(lapply(particles, draw_particle))
}

erase_particle <- function(node) {
  draw.circle(qnode_x(node), qnode_y(node), qnode_mass(node), col="white", nv=1000, border = NA,lty=1,lwd=1)
}

erase_particles <- function(particles) {
  invisible(lapply(particles, erase_particle))
}

update_position <- function(node) {
  new_x = qnode_x(node) + qnode_vx(node)
  new_y = qnode_y(node) + qnode_vy(node)
  p <- new_point(qnode_x(node) + qnode_vx(node), qnode_y(node) + qnode_vy(node))
  v <- new_point(qnode_vx(node), qnode_vy(node))
  f <- new_point(qnode_fx(node), qnode_fy(node))
  new_p <- new_particle(p, qnode_mass(node), v, f, qnode_size(node)) 
  new_n <- list(new_p, list(), list(), list(), list()) # tenho que ver como pega as listas!!!
  return(new_n)
}


update_positions <- function(nodes) {
  return(invisible(lapply(nodes, update_position)))
}

p1 <- new_particle(new_point(0.5, 0.5), 0.01, new_point(0.01, 0), new_point(0, 0), 0)
print(p1)
p2 <- new_particle(new_point(0.3, 0.2), 0.03, new_point(0.01, -0.01), new_point(0, 0), 0)
n1 <- list(p1, list(), list(), list(), list())
n2 <- list(p2, list(), list(), list(), list())
particles <- c(n1, n2)

plot.new() # [0, 1]²
frame()

drawing_loop <- function(particles) {
  draw_particles(particles)
  Sys.sleep(0.1)
  erase_particles(particles)
  drawing_loop(update_positions(particles))
}
  
drawing_loop(particles)