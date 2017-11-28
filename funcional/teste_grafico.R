#require(grDevices)
library(plotrix)



new_point <- function(x, y) data.frame(x=x, y=y)
new_particle <- function(point, mass, velocity, force, quadrantSize, ...)  data.frame(point=point, mass=mass, velocity=velocity, force=force, quadrantSize=quadrantSize)


draw_particle <- function(node) {
  #print(particle)
  draw.circle(qnode_x(node), qnode_y(node), qnode_mass(node), col="red", nv=1000, border = NA,lty=1,lwd=1)
}


draw_particles <- function(particles) {
  invisible(lapply(particles, draw_particle))
  #if (length(particles) > 1) {
   # head <- head(particles, 1)
    #tail <- tail(particles, length(particles)-1)
    #draw_particle(head)
    #draw_particles(tail)
  #}
}

update_position <- function(node) {
  new_x <- qnode_x(node) + qnode_vx(node)
  new_y <- qnode_y(node) + qnode_vy(node)
  v <- new_point(qnode_vx(node), qnode_vy(node))
  f <- new_point(qnode_fx(node), qnode_fy(node))
  new_p <- new_particle(new_x, new_y, qnode_mass(node), v, f, qnode_data(node)) 
  new_n <- list(new_p, list(), list(), list()) # isso eu nao sei se ta certo!!!!

  return(new_n)
}

library('magrittr')

update_positions <- function(nodes) {
  
  if (length(nodes) > 0) {
    head <- head(nodes, 1)
    print(head)
    tail <- tail(nodes, length(nodes)-1)
    cabecao <- update_position(head)
    print(cabecao)
    update_positions(tail)
    #c(update_position(head), update_positions(tail))
  }

  
  
  
  #return(invisible(lapply(nodes, update_position)))
}


p1 <- new_particle(new_point(0.5, 0.5), 0.01, new_point(0.1, 0), new_point(0, 0), 0)
p2 <- new_particle(new_point(0.3, 0.2), 0.03, new_point(0.2, 0), new_point(0, 0), 0)
n1 <- list(p1, list(), list(), list(), list())
n2 <- list(p2, list(), list(), list(), list())
particles <- c(n1, n2)

plot.new() # [0, 1]²
frame()
#draw_particles(update_positions(particles))
#draw_particles(update_positions(draw_particles(particles)))# %>% update_positions %>% draw_particles

update_position(particles)