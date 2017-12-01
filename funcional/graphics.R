library(plotrix)
source("funcional/definitions.R")

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

p1 <- new_particle(new_point(0.5, 0.5), 0.01, new_point(0.0, 0), new_point(0, 0), 0)
p2 <- new_particle(new_point(0.3, 0.2), 0.03, new_point(0.0, -0.0), new_point(0, 0), 0)
p3 <- new_particle(new_point(0.1, 0.9), 0.05, new_point(-0.0, -0.0), new_point(0, 0), 0)
p4 <- new_particle(new_point(0.0, 0.0), 0.03, new_point(-0.0, -0.0), new_point(0, 0), 0)
p5 <- new_particle(new_point(0.7, 0.7), 0.07, new_point(-0.0, -0.0), new_point(0, 0), 0)
p6 <- new_particle(new_point(0.4, 0.9), 0.02, new_point(-0.0, -0.0), new_point(0, 0), 0)

n1 <- list(p1, list(), list(), list(), list())
n2 <- list(p2, list(), list(), list(), list())
n3 <- list(p3, list(), list(), list(), list())
n4 <- list(p4, list(), list(), list(), list())
n5 <- list(p5, list(), list(), list(), list())
n6 <- list(p6, list(), list(), list(), list())


particles <- list(n2, n3, n4, n5, n6)


drawing_loop <- function(particles, iteractions, name, updatePosAndVel) {
  
  if (iteractions == 0) {
    system("convert -delay 10 *.jpg result.gif")
    file.remove(list.files(pattern=".jpg"))
  }
  
  else {
    name <- paste(name, "a")
    filename <- paste(name, ".jpg")
    png(filename = filename)
    plot.new() # [0, 1]??
    frame()
    draw_particles(normalize_masses(particles))
    dev.off()
    #print(particles)
    #print(qList_toParticles(particles))
    particles <- updatePosAndVel(qList_toParticles(particles))
    #print(particles)
    drawing_loop(particles, iteractions-1, name, updatePosAndVel)
  }
}

draw <- function(particles, iteractions, updatePosAndVel) {
  #setwd("/projects/galaxias/gifs")
  invisible(drawing_loop(particles, iteractions, "a", updatePosAndVel))
}

greatest_mass <- function(particles, current_greatest) {
  
  if (length(particles) > 0) {
    t <- tail(particles, length(particles)-1)
    current_mass <- qnode_mass(particles[[1]])
    #print(current_mass)
    if (current_mass > current_greatest)
      return (greatest_mass(t, current_mass))
    else
      return (greatest_mass(t, current_greatest))
  }
  
  else
    return (current_greatest)
  
}

normalize_mass <- function(node, greatest) {
  p <- new_point(qnode_x(node), qnode_y(node))
  v <- new_point(qnode_vx(node), qnode_vy(node))
  f <- new_point(qnode_fx(node), qnode_fy(node))
  new_p <- new_particle(p, qnode_mass(node)/(10*greatest), v, f, qnode_size(node)) 
  new_n <- list(new_p, list(), list(), list(), list()) # tenho que ver como pega as listas!!!
  return(list(new_n))
}

wrapped_normalization <- function(particle) 
  return (normalize_mass(particle, greatest_mass(particles, 0)))

normalize_masses <- function(particles) {
  
  if (length(particles) > 0)
    return (c(wrapped_normalization(particles[[1]]), normalize_masses(tail(particles, length(particles)-1))))
  else
    return (list())
}

#particles <- normalize_masses(particles)
draw(particles, 100, groupingAndComputation)