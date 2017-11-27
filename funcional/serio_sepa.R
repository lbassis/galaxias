# point (2D) : data.frame
#   x : float, y : float
new_point <- function(x, y) data.frame(x=x, y=y)

p1 <- new_point(0.15, 3.4)
p2 <- new_point(2.2, -0.87)

# particle : data.frame
#   point : point,
#   PHYSICIS STUFF
new_particle <- function(point, mass, velocity, force, quadrantSize, ...) data.frame(point=point, mass=mass, velocity=velocity, force=force, quadrantSize=quadrantSize)

# hm eh de boas aninhar data.frame, desde que a dimensionalidade do valor de cada coluna ("chave") 
#   do data.frame tenha a mesma dimensionalidade
#   tipo se um dos elementos for uma lista, da pra encapsular os outros numa lista unitaria
#   mesma coisa pra estruturas de ordem maior tipo matriz
#   no caso arvore tem uma ordem indefinida entao nao da pra usar data.frame

p1 <- new_particle(p1, 1, new_point(0, 0), new_point(0, 0), 0)
p2 <- new_particle(p2, 2, new_point(0, 0), new_point(0, 0), 0)

samples <- list(p1, p2)
#print(samples)

# quad_node : list
#   [[1]] : point,
#   [[2..4]] : quad_node
new_qnode <- function(particle) 
  list(particle, list(), list(), list(), list())
qnode_empty <- function(node) length(node) == 0
qnode_childs <- function(node) if (!qnode_empty(node)) tail(node, length(node)-1)
qnode_degree <- function(node) Reduce("+", lapply(qnode_childs(node), function(c) as.integer(!qnode_empty(c))))

qnode_insert <- function(node, particle) {
  if (qnode_empty(node)) data.frame(
    origin = node$origin, shape = node$shape,
    particle = particle
  )
}

root_origin = new_point(-5, -5)
root_shape = new_point(10, 10)
qroot = new_qnode(root_origin)
#print(qroot)
#print(qnode_childs(qroot))
#print(length(qroot))

#print(qnode_degree(qroot))
#print(qnode_empty(qroot))


root <- new_qnode(new_particle(new_point(10, 10), 20, new_point(0, 0), new_point(0, 0), 1024))
root[[2]] <- new_particle(new_point(3, 3), 1, new_point(0, 0), new_point(0, 0), 0)
root[[3]] <- new_particle(new_point(1, 1), 2, new_point(0, 0), new_point(0, 0), 0)
root[[4]] <- new_particle(new_point(2, 2), 3, new_point(0, 0), new_point(0, 0), 0)
root[[5]] <- new_qnode(new_particle(new_point(20, 20), 20, new_point(0, 0), new_point(0, 0), 256))
root[[5]][[2]] <- new_particle(new_point(2, 2), 4, new_point(0, 0), new_point(0, 0), 0)
root[[5]][[3]] <- new_particle(new_point(1, 1), 5, new_point(0, 0), new_point(0, 0), 0)
root[[5]][[4]] <- new_qnode(new_particle(new_point(30, 30), 30, new_point(0, 0), new_point(0, 0), 64))
root[[5]][[4]][[3]] <- new_particle(new_point(1, 1), 6, new_point(0, 0), new_point(0, 0), 0)
root[[5]][[5]] <- new_qnode(new_particle(new_point(40, 40), 40, new_point(0, 0), new_point(0, 0), 64))
root[[5]][[5]][[2]] <- new_particle(new_point(3, 3), 8, new_point(0, 0), new_point(0, 0), 0)

# recursively counts the number of particles in a quadrant
qnode_nof_particles <- function(node) sum(unlist(lapply(qnode_childs(node), function(c) {
  if (length(c) == 0) 0
  if(is.data.frame(c)) 1 else qnode_nof_particles(c)
})))

print("Number of particles: ")
print(qnode_nof_particles(root))

qnode_data <- function(data){
  function(node) {
    if (is.data.frame(node))
      node[data][[1]]
    else if(!qnode_empty(node))
      node[[1]][data][[1]]
    else
      0
  }
}

qnode_x <- qnode_data("point.x")
qnode_y <- qnode_data("point.y")
qnode_mass <- qnode_data("mass")
qnode_vx <- qnode_data("velocity.x")
qnode_vy <- qnode_data("velocity_y")
qnode_fx <- qnode_data("force.x")
qnode_fy <- qnode_data("force.y")
qnode_size <- qnode_data("quadrantSize")

qnode_centerOfMass <- function(node){
  node_list = list(node[[2]], node[[3]], node[[4]], node[[5]])
  x_list <- lapply(node_list, qnode_x)
  y_list <- lapply(node_list, qnode_y)
  mass_list <- unlist(lapply(node_list, qnode_mass))
  x_mass <- list(unlist(x_list)*unlist(mass_list))
  y_mass <- list(unlist(y_list)*unlist(mass_list))
  mass <- sum(unlist(lapply(mass_list, function(x) if(length(x) > 0) x else 0)))
  if (mass == 0) {
    new_particle(new_point(0,0),0, new_point(0, 0), new_point(0, 0), 0)
  } else {
    x <- sum(unlist(lapply(x_mass, function(x) if(length(x) > 0) x else 0)))/mass
    y <- sum(unlist(lapply(y_mass, function(x) if(length(x) > 0) x else 0)))/mass
    new_particle(new_point(x,y), mass, new_point(0, 0), new_point(0, 0), qnode_size(node))
  }
}

computeMassDistribution <- function(node) {
  if (is.data.frame(node) || qnode_empty(node))
    node
  else {
    newNode <- node
    newNode[[2]] <- computeMassDistribution(node[[2]])
    newNode[[3]] <- computeMassDistribution(node[[3]])
    newNode[[4]] <- computeMassDistribution(node[[4]])
    newNode[[5]] <- computeMassDistribution(node[[5]])
    if (qnode_nof_particles(node) == 0) {
      particle <- new_particle(new_point(0, 0), 0, new_point(0, 0), new_point(0, 0), qnode_size(node))
      newNode <- list(particle, node[[2]], node[[3]], node[[4]], node[[5]])
    } else {
      particle <- qnode_centerOfMass(newNode)
      newNode <- list(particle, newNode[[2]], newNode[[3]], newNode[[4]], newNode[[5]])
    }
    newNode
  }
}

#print(computeMassDistribution(root))

distance <- function(node1, node2) ((qnode_x(node1) - qnode_x(node2))^2 + (qnode_y(node1) - qnode_y(node2))^2)^(1/2)
distance_x <- function(node1, node2) (qnode_x(node1) - qnode_x(node2))
distance_y <- function(node1, node2) (qnode_y(node1) - qnode_y(node2))

# For each particle, calculates the resultant force applied to it by the other particles
computeForces <- function(root) {
  computeSingleForce <- function(node, particle) {
    #G <- 6.67408*(10^(-11))
    G <- 1
    fx <- (G*qnode_mass(node)*qnode_mass(particle))/(distance_x(node, particle)^2)
    if (qnode_x(node) > qnode_x(particle))
      fx <- fx*(-1)
    fy <- (G*qnode_mass(node)*qnode_mass(particle))/(distance_y(node, particle)^2)
    if (qnode_y(node) > qnode_y(particle))
      fy <- fy*(-1)
    new_point(fx, fy)
  }
  # Calculates the resultant force for particle "node"
  computeResultantForce <- function(node, particle) {
    if (qnode_empty(node)) {
      new_point(0, 0)
    } else if (qnode_nof_particles(node) == 0) {
      new_point(0, 0)
    } else if (is.data.frame(node)) {
      computeSingleForce(node, particle)
    } else {
      r <- distance(node, particle)
      d <- qnode_size(node)
      theta <- 1
      if (d/r < theta) {
        return(computeSingleForce(node, particle))
      } else {
        f1 <- computeResultantForce(node[[2]], particle)
        f2 <- computeResultantForce(node[[3]], particle)
        f3 <- computeResultantForce(node[[4]], particle)
        f4 <- computeResultantForce(node[[5]], particle)
        fx_list <- list(f1$x, f2$x, f3$x, f4$x)
        fy_list <- list(f1$y, f2$y, f3$y, f4$y)
        fx <- sum(unlist(fx_list))
        fy <- sum(unlist(fy_list))
        new_point(fx, fy)
      }
    }
  }
  # Loops through the particles calling computeForce
  computeForces_r <- function(root, node) {
    if (qnode_empty(node)) {
      node
    } else if (is.data.frame(node)) {
      force <- computeResultantForce(root, node)
      new_particle(new_point(qnode_x(node), qnode_y(node)), qnode_mass(node), new_point(0, 0), force, qnode_size(node))
    } else {
      newNode <- node
      newNode[[2]] <- computeForces_r(root, node[[2]])
      newNode[[3]] <- computeForces_r(root, node[[3]])
      newNode[[4]] <- computeForces_r(root, node[[4]])
      newNode[[5]] <- computeForces_r(root, node[[5]])
      newNode
    }
  }
  computeForces_r(root, root)
}

print(computeForces(root))
