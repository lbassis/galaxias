library(functional)


# point (2D) : data.frame
#   x : float, y : float
new_point <- function(x, y) data.frame(x=x, y=y)

# particle : data.frame
#   point : point,
#   PHYSICIS STUFF
new_particle <- function(point, mass, velocity, force, quadrantSize, ...) data.frame(point=point, mass=mass, velocity=velocity, force=force, quadrantSize=quadrantSize)

# hm eh de boas aninhar data.frame, desde que a dimensionalidade do valor de cada coluna ("chave") 
#   do data.frame tenha a mesma dimensionalidade
#   tipo se um dos elementos for uma lista, da pra encapsular os outros numa lista unitaria
#   mesma coisa pra estruturas de ordem maior tipo matriz
#   no caso arvore tem uma ordem indefinida entao nao da pra usar data.frame

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

# recursively counts the number of particles in a quadrant
qnode_nof_particles <- function(node) sum(unlist(lapply(qnode_childs(node), function(c) {
  if (length(c) == 0) 0
  if(is.data.frame(c)) 1 else qnode_nof_particles(c)
})))

# gets the "data" column from node's data.frame
qnode_data <- function(data, node){
  if(is.data.frame(node)) # particle
    node[data][[1]]
  else if(!qnode_empty(node)) # non-empty qnode
    node[[1]][data][[1]]
  else # empty qnode
    0
}

# gets a pre-defined "data" column from node's data.frame
qnode_x <- Curry(qnode_data, data="point.x")
qnode_y <- Curry(qnode_data, data="point.y")
qnode_mass <- Curry(qnode_data, data="mass")
qnode_vx <- Curry(qnode_data, data="velocity.x")
qnode_vy <- Curry(qnode_data, data="velocity.y")
qnode_fx <- Curry(qnode_data, data="force.x")
qnode_fy <- Curry(qnode_data, data="force.y")
qnode_size <- Curry(qnode_data, data="quadrantSize")

# calculates the node's center of mass and its position
qnode_centerOfMass <- function(node){
  node_list = list(node[[2]], node[[3]], node[[4]], node[[5]])
  x_list <- lapply(node_list, qnode_x)
  y_list <- lapply(node_list, qnode_y)
  mass_list <- unlist(lapply(node_list, qnode_mass))
  x_mass <- list(unlist(x_list)*unlist(mass_list))
  y_mass <- list(unlist(y_list)*unlist(mass_list))
  mass <- sum(unlist(lapply(mass_list, function(x) if(length(x) > 0) x else 0)))
  if (mass == 0) { # if the mass is equal to 0, avoid division by 0
    new_particle(new_point(0,0),0, new_point(0, 0), new_point(0, 0), 0)
  } else {
    x <- sum(unlist(lapply(x_mass, function(x) if(length(x) > 0) x else 0)))/mass
    y <- sum(unlist(lapply(y_mass, function(x) if(length(x) > 0) x else 0)))/mass
    new_particle(new_point(x,y), mass, new_point(qnode_vx(node), qnode_vy(node)), new_point(0, 0), qnode_size(node))
  }
}

# calculates the center of mass for all qnodes
computeMassDistribution <- function(node) {
  if (qnode_degree(node) == 0 || qnode_empty(node)) # if the node is already a particle or if it is empty, there's no need to do anything
    node
  else {
    newNode <- node
    newNode[[2]] <- computeMassDistribution(node[[2]])
    newNode[[3]] <- computeMassDistribution(node[[3]])
    newNode[[4]] <- computeMassDistribution(node[[4]])
    newNode[[5]] <- computeMassDistribution(node[[5]])
    particle <- qnode_centerOfMass(newNode)
    newNode <- list(particle, newNode[[2]], newNode[[3]], newNode[[4]], newNode[[5]])
    newNode
  }
}

# For each particle, calculates the resultant force applied to it by the other particles
computeForces <- function(root) {
  particle_setForce <- function(particle, force) {
    new_particle(new_point(qnode_x(particle), qnode_y(particle)), qnode_mass(particle), new_point(qnode_vx(particle), qnode_vy(particle)), force, qnode_size(particle))
  }
  computeSingleForce <- function(node, particle) {
    #G <- 6.67408*(10^(-11))
    G <- 1
    m1 <- qnode_mass(node)
    m2 <- qnode_mass(particle)
    d <- distance(node, particle)
    dx <- distance_x(node, particle)
    dy <- distance_y(node, particle)
    f <- (G*qnode_mass(node)*qnode_mass(particle))/(distance(node, particle)^2)
    fx <- f*dx/d
    if (qnode_x(node) == qnode_x(particle))
      fx <- 0
    fy <- f*dy/d
    if (qnode_y(node) == qnode_y(particle))
      fy <- 0
    new_point(fx, fy)
  }
  # Calculates the resultant force for particle "node"
  computeResultantForce <- function(node, particle) {
    if (qnode_empty(node)) {
      new_point(0, 0)
    } else if (qnode_degree(node) == 0) {
      computeSingleForce(node[[1]], particle)
    } else {
      r <- distance(node, particle)
      d <- qnode_size(node)
      theta <- 1
      if (d/r < theta) {
        computeSingleForce(node[[1]], particle)
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
    } else if (qnode_degree(node) == 0) {
      force <- computeResultantForce(root, node[[1]])
      particle <- particle_setForce(node[[1]], force)
      newNode <- list(particle, node[[2]], node[[3]], node[[4]], node[[5]])
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

distance <- function(node1, node2) ((qnode_x(node1) - qnode_x(node2))^2 + (qnode_y(node1) - qnode_y(node2))^2)^(1/2)
distance_x <- function(node1, node2) (qnode_x(node1) - qnode_x(node2))
distance_y <- function(node1, node2) (qnode_y(node1) - qnode_y(node2))

updatePositionAndVelocity <- function(node, deltaT) {
  particle_setPositionAndVelocity <- function (particle, point, velocity) {
    new_particle(point, qnode_mass(particle), velocity, new_point(qnode_fx(particle), qnode_fy(particle)), qnode_size(particle))
  }
  computeAcceleration <- function(node) {
    mass <- qnode_mass(node)
    if (mass == 0) {
      new_point(0, 0)
    } else {
      ax <- qnode_fx(node)/mass
      ay <- qnode_fy(node)/mass
      new_point(ax, ay)
    }
  }
  if (qnode_empty(node)){
    node
  } else if (qnode_degree(node) == 0) {
    a <- computeAcceleration(node)
    ax <- a$x
    ay <- a$y
    vx <- qnode_vx(node)
    vy <- qnode_vy(node)
    calcVelocityComponent <- function (v, a, t) (v + a*t)
    newVx <- calcVelocityComponent(vx, ax, deltaT)
    newVy <- calcVelocityComponent(vy, ay, deltaT)
    calcPositionComponent <- function (v, a, t, pos) (a*(t^2)/2 + v*t + pos)
    newX <- calcPositionComponent(vx, ax, deltaT, qnode_x(node))
    newY <- calcPositionComponent(vy, ay, deltaT, qnode_y(node))
    particle <- particle_setPositionAndVelocity(node, new_point(newX, newY), new_point(newVx, newVy))
    newNode <- list(particle, node[[2]], node[[3]], node[[4]], node[[5]])
  } else {
    newNode <- node
    newNode[[2]] <- updatePositionAndVelocity(node[[2]], deltaT)
    newNode[[3]] <- updatePositionAndVelocity(node[[3]], deltaT)
    newNode[[4]] <- updatePositionAndVelocity(node[[4]], deltaT)
    newNode[[5]] <- updatePositionAndVelocity(node[[5]], deltaT)
    newNode
  }
}

# extracts a list of particles from a quadtree
qnode_toList <- function (node) {
  if (qnode_empty(node)) {
    list()
  } else if (qnode_degree(node) == 0) {
    list(node)
  } else {
    newNode <- node
    newNode[[2]] <- qnode_toList(node[[2]])
    newNode[[3]] <- qnode_toList(node[[3]])
    newNode[[4]] <- qnode_toList(node[[4]])
    newNode[[5]] <- qnode_toList(node[[5]])
    c(newNode[[2]], newNode[[3]], newNode[[4]], newNode[[5]])
  }
}



