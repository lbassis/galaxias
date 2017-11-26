# point (2D) : data.frame
#   x : float, y : float
new_point <- function(x, y) data.frame(x=x, y=y)

p1 <- new_point(0.15, 3.4)
p2 <- new_point(2.2, -0.87)

# particle : data.frame
#   point : point,
#   PHYSICIS STUFF
new_particle <- function(point, mass, ...) data.frame(point=point, mass=mass)

# hm eh de boas aninhar data.frame, desde que a dimensionalidade do valor de cada coluna ("chave") 
#   do data.frame tenha a mesma dimensionalidade
#   tipo se um dos elementos for uma lista, da pra encapsular os outros numa lista unitaria
#   mesma coisa pra estruturas de ordem maior tipo matriz
#   no caso arvore tem uma ordem indefinida entao nao da pra usar data.frame

p1 <- new_particle(p1, 1)
p2 <- new_particle(p2, 2)

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


root <- new_qnode(new_particle(new_point(10, 10), 20))
root[[2]] <- new_particle(new_point(3, 3), 1)
root[[3]] <- new_particle(new_point(1, 1), 2)
root[[4]] <- new_particle(new_point(2, 2), 3)
root[[5]] <- new_qnode(new_particle(new_point(20, 20), 20))
root[[5]][[2]] <- new_particle(new_point(2, 2), 4)
root[[5]][[3]] <- new_particle(new_point(1, 1), 5)
root[[5]][[4]] <- new_qnode(new_particle(new_point(30, 30), 30))
root[[5]][[4]][[3]] <- new_particle(new_point(1, 1), 6)
root[[5]][[5]] <- new_qnode(new_particle(new_point(40, 40), 40))
root[[5]][[5]][[2]] <- new_particle(new_point(3, 3), 8)

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

qnode_centerOfMass <- function(node){
  node_list = list(node[[2]], node[[3]], node[[4]], node[[5]])
  x_list <- lapply(node_list, qnode_x)
  y_list <- lapply(node_list, qnode_y)
  mass_list <- unlist(lapply(node_list, qnode_mass))
  x_mass <- list(unlist(x_list)*unlist(mass_list))
  y_mass <- list(unlist(y_list)*unlist(mass_list))
  mass <- sum(unlist(lapply(mass_list, function(x) if(length(x) > 0) x else 0)))
  x <- sum(unlist(lapply(x_mass, function(x) if(length(x) > 0) x else 0)))/mass
  y <- sum(unlist(lapply(y_mass, function(x) if(length(x) > 0) x else 0)))/mass
  new_particle(new_point(x,y), mass)
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
      particle <- new_particle(new_point(0, 0), 0)
      newNode <- list(particle, node[[2]], node[[3]], node[[4]], node[[5]])
    } else {
      particle <- qnode_centerOfMass(newNode)
      newNode <- list(particle, newNode[[2]], newNode[[3]], newNode[[4]], newNode[[5]])
    }
    newNode
  }
}

print(computeMassDistribution(root))