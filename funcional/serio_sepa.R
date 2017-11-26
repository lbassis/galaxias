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
qnode_childs <- function(node) tail(node, length(node)-1)
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
root[[2]] <- new_particle(new_point(0, 0), 1)
root[[3]] <- new_particle(new_point(1, 1), 2)
root[[4]] <- new_particle(new_point(2, 2), 3)
root[[5]] <- new_qnode(new_particle(new_point(20, 20), 20))
root[[5]][[2]] <- new_particle(new_point(0, 0), 4)
root[[5]][[3]] <- new_particle(new_point(1, 1), 5)
root[[5]][[4]] <- new_qnode(new_particle(new_point(30, 30), 30))
root[[5]][[4]][[2]] <- new_particle(new_point(0, 0), 6)

# recursively counts the number of particles in a quadrant
qnode_nof_particles <- function(node) sum(unlist(lapply(qnode_childs(node), function(c) {
  if (length(node) == 0) 0
  if(is.data.frame(c)) 1 else qnode_nof_particles(c)
})))

print(qnode_nof_particles(root))
