source('funcional/definitions.R')

p1 <- new_point(0.15, 3.4)
p2 <- new_point(2.2, -0.87)


p1 <- new_particle(p1, 1, new_point(0, 0), new_point(0, 0), 0)
p2 <- new_particle(p2, 2, new_point(0, 0), new_point(0, 0), 0)

samples <- list(p1, p2)
#print(samples)



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



print("Number of particles: ")
print(qnode_nof_particles(root))


qnode_x <- qnode_data("point.x")
qnode_y <- qnode_data("point.y")
qnode_mass <- qnode_data("mass")
qnode_vx <- qnode_data("velocity.x")
qnode_vy <- qnode_data("velocity.y")
qnode_fx <- qnode_data("force.x")
qnode_fy <- qnode_data("force.y")
qnode_size <- qnode_data("quadrantSize")



#print(computeMassDistribution(root))



#root <- new_qnode(new_particle(new_point(10, 10), 20, new_point(0, 0), new_point(0, 0), 1024))
#root[[2]] <- new_particle(new_point(1, 2), 2, new_point(0, 0), new_point(0, 0), 0)
#root[[5]] <- new_qnode(new_particle(new_point(5, 5.5), 20, new_point(0, 0), new_point(0, 0), 256))
#root[[5]][[2]] <- new_particle(new_point(5, 6), 4, new_point(0, 0), new_point(0, 0), 0)
#root[[5]][[3]] <- new_particle(new_point(5, 5), 5, new_point(0, 0), new_point(0, 0), 0)
md <- computeMassDistribution(root)
print(computeForces(md))
