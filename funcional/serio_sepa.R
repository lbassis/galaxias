source('funcional/definitions.R')

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

#print(computeMassDistribution(root))

#root <- new_qnode(new_particle(new_point(10, 10), 20, new_point(0, 0), new_point(0, 0), 1024))
#root[[2]] <- new_particle(new_point(1, 1), 2, new_point(0, 0), new_point(0, 0), 0)
#root[[5]] <- new_qnode(new_particle(new_point(5, 5.5), 20, new_point(0, 0), new_point(0, 0), 256))
#root[[5]][[2]] <- new_particle(new_point(1, 3), 4, new_point(0, 0), new_point(0, 0), 0)
#root[[5]][[3]] <- new_particle(new_point(5, 5), 5, new_point(0, 0), new_point(0, 0), 0)
md <- computeMassDistribution(root)
cf <- computeForces(md)
upv <- updatePositionAndVelocity(cf, 1)
print(upv)

print("List of particles: ")

print(qnode_toList(upv))