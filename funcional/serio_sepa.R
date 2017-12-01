source('funcional/definitions.R')

# Creates a hard-coded quadtree for tests
root <- new_qnode(new_particle(new_point(10, 10), 20, new_point(0, 0), new_point(0, 0), 1024))
root[[2]] <- new_qnode(new_particle(new_point(3, 3), 1, new_point(0, 0), new_point(0, 0), 0))
root[[3]] <- new_qnode(new_particle(new_point(1, 1), 2, new_point(0, 0), new_point(0, 0), 0))
root[[4]] <- new_qnode(new_particle(new_point(2, 2), 3, new_point(0, 0), new_point(0, 0), 0))
root[[5]] <- new_qnode(new_particle(new_point(20, 20), 20, new_point(0, 0), new_point(0, 0), 256))
root[[5]][[2]] <- new_qnode(new_particle(new_point(2, 2), 4, new_point(0, 0), new_point(0, 0), 0))
root[[5]][[3]] <- new_qnode(new_particle(new_point(1, 1), 5, new_point(0, 0), new_point(0, 0), 0))
root[[5]][[4]] <- new_qnode(new_particle(new_point(30, 30), 30, new_point(0, 0), new_point(0, 0), 64))
root[[5]][[4]][[3]] <- new_qnode(new_particle(new_point(1, 1), 6, new_point(0, 0), new_point(0, 0), 0))
root[[5]][[5]] <- new_qnode(new_particle(new_point(40, 40), 40, new_point(0, 0), new_point(0, 0), 64))
root[[5]][[5]][[2]] <- new_qnode(new_particle(new_point(3, 3), 8, new_point(0, 0), new_point(0, 0), 0))

# Verifies the number of particles in the tree
print("Number of particles: ")
print(qnode_nof_particles(root))


################ READ THIS ################
## The flow of this simulation is as follows:
## GROUPING: The quadtree is built with at maximum one particle per quadrant
## COMPUTATION: The new positions and velocities of the particles are calculated
## PLOTTING: The particles' positions are plotted/updated
################ READ THIS ################

## GROUPING :
list_toQnode(qnode_toList(root))

## COMPUTATION :
# defines the amount of time between each update
simulationStep <- 1 # 1 second between each update
# evaluates updatePositionAndVelocity for deltaT=simulationStep using Curry from library(functional)
updatePositionAndVelocityForSimulationStep <- Curry(updatePositionAndVelocity, deltaT=simulationStep)
# composes all functions of the COMPUTATION part into a single function, using Compose from library(functional)
computation <- Compose(computeMassDistribution, computeForces, updatePositionAndVelocityForSimulationStep, qnode_toList)

#computation(root)