source('funcional/definitions.R')

p1 <- new_particle(new_point(10, 10), 20, new_point(0, 0), new_point(0, 0), 1024)
p2 <- new_particle(new_point(3, 3), 1, new_point(0, 0), new_point(0, 0), 0)
p3 <- new_particle(new_point(1, 1), 2, new_point(0, 0), new_point(0, 0), 0)
p4 <- new_particle(new_point(2, 2), 3, new_point(0, 0), new_point(0, 0), 0)
p5 <- new_particle(new_point(20, 20), 20, new_point(0, 0), new_point(0, 0), 256)

root <- new_qnode(p1)
root[[2]] <- new_qnode(p2)
root[[3]] <- new_qnode(p3)
root[[4]] <- new_qnode(p4)
root[[5]] <- new_qnode(p5)

print(root)
p_list = qnode_toList(root)
quade = quad_from_input(p_list)
#print(quade)

#r = list()
#r = qnode_insert(r, quad_from_input(p_list), p1)
#print(r)
#r = qnode_insert(r, quade, p2)