source("Qnode.R")

# testando uns objetos aqui
x_point = Point(x = 0.3, y = 0.4)
x_quad = Quad(top_left = x_point, size = 10)
x_particle = Particle(x = x_point$x, y = x_point$y, mass = 0.2, velocity = x_point, force = x_point, quadrant_size = x_point)
x_qnode = Qnode(particle = x_particle, first_child = list(x_particle), second_child = list(), third_child = list(), fourth_child = list())
plot.new()
x_qnode$draw()