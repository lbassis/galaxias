new_point <- setRefClass("new_point",
                          fields = list(x = "numeric", y = "numeric"))

new_quad <- setRefClass("new_quad",
                        fields = list(top_left = "new_point", size = "new_point"))

new_particle <- setRefClass("new_particle", 
                            fields = list(point = "new_point", mass = "numeric", velocity = "new_point",
                            force = "new_point", quadrant_size = "new_point"))

new_qnode <- setRefClass("new_qnode",
                         fields = list(particle = "new_particle", first_child = "list", second_child = "list",
                                       third_child = "list", fourth_child = "list"))


# testando uns objetos aqui
x_point = new_point(x = 1, y = 2)
x_quad = new_quad(top_left = x_point, size = x_point)
x_particle = new_particle(point = x_point, mass = 10, velocity = x_point, force = x_point, quadrant_size = x_point)
x_qnode = new_qnode(particle = x_particle, first_child = list(x_particle), second_child = list(), third_child = list(), fourth_child = list())