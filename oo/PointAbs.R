setRefClass(
  Class="PointAbs",
  contains="VIRTUAL",
  methods = list(
    get_max = function() {},
    
    # returns new
    pointwise_op_bin = function(p, op) {  },
    pointwise_op_un = function(op) { },
    
    # supondo que quad_center é um ponto tb
    quadrant_i = function(quad_center) {},
    subquadrant = function(quad) { }
  )
  
)