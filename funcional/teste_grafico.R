#require(grDevices)
library(plotrix)

plot(1:5,seq(1,10,length=5),bty="n", type = "n", xlab = "", ylab = "", yaxt = "n", xaxt = "n", pch='.')
i <- 0
while (i < 5) {
  draw.circle(2.5-i,8-i,0.1,col="red", nv=10, border = NA)
  draw.circle(2.5+i,8+i,0.1,col="red", nv=10, border = NA)
  
  Sys.sleep(0.2)
  draw.circle(2.5-i,8-i,0.1,col="white", nv=10, border = NA)
  draw.circle(2.5+i,8+i,0.1,col="white", nv=10, border = NA)
  
  i <- i+0.015
}