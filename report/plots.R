library(ggplot2)

observed <- read.table("dat.txt")

ggplot(data.frame(x=c(0,25)), aes(x)) +
  stat_function(fun=function(x)x*(x+1), geom="line", aes(colour="f(x) = x*(x+1)"))


ggplot(observed, aes(V1, V2)) + 
  geom_point(aes(colour = "Observado em Barnes-Hut")) +
  stat_function(fun=function(x)x*(x+1), geom="line", aes(colour="f(x) = x*(x+1)")) +
  xlab("Particulas") +
  ylab("Numero de comparacoes") +
  theme(legend.title=element_blank())

