library(grid)
library(ggplot2)

par(mfrow=c(2, 1), mar=c(1,1,1,1))
plot(1,1)

plot.new()

vps <- baseViewports()
pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
vp1 <-plotViewport(c(1,1,1,1)) ## create new vp with margins, you play with this values 

p <- ggplot(data.frame(x=1,y=1)) + geom_point() + aes(x,y)

print(p,vp = vp1)        ## suggested by @bpatiste

#########
##########################
#########

par(mfrow=c(2, 1), mar=c(1,1,1,1))

plot.new()

vps <- baseViewports()
pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
vp1 <-plotViewport(c(1,1,1,1)) ## create new vp with margins, you play with this values 

p <- ggplot(data.frame(x=1,y=1)) + geom_point() + aes(x,y)

print(p,vp = vp1)        ## suggested by @bpatiste

plot(1,1)
