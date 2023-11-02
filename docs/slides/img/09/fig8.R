library(tikzDevice)
tikz("fig8.tex",standAlone=T,width=5, height=5)
library(rrcov)
data(hemophilia)
par(pty="s")
plot(AHFantigen~AHFactivity,data=hemophilia, 
	pch=c(rep(1,30),rep(19,45)),col=c(rep(6,30),rep(4,45)),
	cex=0.75,xlab="$Y_1$",
	ylab="$Y_2$",
	ylim=c(-0.4,0.6),xlim=c(-0.7,0.4),bty="n")

#drawMahal(hemophilia[1:30,1:2], center=means[[2]], covariance=var(hemophilia[,1:2]),quantile=0.5, pch=1, col=6, add=T)


#lda(gr~AHFantigen+AHFactivity, data=hemophilia)

#Cast to factor, find the group mean and plot them
hemophilia$gr <- as.factor(hemophilia$gr)
means <- by(data=hemophilia[,1:2], hemophilia[,"gr"], colMeans)
points(means[[1]][1],means[[1]][2],pch=4,col=4,lwd=2)
points(means[[2]][1],means[[2]][2],pch=4,col=6)
library(DiscriMiner)
B <- betweenCov(variables = hemophilia[,1:2], group = as.factor(hemophilia[,3]))
W <- withinCov(variables = hemophilia[,1:2], group = as.factor(hemophilia[,3]))
ev <- eigen(solve(W) %*% B)$vectors
proj <- as.matrix(hemophilia[,1:2]) %*% (ev[,1] %*% t(ev[,1]))
proj[,1] <-proj[,1]+0.3
proj[,2] <-proj[,2]+0.3
points(proj, col=c(rep(6,30),rep(4,45)), cex=0.5,pch=20)
Spi <- solve(var(hemophilia[,1:2]))
LD <- MASS::lda(hemophilia[,3]~hemophilia[,1]+hemophilia[,2])
line <- as.vector(t(LD[[3]][1,]-LD[[3]][2,])%*% Spi)
center <- (means[[1]]+means[[2]])/2 #Mean of average, not global mean
slope <- -line[1]/line[2]
intercept <- 0.5*t(means[[1]]-means[[2]])%*%Spi %*% (means[[1]]+means[[2]])
	#center[2]-slope*center[1]
#abline(b=slope, a=intercept/line[2])
segments(x0=0.1703102, y0=slope*0.1703102+intercept/line[2], y1=slope*-0.55+intercept/line[2],x1=-0.55)
# abline(b=-line[1]/line[2], a=center[2]-line[1]/line[2]*center[1],col=4)
# abline(b=-line[1]/line[2], a=-intercept/line[2],col=4)
# abline(b=-line[2]/line[1], a=center[2]+line[2]/line[1]*center[1])
# abline(v=0)
# abline(h=0)
points(center[1],center[2],col=1, pch=3,cex=0.5,lwd=1.5)
#abline(b=-slope, a=0.42,lty=2)
el1 <- ellipse(var(hemophilia[,1:2]), centre=means[[2]])#,t=(5*26)/(25*30)*qf(0.95,5,25))
lines(el1,lwd=1.5,col=6)
el1 <- ellipse(var(hemophilia[,1:2]), centre=means[[1]])#,t=(2*29)/(28*30)*qf(0.95,2,28))
lines(el1,lwd=1.5,col=4)

dev.off()
