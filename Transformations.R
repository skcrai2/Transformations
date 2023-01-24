curve(poly2, col="red", lwd=2, from=-1, to=18)
points(ddpi, sr, type="p", lwd=3)

poly3 <- function(x) out3$coefficient[4]*x^3 + out3$coefficient[3]*x^2 + out3$coefficient[2]*x + out3$coefficient[1]
curve(poly3, col="red", lwd=2, from=-1, to=18)
points(ddpi, sr, type="p", lwd=3)



~~~~~~~~~~~~~~
  ######### Build heirachial models: typically should 
  ######### keep lower order terms.  
  
  
  modsavings <- data.frame(savings, mddpi=savings$ddpi-10)
summary(lm(sr ~ mddpi+I(mddpi^2), modsavings))

##### compare this output to that of the previous...  indicates 
##### to maybe get rid of the mddpi coef, right?  No!  Don't do it. 

##### Illustrate the fact that setting the linear term to zero means
##### the function is optimized at predictor = 0 using
##### 1. Excel program on Quadratic Regression; 2. taking derivative of 
##### a quadratic y = ax^2 + bx + c




~~~~~~~~~~~~~~~
  #############  Orthogonal Polynomials
  lmod <- lm(sr ~ poly(ddpi, 4), savings)
sumary(lmod)


~~~~~~~~~~~~~
  ########## Response Surface/ polynomials with interaction terms
  lmod <- lm(sr ~ polym(pop15, ddpi, degree=2), savings)
pop15r <- seq(20, 50, len=10)
ddpir <- seq(0, 20, len=10)
pgrid <- expand.grid(pop15=pop15r, ddpi=ddpir)
pv <- predict(lmod, pgrid)
persp(pop15r, ddpir, matrix(pv, 10, 10), theta=45, xlab="Pop under 15", ylab="Growth", zlab="Savings rate", ticktype="detailed", shade = .25)





~~~~~~~~~~~~~~~~~~~~~
  #################
#################  Splines
windows()
funky <- function(x) sin(2*pi*x^3)^3
x <- seq(0, 1, by = 0.01)
y <- funky(x) + 0.1*rnorm(101)
matplot(x, cbind(y, funky(x)), type="pl", ylab="y", pch=20, lty=1, col=1)

### adding orthog. polys of deg 4 and 12...

g4 <- lm(y ~ poly(x, 4))
g12 <- lm(y ~ poly(x, 12))
matplot(x, cbind(y, g4$fit, g12$fit), type="pll", ylab="y", lty=c(1,2), pch=20, col=1)


require(splines)
knots <- c(0, 0, 0, 0, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 1, 1, 1, 1)
bx <- splineDesign(knots,x)
lmodb <- lm(y ~ bx-1)
par(mfrow=c(1, 2))
matplot(x, bx, type ="l", col=1)
matplot(x, cbind(y, lmodb$fit), type = "pl", ylab="y", pch=20, lty=1, col=1)

########smoothing spline!!!
ssf <- smooth.spline(x, y)
matplot(x, cbind(y, ssf$y), type="pl", ylab = "y", lty=1, pch=20, col=1)