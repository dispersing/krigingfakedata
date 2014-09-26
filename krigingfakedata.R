#load only kriging
	library(kriging)
	library(proto)
	library(nls2)

#make-and-fake x-y data (I made bimodal disttributions)
	high.points <- 50
	low.points <- 50
	x <- c(rnorm(high.points , 0 , 7.5) , rnorm(low.points , 30 , 7.5))
	y <- c(rnorm(high.points , 0 , 7.5) , rnorm(low.points , 30 , 7.5))

#make sure it looks bimodal *alone*
	hist(x)
	hist(y)

#make sure it looks bimodal *together*
	plot(x,y)

#add some values to the x and y points
	x.vals <- rnorm(high.points , 66 , 6.6)
	y.vals <- rnorm(high.points , 33 , 3.3)
	hist(x.vals)
	hist(y.vals)
	z <- c(x.vals , y.vals)

#krig it
	krig <- kriging(x, y, z)
	image(krig , las = 1 , xlab = "x" , ylab = "y")
	points(x , y , pch = 3 , cex = 0.5)

#see how long it will take to increase pixels (hint: it's exponential)
	a <- system.time(krig <- kriging(x, y, z , pixels = 10))[3]
	b <- system.time(krig <- kriging(x, y, z , pixels = 100))[3]
	c <- system.time(krig <- kriging(x, y, z , pixels = 500))[3]
	d <- system.time(krig <- kriging(x, y, z , pixels = 1000))[3]
	px <- c(10,100,500,1000)
	sys.time <- as.vector(c(a,b,c,d))
	str(sys.time)

#fit an exponential equation to the dta and plot it
	nls.mod <- nls2(sys.time ~ exp(a*px) , start = c(a=.01))
	summary(nls.mod)
	mod.seq <- seq(from = min(px) , to = max(px) , length = 100)
	pred.mod <- predict(nls.mod , list(px = mod.seq))
	
	plot(sys.time ~ px , xlim = c(0 , max(pix)) , ylim = c(0 , max(y)))
	lines(mod.seq , pred.mod , lty = 2)

#write a function based on the equation to see how long it will take for px pixels
	px.time <- function(px) {
		get.a <- summary(nls.mod)$coefficients[1]
		pxtime <- exp(get.a*px)/60
		paste("It will take your machine this many minutes:",round(pxtime , digits = 2))
	}
