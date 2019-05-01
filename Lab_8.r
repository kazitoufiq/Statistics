##Data
setwd("J:/Asset Management/Asset Performance and Investment/11_Analytics/R Working Dir/Lab")

tasmania <- read.csv("EditedRainfall.csv", stringsAsFactors = F)

names(tasmania)

s1 = tasmania[,2]  #Burnie
s2 = tasmania[,3]  #Cape Grim

#if we assume Normal, then MLE are mu.hat and sigma.hat which are mean(s1) and sqrt(((n-1)/n) * var(s1))
# respectively'''

mu.hat = mean(s1)
mu.hat
n= length(s1)
sigma.hat= sqrt(((n-1)/n) * var(s1))    
sigma.hat


library(MASS)
#fitdistr {MASS}	R Documentation
#Maximum-likelihood Fitting of Univariate Distributions

(normal.fit = fitdistr(x = s1, densfun = "normal"))

library(evd)
(gumbel.fit = fitdistr(x = s1, densfun = dgumbel, start = list(loc=50, scale=10)))


#In practical application we use negative log-likelihood


log.like = function(theta) {
loc = theta[1]
scale = theta[2]
out = sum(log(dgumbel(s1, loc=loc, scale=scale)))
return(out)
}


sum(log(dgumbel(s1, loc=41.54, scale=11.32))) ## we use the commad below to find the maximum log -likelihood

fit = optim(c(50,10), log.like, lower=0.0001, method="L-BFGS-B", control = list(fnscale=-1))

theta.hat = fit$par
theta.hat


###Carry out a visual check

pdf1 = function(x) {
dnorm(x, mean = mu.hat, sd = sigma.hat)
}


pdf2 = function(x) {
dgumbel(x, loc = theta.hat[1], scale = theta.hat[2])
}


hist(s1, freq = FALSE, col = "gray", main = NULL, xlab = "x", xlim = c(0, 100))
curve(pdf1, from = 0, to = 100, col = 2, lty = 2, lwd = 2, add=T)
curve(pdf2, from = 0, to = 100, col = 1, lty = 1, lwd = 2, add = TRUE)

log.like(theta.hat)





