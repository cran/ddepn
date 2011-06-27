### R code from vignette source 'ddepn.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: no.nonsense
###################################################
rm(list=ls())


###################################################
### code chunk number 2: Load_package (eval = FALSE)
###################################################
## library(ddepn)


###################################################
### code chunk number 3: SimulateNetwork (eval = FALSE)
###################################################
## set.seed(12345)
## n <- 6
## signet <- signalnetwork(n=n, nstim=2, cstim=0, prop.inh=0.2)
## net <- signet$phi
## stimuli <- signet$stimuli
## weights <- signet$weights


###################################################
### code chunk number 4: SimulateData (eval = FALSE)
###################################################
## #plotdetailed(net,weights=weights,stimuli=stimuli,fontsize=15)
## ## sample data
## dataset <- makedata(net, stimuli, mu.bg=1200, sd.bg=400, mu.signal.a=2000, sd.signal.a=1000)
## data <- dataset$datx


###################################################
### code chunk number 5: RunGA (eval = FALSE)
###################################################
## ret <- ddepn(data, phiorig=net, inference="netga",
##               maxiterations=150, p=50, q=0.3, m=0.8,
##               usebics=TRUE)


###################################################
### code chunk number 6: PlotTheResults (eval = FALSE)
###################################################
## plotrepresult(ret)


###################################################
### code chunk number 7: MCMCsampling (eval = FALSE)
###################################################
## # use the original network as prior probability matrix
## B <- net
## B[B==2] <- -1
## # construct a prior matrix with uniform probabilities for each edge
## if(require(multicore)) {
## 	ret <- ddepn(data, phiorig=net, inference="mcmc",
## 		maxiterations=15010, burnin=1000,
## 		usebics=FALSE, lambda=0.01, B=B,
## 		multicores=TRUE, cores=2, priortype="laplaceinhib")
## }


###################################################
### code chunk number 8: Examine MCMC sampling (eval = FALSE)
###################################################
## plotrepresult(ret$samplings[[1]])


###################################################
### code chunk number 9: MCMC output (eval = FALSE)
###################################################
## mcmc1 <- mcmc(data=ret$ltraces[-c(1:burnin,mi),1])
## mcmc2 <- mcmc(data=ret$ltraces[-c(1:burnin,mi),2])
## mcmcl <- mcmc.list(mcmc1,mcmc2)
## plot(mcmcl)
## gelman.plot(mcmcl)


###################################################
### code chunk number 10: Gelmans potential scale reduction as convergence diagnostic (eval = FALSE)
###################################################
## gelman_diag(ret)


###################################################
### code chunk number 11: Plotting edge confidences (eval = FALSE)
###################################################
## plot_edgeconfidences(ret)


###################################################
### code chunk number 12: Plotting edge confidences (eval = FALSE)
###################################################
## net <- create_signetwork_cv(ret, alpha=0.05, adj.method="none", plot=FALSE, type="wilcox",
## 		alternative="one.sided", paired=FALSE, ord=NULL, sel_policy="strict")


###################################################
### code chunk number 13: Plotting the profiles and parameters (eval = FALSE)
###################################################
## plot_profiles(ret,mfrow=c(3,4))


###################################################
### code chunk number 14: Resuming inference of inhibMCMC (eval = FALSE)
###################################################
## ## resuming the inference from an inhibMCMC run and add another 100 iterations
## ret4 <- ddepn(dataset$datx,phiorig=phit, inference="mcmc", maxiterations=100,
## 		burnin=30, lambda=0.01, B=B, priortype="laplaceinhib", usebics=FALSE)
## ret4 <- resume_ddepn(ret4,maxiterations=100)


###################################################
### code chunk number 15: Resuming inference of netga (eval = FALSE)
###################################################
## ## resuming the inference from an netga run and add another 30 iterations
## ret5 <- ddepn(dataset$datx,phiorig=phit, inference="netga", maxiterations=20, p=10, q=0.3, m=0.8, lambda=0.01, B=B, priortype="laplaceinhib", usebics=FALSE)
## ret5 <- resume_ddepn(ret5,maxiterations=30)


###################################################
### code chunk number 16: ExampleNet (eval = FALSE)
###################################################
## phi <- matrix(0,nrow=5,ncol=5,dimnames=list(c("EGF","X","AKT","MEK","ERK"),c("EGF","X","AKT","MEK","ERK")))
## phi[1,c(3,4,5)] <- 1
## phi[2,4] <- 2
## phi[4,5] <- 1
## layout(t(matrix(c(1,2))),widths=c(1,2))
## plotdetailed(phi,stimuli=list(c(EGF=1,X=2)))
## plotmatrix(phi,"phi")


###################################################
### code chunk number 17: kegggraphs (eval = FALSE)
###################################################
## data(kegggraphs)
## length(kegggraphs)


###################################################
### code chunk number 18: kegggraphs name (eval = FALSE)
###################################################
## names(kegggraphs)[1]


###################################################
### code chunk number 19: kegggraphs element (eval = FALSE)
###################################################
## kegggraphs[[1]]


###################################################
### code chunk number 20: Convert graphNEL to detailed adjacency matrix (eval = FALSE)
###################################################
## kegggraph.to.detailed.adjacency(gR=kegggraphs[[1]]$g)


###################################################
### code chunk number 21: Use Laplaceinhib prior (eval = FALSE)
###################################################
## ddepn(data, lambda=0.01, B=B, usebics=FALSE,
## 		priortype="laplaceinhib")


###################################################
### code chunk number 22: Use Laplace prior (eval = FALSE)
###################################################
## ddepn(data, lambda=0.01, B=B, usebics=FALSE,
## 		priortype="laplace")


###################################################
### code chunk number 23: Use ScaleFree prior (eval = FALSE)
###################################################
## ddepn(data, gam=2.2, it=500, K=0.8, usebics=FALSE)


###################################################
### code chunk number 24: Data generation (eval = FALSE)
###################################################
## library(ddepn)
## set.seed(12345)
## n <- 6
## signet <- signalnetwork(n=n, nstim=2, cstim=0, prop.inh=0.2)
## net <- signet$phi
## stimuli <- signet$stimuli
## weights <- signet$weights
## dataset <- makedata(net, stimuli, mu.bg=1200, sd.bg=400, mu.signal.a=2000, sd.signal.a=1000)
## data <- dataset$datx
## 
## # netga arguments
## minetga <- 15
## p <- 30
## q <- 0.3
## m <- 0.8
## 
## # mcmc arguments
## mimcmc <- 1000
## burnin <- 100
## lambda <- 5
## 
## #laplace prior arguments
## lambda=0.01
## # for priortype="laplace": use original net as prior and reset inhibition
## # edges, such that their type is ignored
## Bpos <- net
## Bpos[Bpos==2] <- 1
## # for priortype="laplaceinhib": use original net as prior and reset
## # inhibition edge entries in B from 2 to -1
## B <- net
## B[B==2] <- -1
## 
## # scale free prior arguments
## gam <- 2.2
## it <- 500
## K <- 0.8
## 


###################################################
### code chunk number 25: GA BIC (eval = FALSE)
###################################################
## ret <- ddepn(data, phiorig=net, inference="netga",
##              maxiterations=minetga, p=p, q=q, m=m,
##              usebics=TRUE)


###################################################
### code chunk number 26: GA Laplace (eval = FALSE)
###################################################
## ret <- ddepn(data, phiorig=net, inference="netga",
## 		maxiterations=minetga, p=p, q=q, m=m,
## 		usebics=FALSE, lambda=lambda, B=B, priortype="laplaceinhib")


###################################################
### code chunk number 27: GA Laplace (eval = FALSE)
###################################################
## ret <- ddepn(data, phiorig=net, inference="netga",
## 		maxiterations=minetga, p=p, q=q, m=m,
## 		usebics=FALSE, lambda=lambda, B=Bpos, priortype="laplace")


###################################################
### code chunk number 28: GA ScaleFree (eval = FALSE)
###################################################
## ret <- ddepn(data, phiorig=net, inference="netga",
## 		maxiterations=minetga, p=p, q=q, m=m,
## 		usebics=FALSE, gam=gam, it=it, K=K, priortype="scalefree")


###################################################
### code chunk number 29: MCMC Laplace (eval = FALSE)
###################################################
## ret <- ddepn(data, phiorig=net, inference="mcmc",
## 		maxiterations=mimcmc, burnin=burnin,
## 		usebics=FALSE, lambda=lambda, B=B, priortype="laplaceinhib")


###################################################
### code chunk number 30: MCMC Laplace (eval = FALSE)
###################################################
## ret <- ddepn(data, phiorig=net, inference="mcmc",
## 		maxiterations=mimcmc, burnin=burnin,
## 		usebics=FALSE, lambda=lambda, B=Bpos, priortype="laplace")


###################################################
### code chunk number 31: MCMC ScaleFree (eval = FALSE)
###################################################
## ret <- ddepn(data,phiorig=net, inference="mcmc",
## 		maxiterations=mimcmc, burnin=burnin,
## 		usebics=FALSE, gam=gam, it=it, K=K, priortype="scalefree")


###################################################
### code chunk number 32: Get data (eval = FALSE)
###################################################
## data(hcc1954)


###################################################
### code chunk number 33: Get data (eval = FALSE)
###################################################
## dat <- format_ddepn(hcc1954)


###################################################
### code chunk number 34: Infer signalling network (eval = FALSE)
###################################################
## mi=1000
## p=500
## q=0.3
## m=0.8
## ret <- ddepn(dat, phiorig=NULL, inference="netga",
## 		maxiterations=mi, p=p, q=q, m=m,
## 		usebics=TRUE)


###################################################
### code chunk number 35: ddepn.Rnw:718-719
###################################################
toLatex(sessionInfo())


