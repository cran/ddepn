# Sample random networks.
# 
# Author: benderc
###############################################################################


samplephi <- function(phi,stimuli, antibodies, tps, reps, dat, searchstatespace=FALSE,
		hmmiterations=5, phiasis=FALSE, lambda=NULL, B=NULL, Z=NULL, fanin=4, gam=NULL,
		it=NULL, K=NULL, priortype="none",scale_lik=FALSE,allow.stim.off=TRUE,
		implementation="C") {
	if(phiasis) {
		phi.n <- phi
	} else {
		out <- match(unique(names(unlist(stimuli))),colnames(phi))
		nostimmat <- phi[,-out]
		proplength <- length(antibodies)-length(unique(unlist(stimuli)))
		propnames <- colnames(nostimmat)
		prop <- matrix(sample(c(0,1,2),length(nostimmat),replace=TRUE),
				nrow=nrow(nostimmat),ncol=ncol(nostimmat), dimnames=dimnames(nostimmat))
		for(p in propnames)
			prop[p,p] <- 0
		phi.n <- cbind(phi[,out,drop=F],prop)
		phi.n <- phi.n[,colnames(phi)]	
	}
	## remove ingoing edges into nodes that have more than fanin connections
	fanin_omit <- which(colSums(detailed.to.simple.regulations(phi.n))>fanin)
	if(length(fanin_omit)>0) {
		for(fi in fanin_omit) {
			phi.n[sample(which(phi.n[,fi]!=0),(sum(detailed.to.simple.regulations(phi.n)[,fi])-fanin)),fi] <- 0
		}
	}
	## get the possible system states
	gammaposs <- propagate.effect.set(phi.n,stimuli,allow.stim.off=allow.stim.off)
	# now get an initial gamma matrix
	gammax <- NULL
	for(sti in 1:length(stimuli)) {
		st <- stimuli[[sti]]
		indices <- grep(paste("^",paste(names(st),collapse="&"),"_",sep=""),colnames(gammaposs))
		## gammaposs are the unique states, they should be repeated by the number of replicates per stimulus
		if(length(indices)==1)
			gx <- replicatecolumns(gammaposs[,rep(indices,length(tps[[sti]]))],reps[sti])
		else
			gx <- replicatecolumns(gammaposs[,sort(sample(indices,length(tps[[sti]]),replace=TRUE))],reps[sti])
		gammax <- cbind(gammax, gx)
	}
	Ltmplist <- likl(dat,gammax,scale_lik)
	Ltmp <- Ltmplist$L
	thetax <- Ltmplist$theta
	Ltmp[Ltmp==Inf] <- 0
	Ltmp[Ltmp==-Inf] <- 0
	Lnew <- sum(Ltmp, na.rm=TRUE)
	bicnew <- get.bic(phi.n,Lnew,length(dat))
	aicnew <- get.aic(phi.n,Lnew)
	if(priortype %in% c("laplaceinhib","laplace","scalefree","uniform"))
		prnew <- prior(phi.n, lambda, B, Z, gam, it, K, priortype)
	else
		prnew <- 0
	if(searchstatespace) {
		bestmodel <- list(phi=phi.n,L=Lnew,aic=aicnew,bic=bicnew,dat=dat,
				theta=thetax, gamma=gammax, gammaposs=gammaposs, tps=tps, stimuli=stimuli, reps=reps,
				hmmiterations=hmmiterations, lastmove="addactivation", coords=c(1,1),fanin=fanin,scale_lik=scale_lik,allow.stim.off=allow.stim.off, 
				implementation=implementation)
		L.res <- perform.hmmsearch(phi.n, bestmodel)	
		gammax <- matrix(L.res$gammax,nrow=nrow(bestmodel$gamma),ncol=ncol(bestmodel$gamma),dimnames=dimnames(bestmodel$gamma))
		thetax <- matrix(L.res$thetax,nrow=nrow(bestmodel$theta),ncol=ncol(bestmodel$theta),dimnames=dimnames(bestmodel$theta))
		Lnew <- L.res$Likl
		bicnew <- L.res$bic
		aicnew <- L.res$aic	
	}
	# if prior is given
	if(priortype %in% c("laplaceinhib","laplace","scalefree","uniform")) {
		postnew <- Lnew + prnew
	} else {
		postnew <- NULL
	}
	return(list(phi.n=phi.n,gammax=gammax,thetax=thetax,posterior=postnew,Lnew=Lnew,bicnew=bicnew,aicnew=aicnew,
					gammaposs=gammaposs,lambda=lambda,B=B,Z=Z,gam=gam,it=it,K=K,pr=prnew,priortype=priortype,scale_lik=scale_lik,
					implementation=implementation))
}


initialphi <- function(dat, phi, stimuli, Lmax, thetax, gammax, gammaposs,
		tps, reps, antibodies, n=100, multicores=FALSE, lambda=NULL, B=NULL, Z=NULL,
		gam=NULL, it=NULL, K=NULL, priortype="none", scale_lik=FALSE, allow.stim.off=TRUE, 
		implementation="C") {
	phimax <- phi
	thetamax <- thetax
	gammamax <- gammax
	gammaposs <- gammaposs
	bicmin <- get.bic(phi,Lmax,length(dat))
	aicmin <- get.aic(phi,Lmax)
	jobs <- list()
	for(i in 1:n) {
		if(i%%50==1)
			cat(".")
		
		if(multicores) {
			jobs[[i]] <- parallel(samplephi(phimax,stimuli, antibodies, tps, reps, dat, lambda=lambda, B=B, Z=Z, gam=gam, it=it, K=K, priortype=priortype, scale_lik=scale_lik, allow.stim.off=allow.stim.off, implementation=implementation))	
		} else {
			jobs[[i]] <- samplephi(phimax,stimuli, antibodies, tps, reps, dat, lambda=lambda, B=B, Z=Z, gam=gam, it=it, K=K, priortype=priortype, scale_lik=scale_lik, allow.stim.off=allow.stim.off, implementation=implementation)
		}
	}
	if(multicores)
		jobs.res <- mccollect(jobs, wait=TRUE)
	else
		jobs.res <- jobs
	
	for(i in 1:length(jobs.res)) {
		res <- jobs.res[[i]]
		if(res$bicnew<bicmin) {
			print(paste("Improved: ",res$bicnew))
			phimax <- res$phi.n
			Lmax <- res$Lnew
			aicmin <- res$aicnew
			bicmin <- res$bicnew
			posteriormax <- res$posterior
			prmax <- res$pr
			thetamax <- res$thetax
			gammamax <- res$gammax
			gammaposs <- res$gammaposs
		}
	}
	return(list(phi=phimax, L=Lmax, aic=aicmin, bic=bicmin, posterior=posteriormax, thetax=thetamax,
				gammax=gammamax, gammaposs=gammaposs, lambda=lambda, B=B, Z=Z, gam=gam, it=it, K=K,
				pr=prmax, priortype=priortype,scale_lik=scale_lik, allow.stim.off=allow.stim.off, 
				implementation=implementation))
}

