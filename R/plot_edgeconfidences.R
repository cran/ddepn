# For an inhibMCMC result object, plot the confidences of 
# activations and inhibitions for each protein
# 
# Author: benderc
###############################################################################


make_edge_df <- function(samp) {
	sam <- samp[[1]]	
	nr <- nrow(sam)
	L <- length(samp)
	## sequence of source nodes for all 10 runs
	src <- rep(rep(rownames(sam),nr),L)
	src <- factor(src,levels=rownames(sam))
	## sequence of dest nodes for all 10 runs
	dest <- rep(rep(rownames(sam),each=nr),L)
	dest <- factor(dest,levels=rownames(sam))
	## number of run
	nr <- rep(1:L,each=nr*nr)
	## counts
	y <- sapply(samp, "[")
	df <- data.frame(y=as.vector(y), src=src, dest=dest)
	df
}

plot_edgeconfidences <- function(ret, start=1, stop=NULL, act="conf.act", inh="conf.inh", cex.axis=1.5, ...) {
	#stopifnot(require(lattice))
	## check if only the samplings list is given, or the whole mcmc return object
	if(!is.null(ret$samplings)) {
		ret <- ret$samplings
	}
	L <- length(ret)
	N <- nrow(ret[[1]][[act]])
	## get the confidences/counts for each edge
	sampa <- lapply(ret, function(x) x[[act]])
	sampi <- lapply(ret, function(x) x[[inh]])
	## transform to data frame for lattice plotting
	dfa <- make_edge_df(sampa)
	dfi <- make_edge_df(sampi)
	type <- paste(c(as.character(dfa[,"src"]),as.character(dfi[,"src"])),c(rep("+",nrow(dfa)),rep("-",nrow(dfi))),sep="")
	df <- cbind(rbind(dfa, dfi),type)
	legendX <- simpleKey(c("activation (+)","inhibition (-)"),points=FALSE,rectangles=TRUE,lines=FALSE,col=c("#FF0000","#0000FF"))
	legendX$corner <- c(1,1)
	legendX$x <- 0.9
	legendX$y <- -0.1 # at the bottom, below the panels
	legendX$rectangles$col <- c("#FF0000","#0000FF")
	legendX$text$cex <- 2
	## make a lattice boxplot figure
	labeltxt <- paste("Confidences of activation/inhibition edges over ",L," MCMC runs.")
	bwplot(y ~ type | dest, data=df, pch=NULL, main=list(label=labeltxt,cex=2),
			ylab=list(label="confidence",cex=2),xlab=list(label="source node", cex=2),
			scales=list(rot=90,alternating=3,cex=cex.axis),
			panel=function(...) {
				panel.grid(h = 0, v = N-1, col="#111111aa", lty=3, lwd=2)
				panel.bwplot(...,fill=rep(c("blue","red"),N))
			},
			key=legendX,
			par.settings=list(layout.heights=list(bottom.padding=10)))
}

