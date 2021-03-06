\name{netga}
\Rdversion{1.1}
\alias{netga}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
netga - Perform network search using a genetic algorithm.
}
\description{
Uses a genetic algorithm for network search. Combines selection of best models,
crossing over of nodes and connected edges between pairs of nodes and mutation
of edges to sample the search space.
}
\usage{
netga(dat, stimuli, P=NULL, maxiterations=1000, p=100,
		q=0.3, m=0.8, hmmiterations=30, multicores=FALSE, usebics=FALSE, 
		cores=2, lambda=NULL, B=NULL, Z=NULL, scorefile=NULL, fanin = 4,
		gam=NULL,it=NULL,K=NULL,quantL=.5,quantBIC=.5,priortype="none", 
		plotresults=TRUE, scale_lik=FALSE, allow.stim.off=TRUE,
		debug=0,retobj=NULL, implementation="C")
}
\arguments{
  \item{dat}{
	The data matrix. See \code{\link{ddepn}}}
  \item{stimuli}{
	The list of stimuli. See \code{\link{ddepn}}}
  \item{P}{
	A list containing a population of network models. If NULL, an initial population
	of networks is sampled including the empty and fully connected network.}
  \item{maxiterations}{
	Integer, maximal number of generations.}
  \item{p}{
	The size of the population. Used only if P is not provided.}
  \item{q}{
	Double \eqn{\in [0;1]}. The rate of selection or crossing over. At most \eqn{(1-q) \cdot |P|}
	nodes are selected to persist into the next generation, but only if the fitness score is more optimal
	than the median of scores in the population. If less than \eqn{(1-q)*|P|} fulfill this criterion,
	the number \eqn{q \cdot |P|} of crossing overs is increased accordingly.}
  \item{m}{
	Double \eqn{\in [0;1]}. The mutation rate - \eqn{m \cdot |P|} networks out of the Population 
	are mutated \emph{after} selection and crossing over. Mutations happen for edges and 
	change the type of the edge randomly.}
  \item{hmmiterations}{
	Maximum number of iterations during the HMM.}
  \item{multicores}{
	Use multiple processors.}
  \item{usebics}{
    Use BIC statistic for model selection.}
  \item{cores}{
    Number of cores to use in case of \code{multicores}=TRUE}
  \item{lambda}{The Prior influence strength in the laplace prior.}
  \item{B}{The Prior information matrix. Corresponds to prior edge probabilities in the final network.}
  \item{Z}{The normalisation factor for the prior distribution.}
  \item{fanin}{Integer: maximal indegree for nodes.}
  \item{gam}{Prior influence strength in scalefree prior. Used as exponent in \code{laplaceinhib} prior: see
  	\code{\link{prior}} for details.}
  \item{it}{Number of iterations to generate background distribution in scalefree prior.}
  \item{K}{Proportionality factor in scalefree prior}
  \item{quantL}{Quantile of Population likelihood. Defines the selection threshold in \code{netga}.
    Also used as posterior selection quantile. Note that the Likelihood or Posterior have to be 
    maximised, so all networks with a likelihood/posterior \emph{greater} than the threshold are selected.}
  \item{quantBIC}{Quantile of Population likelihood. Defines the selection threshold in \code{netga}.
    Note that the BIC is minimised, so all networks with BIC \emph{less} than the threshold are
    selected.}
  \item{scorefile}{
  	Name of pdf-file where intermediate traces of the BIC, the differences of BICs in two succeeding 
  	generations and a comparison of optimum and average fitness are drawn. If NULL, the plot is drawn 
  	on the standard device for plotting.}
  \item{priortype}{Character. One of \code{none}, \code{laplaceinhib}, \code{laplace} or
   \code{scalefree} for use of the respective prior type. Ignored if \code{usebics=TRUE}. For 
   \code{netga}, \code{usebics=FALSE, priortype="none"} means optimising the likelihood 
   directly, for \code{mcmc_ddepn}, \code{none} is not allowed.}
  \item{plotresults}{Boolean. If TRUE, some statistics are plotted while netga is running.}
  \item{scale_lik}{Boolean. Perform scaling of the likelihood according to how 
    many data points were used to calculate the overall likelihood.}
  \item{allow.stim.off}{Boolean. If TRUE, the stimulus can become passive at some time. This will generate additional
    reachable system states, in particular all states from the normal state matrix, generated by 
    the propagation, but with the stimulus node set to 0.}
  \item{debug}{Numeric. If 0, a status bar indicates the progress of the algorithm. If 1 or 2, 
    extra information is printed to the console (for \code{debug=2} more information than 
    for \code{debug=1}).}
  \item{retobj}{List. The output generated during a GA run (see function \code{\link{ddepn}} for argument
    \code{inference="netga"}). Passed by function \code{resume_ddepn} to resume the inference.}
  \item{implementation}{String. One of \code{"C","R","R_globalest","C_globalest"}. Different implementations of the HMM in 
    \code{\link{perform.hmmsearch}}. If \code{"R"}, the original pure R-implementation is 
    used, if \code{"C"}, a ported C-implementation is used. If \code{"R_globalest"}, an experimental version of
    the parameter estimation is used in the HMM, \code{"C_globalest"} is the C-port of 
    this version. See \code{details} for a description.}
}
\details{
Usually this function is called internally by \code{\link{ddepn}}.
}
\value{
A list containing the following objects:\cr
A list named \code{P} holding the model objects for each candidate network in the population. Each 
entry in the list contains the following elements:
  \item{phi}{Adjacency matrix. The inferred network.}
  \item{L}{Likelihood.}
  \item{aic}{Akaikes Information Criterion.}
  \item{bic}{Bayesian Information Criterion.}
  \item{posterior}{Posterior probability. Only present if \code{usebics=FALSE}, i.e. when a prior
    model is used during inference.}
  \item{dat}{The data matrix.}
  \item{theta}{The parameter matrix for the Gaussians.}
  \item{gamma}{The state transition matrix.}
  \item{gammaposs}{The theoretical state transition matrix, as generated by the effect propagation.}
  \item{tps}{A list. Each element is a vector of time points for each experiment in the data matrix.}
  \item{stimuli}{List of stimuli.}
  \item{reps}{Number of replicates for each experiment.}
  \item{hmmiterations}{Maximum number of iterations during an HMM run.}
%  \item{TSA}{deprecated.}
%  \item{Tt}{deprecated.} 
  \item{lastmove}{String. Type of the last change that was performed.}
  \item{coords}{Position in the network where the last change was performed.}
  \item{lambda}{Laplace prior hyperparameter.}
  \item{B}{The Prior information matrix. See \code{\link{prior}} for details.}
  \item{Z}{Laplace prior normalisation factor. (Not used at the moment.)}
  \item{fanin}{Maximal indegree for nodes.}
  \item{gam}{Sparsity prior hyperparameter.}
  \item{it}{Sparsity prior iterations.}
  \item{K}{Sparsity prior scaling factor.}
  \item{priortype}{The prior that was used.}\cr
A matrix named \code{scorestats} holding in each column the traces of the following statistics, 
each row corresponds to one iteration:\cr
    \item{dL_total}{Median of total Likelihood change.}
    \item{dP_total}{Median of total Prior change.}
    \item{dL_crossover}{Median of Likelihood change for crossover.}
    \item{dL_mutation}{Median of Likelihood change for mutation.}
    \item{dP_crossover}{Median of Prior change for crossover.}
    \item{dP_mutation}{Median of Prior change for mutation.}
    \item{dL_total_abs}{As above, but absolute values.}
    \item{dP_total_abs}{As above, but absolute values.}
    \item{dL_crossover_abs}{As above, but absolute values.}
    \item{dL_mutation_abs}{As above, but absolute values.}
    \item{dP_crossover_abs}{As above, but absolute values.}
    \item{dP_mutation_abs}{As above, but absolute values.}
    \item{score}{Trace of the total median score. The score depends on the type of inference, if 
      \code{usebics=TRUE} it is the BIC score, if \code{usebics=FALSE} and \code{priortype="none"},
      it is the Liklihood score, otherwise the posterior score.}
}
\references{
TODO
}
\author{
Christian Bender
}
\note{
TODO
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ddepn}}
}
\examples{
\dontrun{
## load package
library(ddepn)

## sample a network
n <- 6
signet <- signalnetwork(n=n, nstim=2, cstim=0, prop.inh=0.2)
phit <- signet$phi
stimuli <- signet$stimuli

## sample data
dataset <- makedata(phit, stimuli, mu.bg=1200, sd.bg=400, mu.signal.a=2000, sd.signal.a=1000)

## now the GA
ret <- netga(dataset$dat, stimuli, P=NULL, maxiterations=30, p=15,
    q=0.3, m=0.8, hmmiterations=100, multicores=FALSE, usebics=TRUE, cores=1,
    lambda=NULL, B=NULL, scorefile=NULL, priortype="none")
## now the GA with prior
## use original network as prior matrix
## reset all entries for inhibiting edges 
## to -1
B <- phit
B[B==2] <- -1
ret <- netga(dataset$dat, stimuli, P=NULL, maxiterations=30, p=15,
    q=0.3, m=0.8, hmmiterations=100, multicores=FALSE, usebics=FALSE, cores=1,
    lambda=0.01, B=B, scorefile=NULL, priortype="laplaceinhib")


}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
