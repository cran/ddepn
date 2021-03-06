crossover <- function(phi1, phi2,fanin) {
  crossnode <- sample(1:nrow(phi1),1) 
  phi1.tmp <- phi1
  # change line, make sure that no edges are exchanged that would 
  # increase the number of incoming edges in the 'other' network
  # such that the node-fanin is bigger than fanin
  chcol <- 1:ncol(phi1)
  out1 <- which(colSums(ifelse(rbind(phi2[crossnode,],phi1[-crossnode,])==0,0,1)) > fanin)
  out2 <- which(colSums(ifelse(rbind(phi1[crossnode,],phi2[-crossnode,])==0,0,1)) > fanin)
  if(length(out1) > 0 | length(out2) > 0) {
  	ch <- chcol[-c(out1,out2)]
  } else {
	ch <- chcol
  }
  if(length(ch)>0) {
  	phi1[crossnode,ch] <- phi2[crossnode,ch]
  	phi2[crossnode,ch] <- phi1.tmp[crossnode,ch]
  }
  # now change the column except for the position phiX[crossnode,crossnode], since it is already changed
  # is zero anyways, so changing wouldn't make a difference. I keep this in the case that self-loops will be 
  # allowed some day
  phi1[-crossnode,crossnode] <- phi2[-crossnode,crossnode]
  phi2[-crossnode,crossnode] <- phi1.tmp[-crossnode,crossnode]
  return(list(phi1,phi2))
}
