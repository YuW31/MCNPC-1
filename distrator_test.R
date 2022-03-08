distractor.check=function(Q){
  combn=combinat::combn
  Q=as.matrix(Q)
  J=length(unique(Q[,1]))
  K=ncol(Q)-2
  # [Test] Add Comments
  flag.useful = flag.proper = NULL
  for (j in 1:J){
    subQ=Q[which(Q[,1]==j),]
    if (is.matrix(subQ)==T){
      ## Check usefulness. Note: If (B-A)A=0, A is nested in B
      dif = matrix(rep(subQ[1,3:ncol(Q)],nrow(subQ)-1), nrow(subQ)-1, K, byrow=T) - subQ[2:nrow(subQ),3:ncol(Q)]
      if (length(which(subQ[1,3:ncol(Q)]%*%(t(abs(dif)))==0))!=0) flag.useful = rbind(flag.useful, cbind(j,which(subQ[1,3:ncol(Q)]%*%(t(dif))==0)))
      ## Check proper
      #if (nrow(subQ)==2 & abs(subQ[1,3:ncol(Q)] - subQ[2,3:ncol(Q)])%*%subQ[2,3:ncol(Q)]!=0) flag.proper = c(flag.proper, j)
      if (nrow(subQ)>2){
        # index of distractors
        index = as.matrix(combn(2:nrow(subQ), 2))
        for (k in 1:ncol(index)){
          union=(subQ[index[1,k],3:ncol(Q)] | subQ[index[2,k],3:ncol(Q)])*1
          # Check whether the union is in subQ
          condition = which(rowSums(abs(subQ[,3:ncol(Q)] - matrix(rep(union, nrow(subQ)), nrow(subQ), ncol(Q)-2, byrow=T)))==0)
          if (length(condition)==0 & (union-subQ[1, 3:ncol(subQ)])%*%subQ[1, 3:ncol(subQ)]!=0){
            flag.proper = c(flag.proper, j)
            break
          }else{
            k = k+1
          }
        }
      }
    }
  }
  if (is.null(flag.useful)==T & is.null(flag.proper)==T) return(cat("\n","All distractors are useful and proper.","\n\n"))
  if (is.null(flag.useful)==T & is.null(flag.proper)==F) return(cat("\n","All distractors are useful but distractors in items",flag.proper,"are not proper.","\n\n"))
  if (is.null(flag.useful)==F & is.null(flag.proper)==T) return(cat("\n","All distractors are proper but items", paste(flag.useful[,1], collapse = ", "),"are not useful.","\n\n"))
  if (is.null(flag.useful)==F & is.null(flag.proper)==F) return(cat("Items", paste(flag.useful[,1], collapse = ", "), "are not useful.","\n\n",
                                                                    "Distractors in items",paste(flag.proper, collapse = ", "), "are not proper.","\n\n"))
}


