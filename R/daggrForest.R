# Random forest downsampling and aggregating function
daggrForest = function(X, y, nagg=25, trace=TRUE, ncore=1, ...){
  
  train.under = X[which(y==0),]
  train.over = X[which(y==1),]
  n.over = nrow(train.over)
  
  parfun = function(i){
    traini = data.frame(rbind(train.over,
                              train.under[sample(1:nrow(train.under),
                                                 n.over, replace=F),]))
    rf.i = randomForest(formula,
                        data=traini, na.action=na.omit, ...)
    if(trace)
      print(paste("End of model",i), quote=F)
    return(rf.i)
  }
  if(ncore==1)
    rf.list = lapply(1:nagg, parfun)
  else
    rf.list = mclapply(1:nagg, parfun, mc.cores=ncore)
  return(rf.list)
}

# Predict from an daggrForest list
predict = function(rf.list, newdata){
  nagg = length(rf.list)
  preds = matrix(0, ncol=nagg, nrow=nrow(newdata))
  for(i in 1:nagg){
    preds[,i] = predict(rf.list[[i]], newdata, type="prob")[,2]
  }
  return(rowMeans(preds))
}

# Variable importance plot
ImportancePlot(rf.list, sort=TRUE){
  nagg=length(rf.list)
  imp.mat = matrix(0, ncol=nagg, nrow=rf.list[[1]]$forest$xlevels)
  for(i in 1:nagg){
    imp.mat[,i] = as.numeric(rf.list[[i]]$importance)
  }
  imp.mean = rowMeans(imp.mat)
  ord = order(imp.mean, decreasing=F)
  imp.mean = as.matrix(imp.mean[ord])
  row.names(imp.mean) = row.names(m5a[[1]]$importance)[ord]
  colnames(imp.mean) = NA
  dotchart(imp.mean, main="Averaged variable importance plot",
           xlim=c(0,5e3),
           xlab="Mean decrease in Gini coefficient")
  
}