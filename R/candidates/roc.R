roc<-function(state, score,ref=1){
  state    =state[order(score, decreasing=TRUE)]
  reference=rev(sort(score))
  labels   =state>ref
  cumn     =cumsum(rep(1,length(labels)))
  
  TPR =cumsum( labels)/sum( labels) 
  FPR =cumsum(!labels)/sum(!labels)
  
  TP  =cumsum( labels)  
  FP  =cumsum(!labels)
  
  FN  =sum( labels)-TP  
  TN  =sum(!labels)-FP  
  
  TSS =(TP/(TP+FN)-FP/(FP+TN))
  
  data.frame(state,score=reference,labels=labels,
             TPR=TPR,FPR=FPR,
             TP=TP,TN=TN,FP=FP,FN=FN,
             TSS=TSS)}
