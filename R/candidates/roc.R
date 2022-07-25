roc2<-function(state, scores){
  
  labels=state>1
  labels=labels[order(scores, decreasing=TRUE)]
  
  reference=sort(scores)
  cumn=cumsum(rep(1,length(labels)))
  
  TPR =cumsum( labels)/sum(   labels)
  FPR =cumsum(!labels)/sum(  !labels)
  
  TP  =cumsum( labels)
  TN  =cumsum(!labels)
  
  FP  =sum( labels)-TP
  FN  =sum(!labels)-TN
  
  TSS =(TP/(TP+FN)-FP/(TN+FP))
  
  data.frame(state,score=reference,labels=labels,
             TPR=TPR,FPR=FPR,
             TP=TP,TN=TN,FP=FP,FN=FN,
             TSS=TSS)}

roc<-function(state, indicator){
  # label is true or false relative to an reference level, e.g. B/BMSY
  # indicatotor is from an indicator, does not need to be relative
  
  # i) order by descending indicatotor, i.e. indicator value
  order    =order(indicator, decreasing=TRUE)
  state    =state[order]
  indicator=indicator[order]
  label    =state>1
  
  # ii) assign labels of T/F to known values
  cumn     =seq(1,length(label))
  
  # iii) Calculate TRUE and FALSE positive rates
  TPR =cumsum( label)/sum( label) 
  FPR =cumsum(!label)/sum(!label)
  
  # iv) Calc TRUE and FALSE positives
  TP  =cumsum( label)  
  FP  =cumsum(!label)
  
  # v) Calc TRUE and FALSE negatives
  TN  =sum(!label)-FP  
  FN  =sum( label)-TP  
  
  # vi) calc True Skill Score for each indicatotor, i.e. indicator value
  TSS =(TP/(TP+FN)-FP/(FP+TN))
  
  data.frame(state    =state,
             label    =label,
             indicator=indicator,
             TPR=TPR,FPR=FPR,
             TP=TP,TN=TN,FP=FP,FN=FN,
             TSS=TSS,
             order=order)}

if (FALSE){
  library(FLCore)
  library(ggplotFL)
  library(pROC)
  
  
  data(ple4)
  
  # obs is truth from OM
  obs <- propagate(stock(ple4), 100)
  set.seed(876)
  
  # pred are model predicitons under different values of
  pred <- rlnorm(100, log(stock(ple4)), 0.6)
  
  plot(pred[,ac(2000:2017)])+
     geom_hline(aes(yintercept=mean(pred[,ac(2001:2017)])),col="blue")

  dat=model.frame(FLQuants(state=obs[, ac(2000:2017)],
                           indicatotor=pred[,ac(2000:2017)]))

  mn=mean(pred[,ac(2001:2017)])

  ggplot(dat)+
    geom_point(aes(state/mn,indicatotor/mn))+
    geom_hline(aes(yintercept=1))+
    geom_vline(aes(xintercept=1))

  rDat=roc(dat$state/mn>1,dat$indicatotor/mn)

  ggplot(roc(dat$state/mn,dat$indicatotor/mn))+
    geom_line(aes(FPR,TPR))

  auc(rDat$label,rDat$indicatotor)
  
}  