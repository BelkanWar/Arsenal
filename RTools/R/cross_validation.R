#this function can only deal with glm(), lm() and gam() from mgcv
crossvalid<-function(model,fold=10){
  data<-model$model
  data$tAGG<-sample(gl(fold,1,length(data[[1]])))
  equation<-model$formula
  family=model$family
  validation<-c()
  for(i in 1:fold){
    k.model<-do.call(as.character(model$call)[1],list(formula=equation,data=subset(data,tAGG!=i),family=family))
    validation[i]<-sum((predict(k.model,newdata=subset(data,tAGG==i))-subset(data,tAGG==i)[,strsplit(as.character(model$formula),"~")[[2]][1]])^2)
  }
  accuracy<-mean(validation)
  accuracy
}
