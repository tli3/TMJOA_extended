#cd /overflow/tengfei/user/tengfei/projects/TMJ_baseline/updated_20230402/updated_20230427/;module purge;module load gcc;module load r/4.2.2;R
iii=15
method.list = c("glmnet", "svmLinear", "rf", "xgbTree", "lda2","nnet","naive_bayes","glmboost","hdda","bayesglm","lightgbm")    
vecT=expand.grid(c(1,3,4,5,6,8,11),1:11)    
i1=vecT[iii,1]    
i2=vecT[iii,2]    
library(caret)    
library(glmnet)    
library(Matrix)    
source("lightgbm.r")   
library(qqman)    
library(MLmetrics)    
library(ggplot2) # Data visualization    
library(data.table)    
library(LiblineaR)    
library(xgboost)    
library(lightgbm)    
library(MASS)    
library(partykit)
options(scipen=999)    
if(!require("kernlab"))install.packages("kernlab",repos = "http://cran.us.r-project.org")    
library(pls)    
library(randomForest)    
library(PRROC)    
A=read.csv("TMJOAI_040227_Norm.csv",check.names = FALSE)    
y=A[,1]    
X=A[,-1] 
PREV=colnames(X)[50:73]
Nfold=10    
N=10    
seed0=2022    
set.seed(seed0)    
foldsCVT <- createFolds(factor(y), k=Nfold, list=TRUE, returnTrain=FALSE)    
train.control <- trainControl(method = "cv", number = 10, # k-folds CV with k=10    
                              classProbs = TRUE,    
                              savePredictions = TRUE,    
                              summaryFunction = multiClassSummary)# save predictions for ROC    
train.control1 <- trainControl(method = "cv", number = 10, # k-folds CV with k=10    
                              classProbs = TRUE,    
                              savePredictions = TRUE,    
                              summaryFunction = twoClassSummary,verboseIter = F)# save predictions for ROC    
predYT=matrix(NA,length(y),1)  
Shap=matrix(0,length(y),dim(X)[2]);colnames(Shap)= colnames(X)   
select=rep(NA,10);select11=rep(NA,10)    
predYT_valid=matrix(NA,length(y),10)    
for(ii in 1:Nfold)    
{    
	print(Nfold-ii)    
	indtempT=foldsCVT[[ii]]    
	y0=y[-indtempT]    
	X0=X[-indtempT,]    
	X1=X[indtempT,]    
	p=dim(X0)[2]    
	training.set=as.data.frame(cbind(factor(paste0("X",y0)),X0));colnames(training.set)[1]="Y"    
	test.set=as.data.frame(X1)    
	#W0=table(training.set$Y)/sum(table(training.set$Y));w0=training.set$Y;w00=rep(NA,length(w0));w00[w0==names(W0)[1]]=W0[2];w00[w0==names(W0)[2]]=W0[1];    
	if(i1==1){  
		fea=matrix(NA,dim(X)[2],Nfold*N);rownames(fea)=colnames(X)   	 
		kk=1    
		predY=matrix(NA,length(y0),10)    
		for(seed1 in 2020:(2020+N-1))    
		{    
			if(seed1%%50==0)print(c((2020+N-1)-seed1,Nfold-ii))    
			set.seed(seed1)    
			foldsCV <- createFolds(factor(y0), k=Nfold, list=TRUE, returnTrain=FALSE)    
			for(i in 1:Nfold)    
			{    
				indtemp=foldsCV[[i]]    
				Y1=y0[-indtemp]    
				X.fea0=X0[-indtemp,]    
				p=dim(X.fea0)[2]    
				cv0=cv.glmnet(as.matrix(X.fea0),factor(Y1),family="binomial",alpha=1)    
				lamb0=cv0$lambda.min    
				mod0=glmnet(as.matrix(X.fea0),factor(Y1),family="binomial",alpha=1,lambda=lamb0)    
				fea[,kk]=mod0[[2]][,1]    
				kk=kk+1    
				predY[indtemp,seed1-2019]=predict(mod0,as.matrix(X[indtemp,]))    
			}    
		}    
		score=apply(fea!=0,1,mean)    
		score=sort(score,decreasing=T)    
	}    
	if((i1<=5)&(i1!=1)){    
	model0 = train(Y~.,data = training.set,method = method.list[i1],trControl = train.control,verbosity=0)}    
	if(i1==6){    
	model0 = train(Y~.,data = training.set,method = method.list[i1],center = TRUE)}    
	if(i1>6){    
	if(i1!=11)model0 = train(Y~.,data = training.set,method = method.list[i1],trControl = train.control)    
	if(i1==11)model0 = train(Y~.,data = training.set,method = lightgbm_caret_wrapper,trControl = train.control1)}    
	if(i1!=1){    
		rfImp <- varImp(model0, scale = T);ind00=sort(-as.matrix(round(rfImp[[1]][,1],3)),ind=T)$ix;   
		tempp=t(t(as.matrix(round(rfImp[[1]],3))[ind00,]));rownames(tempp)=gsub("\\\\","",gsub("`","",rownames(tempp)))  
		tempp=t(t(tempp[unique(rownames(tempp)),])) 
		write.table(tempp,file=paste0("out/2022/imp/",method.list[i1],"_",ii,".txt"),quote=F,col.names=F)    
		score=tempp[,1];names(score)=gsub("\\\\","",gsub("`","",names(score)))  
	}  
	if(i1==1){    
	write.table(t(t(score)),file=paste0("out/2022/imp/",method.list[i1],"_",ii,".txt"),quote=F,col.names=F)    
	}    
	selectT=c(5,10,15,20)    
	selectT=selectT[selectT<=length(score)]	 
	selectL=length(selectT)    
	AUCT=rep(NA,length(selectT))    
	for(jj in 1:length(selectT)){    
	if(i2<=5){model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:selectT[jj]])],method = method.list[i2],trControl = train.control,verbosity=0)}    
	if(i2==6){model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:selectT[jj]])],method = method.list[i2],center = TRUE)}    
	if(i2>6){if(i2<11)model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:selectT[jj]])],method = method.list[i2],trControl = train.control)    
	if(i2==11){model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:selectT[jj]])],method = lightgbm_caret_wrapper,trControl = train.control1)    
	temp=model0[[4]];colnames(temp)[colnames(temp)=="ROC"]="AUC";model0[[4]]=temp}}    
	AUCT[jj]=max(model0[[4]]$AUC)}    
	select[ii]=selectT[which.max(AUCT)]    
	for(jj in setdiff(1:10,ii)){    
		indvalidT=foldsCVT[[jj]]    
		y0_valid=y[-c(indtempT,indvalidT)]    
		X0_valid=X[-c(indtempT,indvalidT),]    
		X1_valid=X[indvalidT,]    
		training.set1=as.data.frame(cbind(factor(paste0("X",y0_valid)),X0_valid));colnames(training.set1)[1]="Y"    
		valid.set=as.data.frame(X1_valid)    
		if((i2<=5)&(i2!=1)){    
		model0 = train(Y~.,data = training.set1[,c("Y",names(score)[1:select[ii]])],method = method.list[i2],trControl = train.control,verbosity=0)}    
		if(i2==6){model0 = train(Y~.,data = training.set1[,c("Y",names(score)[1:select[ii]])],method = method.list[i2],center = TRUE)}    
		if(i2>6){if(i2!=11)model0 = train(Y~.,data = training.set1[,c("Y",names(score)[1:select[ii]])],method = method.list[i2],trControl = train.control)    
		if(i2==11)model0 = train(Y~.,data = training.set1[,c("Y",names(score)[1:select[ii]])],method = lightgbm_caret_wrapper,trControl = train.control1)}    
		if(i2!=1){predYT_valid[indvalidT,ii] <- round(as.numeric(predict(model0, valid.set,type="prob")[,2]),4)}    
		if(i2==1){    
			cv0=cv.glmnet(as.matrix(training.set1[,c(names(score)[1:select[ii]])]),training.set1[,"Y"],family="binomial",alpha=1)    
			lamb0=cv0$lambda.min    
			mod0 <- glmnet(as.matrix(training.set1[,c(names(score)[1:select[ii]])]),training.set1[,"Y"],family="binomial",alpha=1,lambda=lamb0)    
			temp111=exp(as.numeric(predict(mod0,as.matrix(valid.set[,c(names(score)[1:select[ii]])]))[,1]))    
			predYT_valid[indvalidT,ii]=round(temp111/(1+temp111),4)    
		}    
	}    
	if(i2==1){    
		cv0=cv.glmnet(as.matrix(training.set[,c(names(score)[1:select[ii]])]),training.set[,"Y"],family="binomial",alpha=1)    
		lamb0=cv0$lambda.min    
		mod0 <- glmnet(as.matrix(training.set[,c(names(score)[1:select[ii]])]),training.set[,"Y"],family="binomial",alpha=1,lambda=lamb0)    
		temp111=exp(as.numeric(predict(mod0,as.matrix(test.set[,c(names(score)[1:select[ii]])]))[,1]))    
		predYT[indtempT]=round(temp111/(1+temp111),4) 
		for(jjj in 1:select[ii]){ 
		datatemp=test.set;datatemp[,names(score)[jjj]]=0 
		temp000=as.numeric(predict(mod0,as.matrix(datatemp[,c(names(score)[1:select[ii]])]))[,1]) 
		Shap[indtempT,names(score)[jjj]]=temp000-log(predYT[indtempT]/(1-predYT[indtempT]))} 
	}    
	if((i2<=5)&(i2!=1)){    
	model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:select[ii]])],method = method.list[i2],trControl = train.control,verbosity=0)
	rf_model <- model0$finalModel; l0=rf_model$ntree;impT=rep(0,sum(!(colnames(training.set)%in%PREV))-1);names(impT)=setdiff(colnames(training.set),c('Y',PREV))
	for(i0 in 1:min(l0,100)){
	print(min(l0,100)-i0)
	individual_tree <- randomForest::getTree(rf_model, k = i0,labelVar=TRUE);
	temp0=individual_tree[which(gsub('`','',individual_tree[,3])%in%PREV),3:4];temp0[,1]=gsub('`','',temp0[,1])
	for(j0 in 1:dim(temp0)[1]){
		training.set2=training.set[,!(colnames(training.set)%in%PREV)];training.set2$Y=factor(paste0('X',as.numeric(training.set[,temp0[j0,1]]>temp0[j0,2])))
		temp11=table(training.set2$Y);temp11=sum(temp11)/temp11;w0=as.numeric(training.set2$Y);w0[w0==1]=temp11[1];w0[w0==2]=temp11[2]
		model0temp = train(Y~.,data = training.set2,method = 'rf',trControl = train.control,ntree=10,verbosity=0,weights=w0)
		rfImptemp <- varImp(model0temp, scale = T);impT[gsub('`','',rownames(rfImptemp[[1]]))]=impT[gsub('`','',rownames(rfImptemp[[1]]))]+rfImptemp[[1]][,1]
		}
    print(head(sort(impT,decreasing=T)))		
	}
	}
	impT=sort(impT,decreasing=T)
	selectT=c(5,10,15,20)    
	selectT=selectT[selectT<=length(score)]	 
	selectL=length(selectT)    
	AUCT=rep(NA,length(selectT))    
	for(jj in 1:length(selectT)){    
	model0 = train(Y~.,data = training.set[,union(c("Y",names(score)[1:select[ii]]),names(impT)[1:selectT[jj]])],method = method.list[i2],trControl = train.control,verbosity=0)
	AUCT[jj]=max(model0[[4]]$AUC)}    
	select11[ii]=selectT[which.max(AUCT)]
	model0 = train(Y~.,data = training.set[,union(c("Y",names(score)[1:select[ii]]),names(impT)[1:select11[ii]])],method = method.list[i2],trControl = train.control,verbosity=0)
    if(i2!=1){predYT[indtempT] <- round(as.numeric(predict(model0, test.set,type="prob")[,2]),4) 
		predYT[indtempT]=pmin(pmax(predYT[indtempT],0.001),0.999) 
		for(jjj in 1:select[ii]){  
		datatemp=test.set;datatemp[,names(score)[jjj]]=0 
		temp000=as.numeric(predict(model0, datatemp,type="prob")[,2]) 
		temp000=pmin(pmax(temp000,0.001),0.999) 
		Shap[indtempT,names(score)[jjj]]=log(temp000/(1-temp000))-log(predYT[indtempT]/(1-predYT[indtempT]))} 
	} 
}    
write.table(predYT,file=paste0("out/2022/out/",method.list[i1],"_",method.list[i2],".txt"),quote=F,col.names=F,row.names=F)    
write.table(predYT_valid,file=paste0("out/2022/out_valid/",method.list[i1],"_",method.list[i2],".txt"),quote=F,col.names=F,row.names=F)    
write.table(select,file=paste0("out/2022/select/",method.list[i1],"_",method.list[i2],".txt"),quote=F,col.names=F,row.names=F)    
write.table(Shap,file=paste0("out/2022/Shap/",method.list[i1],"_",method.list[i2],".txt"),quote=F,row.names=F) 
