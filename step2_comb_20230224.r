cd /overflow/tengfei/user/tengfei/projects/TMJ_baseline/updated_20230402/updated_interact_20240321
module purge;module load gcc;module load r/4.2.2;R
i2=1;optN=2#24
method.list = c("glmnet", "svmLinear", "rf", "xgbTree", "lda2","nnet","naive_bayes","glmboost","hdda","bayesglm","lightgbm")     
library(caret)   
library(glmnet)   
library(pROC)
library(Matrix)   
library(qqman)   
library(MLmetrics)   
library(ggplot2) # Data visualization   
library(data.table)   
library(caret)   
library(LiblineaR)   
library(xgboost)   
library(lightgbm) 
library(SHAPforxgboost)  
library(MASS)   
options(scipen=999)   
library(data.table)   
if(!require("kernlab"))install.packages("kernlab",repos = "http://cran.us.r-project.org")   
library(pls)   
library(randomForest)   
A=read.csv("TMJOAI_112223_inter.csv",check.names = FALSE)   
y=A[,1]   
X=A[,-1]   
Nfold=10   
N=10   
fea=matrix(NA,dim(X)[2],Nfold*N);rownames(fea)=colnames(X)   
for(seed0 in 2022:2071){
	set.seed(seed0)   
	foldsCVT <- createFolds(factor(y), k=Nfold, list=TRUE, returnTrain=FALSE)   
	train.control <- trainControl(method = "cv", number = 5, # k-folds CV with k=10   
								  classProbs = TRUE,   
								  savePredictions = TRUE,   
								  summaryFunction = multiClassSummary)# save predictions for ROC   
	predYT=matrix(NA,length(y),1)   
	select=rep(NA,10)   
	predYT_valid=matrix(NA,length(y),10) 
	#file0=union(union(union(Sys.glob(paste0('out/',seed0,'/out_valid/*_rf*')),Sys.glob(paste0('out/',seed0,'/out_valid/*_lightgbm*'))),
	#Sys.glob(paste0('out/',seed0,'/out_valid/*_xgbTree*'))),Sys.glob(paste0('out/',seed0,'/out_valid/*_glmboost*')))
	file0=union(union(union(union(union(Sys.glob(paste0('out/',seed0,'/out_valid/glmnet_rf*')),Sys.glob(paste0('out/',seed0,'/out_valid/glmnet_lightgbm*'))),
	Sys.glob(paste0('out/',seed0,'/out_valid/glmnet_xgbTree*'))),Sys.glob(paste0('out/',seed0,'/out_valid/glmboost_rf*'))),
	Sys.glob(paste0('out/',seed0,'/out_valid/glmboost_lightgbm*'))),Sys.glob(paste0('out/',seed0,'/out_valid/glmboost_xgbTree*')))
	L00=length(file0);file0=unlist(lapply(strsplit(file0,'/'),'[',4))
	pred00=array(NA,c(length(y),10,L00))
	pred01=matrix(NA,length(y),L00)
	Shap=matrix(0,length(y),L00);colnames(Shap)= gsub('.txt','',file0)
	for(ii in 1:L00){pred00[,,ii]=as.matrix(read.table(paste0('out/',seed0,'/out_valid/',file0[ii])));pred01[,ii]=read.table(paste0('out/',seed0,'/out/',file0[ii]))[[1]]}
	#file1=union(union(union(Sys.glob(paste0('../updated_20240219/Clinical_image/out/',seed0,'/out_valid/lda2_svm*')),
	#Sys.glob(paste0('../updated_20240219/Clinical_image/out/',seed0,'/out_valid/lda2_bayesglm*'))),
	#Sys.glob(paste0('../updated_20240219/Clinical_image/out/',seed0,'/out_valid/lda2_glmboost*'))),
	#Sys.glob(paste0('../updated_20240219/Clinical_image/out/',seed0,'/out_valid/lda2_glmnet*')))
	#L01=length(file1);file1=unlist(lapply(strsplit(file1,'/'),'[',7))
	#pred00T=array(NA,c(length(y),10,L00+L01));pred00T[,,1:L00]=pred00;pred00=pred00T
	#pred01T=matrix(NA,length(y),L00+L01);pred01T[,1:L00]=pred01;pred01=pred01T
	#ShapT=matrix(0,length(y),L00+L01);colnames(ShapT)= c(gsub('.txt','',file0),gsub('.txt','',file1));ShapT[,1:L00]=Shap;Shap=ShapT
	#for(ii in 1:L01){pred00[,,L00+ii]=as.matrix(read.table(paste0('../updated_20240219/Clinical_image/out/',seed0,'/out_valid/',file1[ii])));
	#pred01[,ii+L00]=read.table(paste0('../updated_20240219/Clinical_image/out/',seed0,'/out/',file1[ii]))[[1]]}
	#file0=c(file0,file1)
	for(ii in 1:Nfold)   
	{   
		print(Nfold-ii)   
		indtempT=foldsCVT[[ii]]   
		y0=y[-indtempT]   
		X0=data.frame(pred00[-indtempT,ii,])
		X1=data.frame(pred01[indtempT,])
		colnames(X0)=colnames(X1)=gsub('.txt','',file0)
		p=dim(X0)[2]   
		training.set=as.data.frame(cbind(factor(paste0("X",y0)),X0));colnames(training.set)[1]="Y"   
		test.set=as.data.frame(X1)
		#model0 = train(Y~.,data = training.set,method = method.list[1],trControl = train.control,verbosity=0)
		model0 = train(Y~.,data = training.set,method = method.list[1],trControl = train.control,verbosity=0)#AUC
		rfImp <- varImp(model0, scale = T);ind00=sort(-as.matrix(round(rfImp[[1]][,1],3)),ind=T)$ix;
		tempp=t(t(as.matrix(round(rfImp[[1]],3))[ind00,]));rownames(tempp)=gsub("\\\\","",gsub("`","",rownames(tempp)))
		tempp=t(t(tempp[unique(rownames(tempp)),]))
		score=tempp[,1];names(score)=gsub("\\\\","",gsub("`","",names(score))) 
		optN=2#which(cumsum(score)/sum(score)>0.99)[1]
		if((i2<=5)&(i2!=1)){#shap  
			model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:optN])],method = method.list[i2],trControl = train.control,verbosity=0)}
		if(i2==6){model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:optN])],method = method.list[i2],center = TRUE)}   
		if(i2>6){model0 = train(Y~.,data = training.set[,c("Y",names(score)[1:optN])],method = method.list[i2],trControl = train.control)}   
		if(i2!=1){predYT_valid[-indtempT,ii] <- round(as.numeric(predict(model0,training.set[,c("Y",names(score)[1:optN])],type="prob")[,2]),4)
		predYT[indtempT]= round(as.numeric(predict(model0,test.set,type="prob")[,2]),4)
		for(jjj in 1:optN){ 
			datatemp=test.set;datatemp[,names(score)[jjj]]=0
			temp000=as.numeric(predict(model0, datatemp,type="prob")[,2])
			temp000=pmin(pmax(temp000,0.001),0.999)
			Shap[indtempT,names(score)[jjj]]=log(temp000/(1-temp000))-log(predYT[indtempT]/(1-predYT[indtempT]))}
		}   
		if(i2==1){
			cv0=cv.glmnet(as.matrix(training.set[,-1]),training.set[,"Y"],family="binomial",alpha=1)   
			lamb0=cv0$lambda.min   
			mod0 <- glmnet(as.matrix(training.set[,-1]),training.set[,"Y"],family="binomial",alpha=1,lambda=lamb0)   
			temp111=exp(as.numeric(predict(mod0,as.matrix(training.set[,-1]))[,1]))   
			predYT_valid[-indtempT,ii]=apply(training.set[,-1],1,median)#round(temp111/(1+temp111),4)  
			temp111=exp(as.numeric(predict(mod0,as.matrix(test.set))[,1]))   
			predYT[indtempT]=apply(test.set,1,median)#round(temp111/(1+temp111),4)#apply(test.set,1,mean)#apply(test.set,1,median)#round(temp111/(1+temp111),4) 
		}
	}
	summ0<-function(pred,Y){
		pred=as.numeric(pred);Y=as.numeric(Y)
		if(max(pred)==2)pred=pred-1
		if(max(Y)==2)Y=Y-1
		acc=sum((pred>0.5)==Y)/length(Y) 
		prec1=sum((pred>0.5)&(Y==1))/(sum(pred>0.5)+.00001) 
		prec0=sum((pred<=0.5)&(Y==0))/(sum(pred<=0.5)+.00001) 
		recall1=sum((pred>0.5)&(Y==1))/(sum(Y==1)+.00001) 
		recall0=sum((pred<0.5)&(Y==0))/(sum(Y==0)+.00001) 
		auc0=pROC::roc(Y,pred,smooth=F) 
		f1score=(1/(1/prec1+1/recall1)+1/(1/prec0+1/recall0))
		acc_sd=sqrt(acc*(1-acc)/(length(Y)));prec1_sd=sqrt(prec1*(1-prec1)/(sum(pred>0.5)+.00001))
		prec0_sd=sqrt(prec0*(1-prec0)/(sum(pred<=0.5)+.00001))
		recall1_sd=sqrt(recall1*(1-recall1)/(sum(Y==1)+.00001))
		recall0_sd=sqrt(recall0*(1-recall0)/(sum(Y==0)+.00001));
		temp1=as.numeric(ci.auc(Y,pred,method='delong'))
		auc_sd=diff(temp1)[1]/1.96
		prec10=rnorm(1000,prec1,prec1_sd)
		prec00=rnorm(1000,prec0,prec0_sd)
		recall10=rnorm(1000,recall1,recall1_sd)
		recall00=rnorm(1000,recall0,recall0_sd)
		f1score00=(1/(1/prec10+1/recall10)+1/(1/prec00+1/recall00))
		statt0=c(acc,acc_sd,prec1,prec1_sd,prec0,prec0_sd,recall1,recall1_sd,recall0,recall0_sd,f1score,sd(f1score00),as.numeric(auc0$auc),auc_sd) 
		names(statt0)=c('Accuracy','Accuracy_SD','Precision_case','Precision_case_SD','Precision_control','Precision_control_SD','Recall_case','Recall_case_SD',
		'Recall_control','Recall_control_SD','F1score','F1score_SD','AUC','AUC_SD')
		return(statt0)
	}
	summ1<-function(pred,Y){indtemp=which(!is.na(pred));return(summ0(pred[indtemp],Y[indtemp]))}
	print(round(summ0(predYT,y),4))
	#round(apply(apply(predYT_valid,2,summ1,Y=y),1,mean),4)
	write.table(predYT,file=paste0("final/comb_",seed0,".txt"),quote=F,col.names=F,row.names=F) 
	write.table(Shap,file=paste0("final/comb_",seed0,"_Shap.txt"),quote=F,row.names=F) 
	write.table(predYT_valid,file=paste0("final_valid/comb_",seed0,".txt"),quote=F,col.names=F,row.names=F)   
}	
predYT=matrix(NA,length(read.table(paste0('final/comb_',2022,'.txt'))[[1]]),50)
predYT1=array(NA,c(length(read.table(paste0('final/comb_',2022,'.txt'))[[1]]),10,50))
for(seed0 in 2022:2071)
{
predYT[,seed0-2021]=read.table(paste0('final/comb_',seed0,'.txt'))[[1]]
predYT1[,,seed0-2021]=as.matrix(read.table(paste0('final_valid/comb_',seed0,'.txt')))
}
res1=round(apply(predYT,2,summ1,Y=y),4)
res1=cbind(res1,round(apply(res1,1,mean),4),round(apply(res1,1,sd),4))
colnames(res1)=c(paste0('Seed_',2022:2071),'Mean','SD')
res1=res1[seq(1,13,2),]
write.csv(t(res1),file=paste0("final/EHPN.csv"),quote=F)
res1=apply(apply(predYT1,c(2,3),summ1,Y=y),c(1,3),mean)
res1=round(cbind(res1,apply(res1,1,mean),apply(res1,1,sd)),4)
colnames(res1)=c(paste0('Seed_',2022:2071),'Mean','SD')
res1=res1[seq(1,13,2),]
write.csv(t(res1),file=paste0("final/EHPN_valid.csv"),quote=F)

ShapT=array(NA,c(162,21,10))
for(seed0 in 2022:2031){
Shap=read.table(paste0("final/comb_",seed0,"_Shap.txt"),head=T);
Shap[Shap>8]=8
Shap[Shap< -8]=-8
ShapT[,,seed0-2021]=as.matrix(Shap)
}
Shap1=apply(ShapT,c(1,2),mean)
colnames(Shap1)=colnames(Shap);Shap=Shap1
X0=pred01
colnames(X0)=colnames(Shap)=gsub('lda2_','AUC_',colnames(Shap))
Shap[is.na(Shap)]=0;Shap=as.data.frame(Shap);
aa=shap.plot.summary.wrap2(round(Shap,4),X = X0,dilute =1)
pdf(paste0('final/Shap_combmethod.pdf'))
print(aa)
dev.off()
