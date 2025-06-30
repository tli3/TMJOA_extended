cd /overflow/tengfei/user/tengfei/projects/TMJ_baseline/updated_20230402/updated_interact_20240321;module load r/4.1.0;R
library(pROC)
library(dplyr)
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
#summ1<-function(pred,Y){indd=read.table('oldind.txt')[[1]];pred[-indd]=NA;indtemp=which(!is.na(pred));return(summ0(pred[indtemp],Y[indtemp]))}
iftest=1
A=read.csv('TMJOAI_112223_inter.csv',check.names = FALSE)
y=A[,1]
if(iftest==1)sub0=dir(paste0('out/',2027,'/out/'))
if(iftest==0)sub0=dir(paste0('out/',2027,'/out_valid/'))
STATTT=array(NA,c(length(sub0),14,50))
for(seed0 in 2027:2071){
	sub0=dir(paste0('out/',seed0,'/out/'))
	STATT=matrix(NA,length(sub0),14);rownames(STATT)=gsub('.txt','',sub0);colnames(STATT)=c('Accuracy','Accuracy_SD','Precision_case','Precision_case_SD','Precision_control','Precision_control_SD','Recall_case','Recall_case_SD',
		'Recall_control','Recall_control_SD','F1score','F1score_SD','AUC','AUC_SD')
	for(i in 1:length(sub0))
	{
	if(iftest==1){if(file.exists(paste0('out/',seed0,'/out/',sub0[i])))temp=read.table(paste0('out/',seed0,'/out/',sub0[i]))[[1]];
	if(file.exists(paste0('out/',seed0,'/out/',sub0[i])))temp=read.table(paste0('out/',seed0,'/out/',sub0[i]))[[1]];
	if(!file.exists(paste0('out/',seed0,'/out/',sub0[i])))temp=read.table(paste0('../out/',seed0,'/out/',sub0[i]))[[1]];
	STATT[i,]=round(summ0(temp,y),5)}
	if(iftest==0){if(file.exists(paste0('out/',seed0,'/out_valid/',sub0[i])))temp=read.table(paste0('out/',seed0,'/out_valid/',sub0[i]));
	if(!file.exists(paste0('out/',seed0,'/out_valid/',sub0[i])))temp=read.table(paste0('../out/',seed0,'/out_valid/',sub0[i]));
	STATT[i,]=round(apply(apply(temp,2,summ1,Y=y),1,mean),5)}
	}
	rownames(STATT)=gsub('lda2_','AUC_',rownames(STATT))
	STATTT[,,seed0-2026]=as.matrix(STATT)
	if(iftest==1)write.csv(STATT,paste0('final/Performance_test_set_',seed0,'_methods.csv'),quote=F)
	if(iftest==0)write.csv(STATT,paste0('final/Performance_valid_set_',seed0,'_methods.csv'),quote=F)
}
MSTATT=apply(STATTT,c(1,2),mean);colnames(MSTATT)=colnames(STATT);rownames(MSTATT)=rownames(STATT)
SSTATT=apply(STATTT,c(1,2),sd);MSTATT[,seq(2,14,2)]=SSTATT[,seq(1,13,2)]
if(iftest==1)write.csv(MSTATT,paste0('final/Performance_test_set_methods.csv'),quote=F)
if(iftest==0)write.csv(MSTATT,paste0('final/Performance_valid_set_methods.csv'),quote=F)
sort(STATTT[,11,1])


STATT=MSTATT;
#uu=rownames(STATT);uu1=unlist(lapply(strsplit(uu,'_'),'[',2));indd=which(uu1%in%c('xgbTree','rf','lightgbm'));STATT=STATT[indd,]
max(STATT[,'F1score'])
temp=gsub('svmLinear','SVM',gsub('glmboost','glmboo',gsub('naive_bayes','NaiveBayes',rownames(STATT))))
SEL0=unlist(lapply(strsplit(temp,'_'),'[',1))
ML0=unlist(lapply(strsplit(temp,'_'),'[',2))
F1=STATT[,'F1score']
if(iftest==1)pdf('fig/F1score_test_set_by_selection_method.pdf')
if(iftest==0)pdf('fig/F1score_valid_set_by_selection_method.pdf')
par(mfrow=c(2,1))
boxplot(F1~SEL0,las=2,xlab='')
stripchart(F1~SEL0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)    
boxplot(F1~ML0,las=2,xlab='')
stripchart(F1~ML0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 
dev.off()
if(iftest==1)pdf('fig/AUC_test_set_by_selection_method.pdf')
if(iftest==0)pdf('fig/AUC_valid_set_by_selection_method.pdf')
AUC=STATT[,'AUC']   
par(mfrow=c(2,1))
boxplot(AUC~SEL0,las=2,xlab='')
stripchart(AUC~SEL0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)    
boxplot(AUC~ML0,las=2,xlab='')
stripchart(AUC~ML0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 
dev.off()
if(iftest==1)pdf('fig/ACC_test_set_by_selection_method.pdf')	   
if(iftest==0)pdf('fig/ACC_valid_set_by_selection_method.pdf')	   
ACC=STATT[,'Accuracy']   
par(mfrow=c(2,1))
boxplot(ACC~SEL0,las=2,xlab='')
stripchart(ACC~SEL0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)    
boxplot(ACC~ML0,las=2,xlab='')
stripchart(ACC~ML0,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE) 
dev.off()
############################################################
#SHP
cd /overflow/tengfei/user/tengfei/projects/TMJ_baseline/updated_20230402/updated_interact_20240321;module load r/4.2.2;R
library(SHAPforxgboost)
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
	statt0=c(acc,prec1,prec0,recall1,recall0,f1score,as.numeric(auc0$auc)) 
	names(statt0)=c('Accuracy','Precision_case','Precision_control','Recall_case','Recall_control','F1score','AUC')
	return(statt0)
}
A=read.csv('TMJOAI_112223_inter.csv',check.names = FALSE)
y=A[,1]
X=A[,-1]  
library(SHAPforxgboost) 
for(seed0 in 2022:2071)
{
	sub0=dir(paste0('out/',seed0,'/Shap/'))
	STATT=matrix(NA,length(sub0),7);rownames(STATT)=gsub('.txt','',sub0);colnames(STATT)=c('Accuracy','Precision_case','Precision_control','Recall_case','Recall_control','F1score','AUC')
	for(i in 1:length(sub0))
	{
	temp=read.table(paste0('out/',seed0,'/out/',sub0[i]))[[1]]
	STATT[i,]=summ0(temp,y)
	}
	max(STATT[,'F1score'])
	pat='lightgbm_rf'
	pat='xgbTree_glmnet'
	aa=NULL
	Shap=read.table(paste0('out/',seed0,'/Shap/',pat,'.txt'),head=T)
}
Shap[Shap>10]=10
Shap[Shap< -10]=-10
X0=X;colnames(X0)=colnames(Shap)
shap0<- shap.prep(shap_contrib = round(Shap,4), X_train = X0)
aa=shap.plot.summary.wrap2(round(Shap,4),X = X0,dilute =1, top_n = 40)
pdf('Shap_xgbTree_glmnet.pdf')
plot(aa)
dev.off()


patT=c('glmnet_rf','glmboost_rf','glmnet_xgbTree','glmboost_xgbTree','glmnet_lightgbm','glmboost_lightgbm')
X0=NULL;Shap=array(0,c(300,dim(X)[1],dim(X)[2]));ii=1;X0=X
for(i in 1:length(patT)){
	print(i)
	for(seed0 in 2022:2071){
	temp=as.matrix(read.table(paste0('out/',seed0,'/Shap/',patT[i],'.txt'),head=T));temp[is.na(temp)]=0
	Shap[ii,,]=temp
	ii=ii+1
	}
}
Shap1=apply(Shap,c(2,3),mean);Shap=Shap1
colnames(Shap)=colnames(X0);
colnames(Shap)=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',colnames(Shap))))))
colnames(X0)=colnames(Shap)
Shap=as.data.frame(Shap)
aa=shap.plot.summary.wrap2(round(Shap,4),X = X0,dilute =1, top_n = dim(X0)[2])
ab=aggregate(aa[[1]]$mean_value,by=list(aa[[1]]$variable),FUN=mean)
indd=which(cumsum(ab[,2])/sum(ab[,2])>0.95)[1]
var0=ab[1:indd,1]
aa=shap.plot.summary.wrap2(round(Shap,4),X = X0,dilute =1, top_n = indd)
pdf(paste0('fig/Shap_comb_feaT.pdf'))
print(aa)
dev.off()
#############################################
cd /overflow/tengfei/user/tengfei/projects/TMJ_baseline/updated_20230402/updated_interact_20240321;module load r/4.2.2;R
library(SHAPforxgboost)
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
	statt0=c(acc,prec1,prec0,recall1,recall0,f1score,as.numeric(auc0$auc)) 
	names(statt0)=c('Accuracy','Precision_case','Precision_control','Recall_case','Recall_control','F1score','AUC')
	return(statt0)
}
A=read.csv('TMJOAI_112223_inter.csv',check.names = FALSE)
y=A[,1]
X=A[,-1]  
patT=c('glmnet_rf','glmboost_rf','glmnet_xgbTree','glmboost_xgbTree','glmnet_lightgbm','glmboost_lightgbm')
ShapT=read.table(paste0('out/',2022,'/Shap/',patT[1],'.txt'),head=T)
colnames(ShapT)=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',colnames(ShapT))))))
temp0=colnames(ShapT)
ShapT=matrix(NA,50,dim(ShapT)[2])
colnames(ShapT)=temp0
rownames(ShapT)=2022:2071
for(seed0 in 2022:2071){
	X0=NULL;Shap=NULL
	print(seed0)
	for(i in 1:length(patT)){
		Shap=rbind(Shap,read.table(paste0('out/',seed0,'/Shap/',patT[i],'.txt'),head=T))
		X0=rbind(X0,X);
		}
	colnames(Shap)=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',colnames(Shap))))))
	colnames(X0)=colnames(Shap)
	Shap[is.na(Shap)]=0;
	aa=shap.plot.summary.wrap2(round(Shap,4),X = X0,dilute =1)
	ab=aggregate(aa[[1]]$mean_value,by=list(aa[[1]]$variable),FUN=mean)
	rownames(ab)=ab[,1]
	ShapT[seed0-2021,]=ab[colnames(ShapT),2]
}
temp0=apply(ShapT,2,sum)
ShapT1=ShapT[,temp0>0.01]
temp0=apply(ShapT1,2,sum)
ind0=sort(-temp0,ind=T)$ix
ShapT2=ShapT1[,ind0]
write.csv(ShapT2,file='fig/SHAP_change.csv',quote=F)
pdf('fig/SHAP_change.pdf')
old_par <- par()
par(xpd=T)
par(mar=c(5, 4, 4, 15), oma=c(0, 0, 0, 0))
matplot(ShapT2[,1:20],type='l',col=c(1:9,1:9),lty=c(rep(1,9),rep(2,9)))
legend("topright", inset=c(-0.9, 0), # Use negative inset to move legend outside
       legend=colnames(ShapT2)[1:20], # Your labels
       col=c(1:9, 1:9), 
       lty=c(rep(1,9), rep(2,9)), 
       xpd=TRUE,cex=0.5)
par(old_par)
dev.off()


patT=c('glmnet_rf','glmboost_rf','glmnet_xgbTree','glmboost_xgbTree','glmnet_lightgbm','glmboost_lightgbm')
X0=NULL;Shap=array(0,c(300,dim(X)[1],dim(X)[2]));ii=1;X0=X
colnames(X0)=gsub('C_C_','C_',colnames(X0))
colnames(X0)=gsub('_BL','',colnames(X0))
colnames(X0)=gsub('demo_','',colnames(X0))
colnames(X0)=gsub('clinic_','',colnames(X0))
colnames(X0)=gsub('Af_Af_','Af_',colnames(X0))
colnames(X0)=gsub('.BL','',colnames(X0))
colnames(X0)=gsub('longRunHighGreyLevelEmphasis','LongRhighGLE',colnames(X0))
colnames(X0)=gsub('shortRunLowGreyLevelEmphasis','shortRlowGLE',colnames(X0))
colnames(X0)=gsub('highGreyLevelRunEmphasis','highGLRE',colnames(X0))
colnames(X0)=gsub('InterferenceWithDailyActivities','Interfere',colnames(X0))
colnames(X0)=gsub('shortRunHighGreyLevelEmphasis','shortRhighGLE',colnames(X0))
colnames(X0)=gsub('haralickCorrelation','harCor',colnames(X0))
colnames(X0)=gsub('longRunLowGreyLevelEmphasis','longRlowGLE',colnames(X0))
colnames(X0)=gsub('lowGreyLevelRunEmphasis','lowGLRE',colnames(X0))
colnames(X0)=gsub('inverseDifferenceMoment','invDifMom',colnames(X0))
colnames(X0)=gsub('greyLevelNonuniformity','GLNonUnif',colnames(X0))
colnames(X0)=gsub('_','\\.',colnames(X0))
colnames(X0)=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',colnames(X0))))))
colnames(X0)=gsub('MouthOpening','MouthOpen',gsub('clusterProminence','clustProm',gsub('MuscleSoreness','MuscleSore',colnames(X0))))
colnames(X0)=gsub('\\.1','',colnames(X0))
for(i in 1:length(patT)){
	print(i)
	for(seed0 in 2022:2071){
	temp=as.matrix(read.table(paste0('out/',seed0,'/Shap/',patT[i],'.txt'),head=T));temp[is.na(temp)]=0
	Shap[ii,,]=temp
	ii=ii+1
	}
}
Shap1=apply(Shap,c(2,3),mean);Shap=Shap1
colnames(Shap)=colnames(X0);
colnames(Shap)=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',colnames(Shap))))))
colnames(X0)=colnames(Shap)
Shap=as.data.frame(Shap)
aa=shap.plot.summary.wrap2(round(Shap,4),X = X0,dilute =1, top_n = dim(X0)[2])
ab=aggregate(aa[[1]]$mean_value,by=list(aa[[1]]$variable),FUN=mean)
indd=which(cumsum(ab[,2])/sum(ab[,2])>0.95)[1]
var0=ab[1:indd,1]
aa=shap.plot.summary.wrap2(round(Shap,4),X = X0,dilute =1, top_n = indd)
pdf(paste0('fig/Shap_comb_feaT.pdf'))
print(aa)
dev.off()





library(ggplot2)
grDevices::cairo_pdf('feat_imp.pdf') 
	score2_1=ab[1:indd,];temp=score2_1[,1];score2_1=score2_1[,-1];names(score2_1)=temp
	data00=cbind(names(score2_1),score2_1);colnames(data00)=c('TopFeatures','Select_importance')
	data00=as.data.frame(data00);rownames(data00)=NULL;data00[,2]=as.numeric(as.character(data00[,2]))
	indd=which(cumsum(score2_1)/sum(score2_1)>0.98)[1];data00=data00[1:indd,]
	print(eval(substitute(ggplot(data00, aes(x=reorder(var1,var2,mean), y=var2)) + geom_bar(stat="identity", 
			width=0.5,color="tan4",fill="darkslateblue",alpha=0.2, 
			size=0.5,)+coord_flip()+ ggtitle(paste0("Feature Importance:"))+
			stat_summary(fun.y=mean, geom="point", shape=20, size=1, color="red", fill="red") +
			theme(axis.text.x = element_text(face="bold",angle = 90, hjust = 1)),
			list(var1=as.name(colnames(data00)[1]),var2=as.name(colnames(data00)[2])))))
	dev.off() 
	
write.csv(ab,'ShapImp.csv',quote=F,row.names=F)

library(ggplot2)
#X0=X;#colnames(X0)=gsub('\\+','\\.',colnames(X0))
colnames(X0)=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',colnames(X0))))))
temp0=as.character(var0);temp0=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',temp0)))))
temp0=gsub('C_C_','C_',temp0)
temp0=gsub('_BL','',temp0)
temp0=gsub('demo_','',temp0)
temp0=gsub('clinic_','',temp0)
temp0=gsub('Af_Af_','Af_',temp0)
temp0=gsub('.BL','',temp0)
temp0=gsub('longRunHighGreyLevelEmphasis','LongRhighGLE',temp0)
temp0=gsub('shortRunLowGreyLevelEmphasis','shortRlowGLE',temp0)
temp0=gsub('highGreyLevelRunEmphasis','highGLRE',temp0)
temp0=gsub('InterferenceWithDailyActivities','Interfere',temp0)
temp0=gsub('shortRunHighGreyLevelEmphasis','shortRhighGLE',temp0)
temp0=gsub('haralickCorrelation','harCor',temp0)
temp0=gsub('longRunLowGreyLevelEmphasis','longRlowGLE',temp0)
temp0=gsub('lowGreyLevelRunEmphasis','lowGLRE',temp0)
temp0=gsub('inverseDifferenceMoment','invDifMom',temp0)
temp0=gsub('greyLevelNonuniformity','GLNonUnif',temp0)
temp0=gsub('_','\\.',temp0)
temp0=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',temp0)))))
temp0=gsub('MouthOpening','MouthOpen',gsub('clusterProminence','clustProm',gsub('MuscleSoreness','MuscleSore',temp0)))
temp0=gsub('\\.1','',temp0)
colnames(X0)=gsub('C_C_','C_',colnames(X0))
colnames(X0)=gsub('_BL','',colnames(X0))
colnames(X0)=gsub('demo_','',colnames(X0))
colnames(X0)=gsub('clinic_','',colnames(X0))
colnames(X0)=gsub('Af_Af_','Af_',colnames(X0))
colnames(X0)=gsub('.BL','',colnames(X0))
colnames(X0)=gsub('longRunHighGreyLevelEmphasis','LongRhighGLE',colnames(X0))
colnames(X0)=gsub('shortRunLowGreyLevelEmphasis','shortRlowGLE',colnames(X0))
colnames(X0)=gsub('highGreyLevelRunEmphasis','highGLRE',colnames(X0))
colnames(X0)=gsub('InterferenceWithDailyActivities','Interfere',colnames(X0))
colnames(X0)=gsub('shortRunHighGreyLevelEmphasis','shortRhighGLE',colnames(X0))
colnames(X0)=gsub('haralickCorrelation','harCor',colnames(X0))
colnames(X0)=gsub('longRunLowGreyLevelEmphasis','longRlowGLE',colnames(X0))
colnames(X0)=gsub('lowGreyLevelRunEmphasis','lowGLRE',colnames(X0))
colnames(X0)=gsub('inverseDifferenceMoment','invDifMom',colnames(X0))
colnames(X0)=gsub('greyLevelNonuniformity','GLNonUnif',colnames(X0))
colnames(X0)=gsub('_','\\.',colnames(X0))
colnames(X0)=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',colnames(X0))))))
colnames(X0)=gsub('MouthOpening','MouthOpen',gsub('clusterProminence','clustProm',gsub('MuscleSoreness','MuscleSore',colnames(X0))))
colnames(X0)=gsub('\\.1','',colnames(X0))

feature30=scale(X0[,temp0])[T]
class0=rep(y,dim(X0[,temp0])[2])
class1=class0
name0=t(matrix(rep(colnames(X0[,temp0]),dim(X0[,temp0])[1]),dim(X0[,temp0])[2]))[T]
SCORE2=ab[1:indd,2];names(SCORE2)=ab[1:indd,1]
data00=cbind(name0,class1,feature30,as.matrix(SCORE2)[T])
colnames(data00)=c('Feature','Group','Score','Score0')
data00=data.frame(data00)
data00$Score=as.numeric(as.character(data00$Score))
data00$Score0=as.numeric(as.character(data00$Score0))
data00$Feature=as.factor(data00$Feature)
data00$Group=as.factor(data00$Group)
data00[,1]=factor(data00[,1])
temp0=-sort(-unique(SCORE2))[10]
data00=data00[data00$Score0>temp0,]
grDevices::cairo_pdf(paste0('fig/','/boxplot_rf_event.pdf'))
ggplot(aes(y = Score, x = reorder(Feature,-Score0,mean), fill = Group), data = data00) +
geom_boxplot()+ggtitle(paste0("Boxplots of top features for case (1) vs control (0) "))+
theme(axis.text.x = element_text(face="bold",angle = 90, hjust = 1),
plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))+xlab('')+ylab('Normalized Measures')
dev.off()


library(fields);library(matlab)
library(ggplot2)
#X0=X;#colnames(X0)=gsub('\\+','\\.',colnames(X0))
colnames(X0)=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',colnames(X0))))))
temp0=as.character(var0);temp0=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',temp0)))))
temp0=gsub('C_C_','C_',temp0)
temp0=gsub('_BL','',temp0)
temp0=gsub('demo_','',temp0)
temp0=gsub('clinic_','',temp0)
temp0=gsub('Af_Af_','Af_',temp0)
temp0=gsub('.BL','',temp0)
temp0=gsub('longRunHighGreyLevelEmphasis','LongRhighGLE',temp0)
temp0=gsub('shortRunLowGreyLevelEmphasis','shortRlowGLE',temp0)
temp0=gsub('highGreyLevelRunEmphasis','highGLRE',temp0)
temp0=gsub('InterferenceWithDailyActivities','Interfere',temp0)
temp0=gsub('shortRunHighGreyLevelEmphasis','shortRhighGLE',temp0)
temp0=gsub('haralickCorrelation','harCor',temp0)
temp0=gsub('longRunLowGreyLevelEmphasis','longRlowGLE',temp0)
temp0=gsub('lowGreyLevelRunEmphasis','lowGLRE',temp0)
temp0=gsub('inverseDifferenceMoment','invDifMom',temp0)
temp0=gsub('greyLevelNonuniformity','GLNonUnif',temp0)
temp0=gsub('_','\\.',temp0)
temp0=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',temp0)))))
temp0=gsub('MouthOpening','MouthOpen',gsub('clusterProminence','clustProm',gsub('MuscleSoreness','MuscleSore',temp0)))
temp0=gsub('\\.1','',temp0)
X1=X0[,temp0]
cor0=cor(X1);m=n=dim(X1)[2]
pdf('fig/cor_inter1.pdf',width=8)
par(mar=c(14.1,12.1,1.1,5.1))
par(font.axis = 2)
image(x=1:n, y=1:m,z=t(cor0),xlab='', ylab='', main="",yaxt="n", xaxt='n',zlim = c(-1,1),lwd=2,xpd=T,col=jet.colors(64),cex.lab=0.1,cex=0.1)
axis(1,at=1:n,labels=colnames(cor0),tick=F,las=2,mgp = c(10, 0.1, 10),cex=0.1,cex.lab=0.1,cex.axis=0.5)
axis(2,at=1:m,labels=rownames(cor0),tick=F,las=2,mgp = c(3, 0.1, 0),cex=0.1,cex.lab=0.1,cex.axis=0.5)
#image.plot(x=1:n, y=1:m,z=t(cor0), legend.only=T,col=jet.colors(64))
#n=8;m=12
#grid(nx =n, ny = m, col = "1", lty = 1,lwd = 0.8, equilogs = TRUE)
#segments((0.5), 0.5, (0.5), (m+0.5), col=1,lty=1,lwd = 4)
#segments((n+0.5), 0.5, (n+0.5), (m+0.5), col=1,lty=1,lwd = 4)
#segments((n+m+0.5), (m+0.5),(0.5), (m+0.5), col=1,lty=1,lwd = 4)
#segments((n+0.5), (0.5),(0.5), (0.5), col=1,lty=1,lwd = 4)
#plot.new()
image.plot(legend.only=TRUE, zlim=c(-1,1),col=jet.colors(64)) 
#for(j1 in 1:dim(adj_pval0)[1]){for(j2 in 1:dim(adj_pval0)[2]){if((adj_pval0[j1,j2]<0.05)&(adj_pval0[j1,j2]>=0.01))text(j2,j1,'*',cex=2)
#if((adj_pval0[j1,j2]<0.01)&(adj_pval0[j1,j2]>=0.001))text(j2,j1,'**',cex=2)
#if((adj_pval0[j1,j2]<0.001))text(j2,j1,'***',cex=2)}}
dev.off()

###################################
#AUCplot
###################################
library(ggplot2)
seed0=2022
sub0=paste0('out/',seed0,'/out/',c('glmnet_lightgbm.txt','glmnet_xgbTree.txt','glmnet_rf.txt','glmboost_lightgbm.txt','glmboost_xgbTree.txt','glmboost_rf.txt'))
name0=gsub(paste0('out/',seed0,'/out/'),'',gsub('.txt','',sub0))
PredTestT=array(NA,c(length(y),length(sub0),50));
for(seed0 in 2022:2071){
for(i in 1:length(sub0))
{
A=read.table(sub0[i])[[1]]
PredTestT[,i,seed0-2021]=A
}
}
PredTestT=aperm(PredTestT,c(1,3,2));p2=dim(PredTestT)[2]
PredTestT=matrix(PredTestT,dim(PredTestT)[1]*p2)
PredTest=cbind(PredTestT,rep(y,p2))
colnames(PredTest)=c(name0,'Y')
PredTest=as.data.frame(PredTest)
combT=NULL;for(seed0 in 2022:2071)combT=c(combT,read.table(paste0('final/comb_',seed0,'.txt'))[[1]])
PredTest1=cbind(PredTest,combT)
colnames(PredTest1)[dim(PredTest1)[2]]='Ensemble'
colnames(PredTest1)=gsub('naive_bayes_','auc_',gsub('hdda_','auc_',gsub('svmLinear_','auc_',colnames(PredTest1))))
#PredTest1=cbind(PredTest1,X[,var0[1:7]])
PredTest1=PredTest1
PredTest1=as.data.frame(PredTest1)
PredTest1=PredTest1[,c(1:6,8,7)]


grDevices::cairo_pdf(paste0('fig/','rocT_test.pdf'))
par(mar=c(10.1, 4.1, 10.1, 9.1), xpd=TRUE)
type0=c(rep(3,8),rep(1,6))
smooth0=F
wid0=2.1
col0=c('black','gray47','blue','darkorange','red','darkgreen','purple','mediumvioletred',
'darkred','darkgoldenrod4','slateblue4','green','gold')
pROC::plot.roc(as.numeric(PredTest1$Y)~PredTest1[,1],smooth=smooth0,lty=type0[1],lwd=wid0,col=col0[1],cex.lab=1.3,cex.axis=1.3)
for(i in c(1:6))
{
pROC::plot.roc(as.numeric(PredTest1$Y)~PredTest1[,i+1],smooth=smooth0,lty=type0[i+3],
lwd=wid0,col=col0[i+1],cex.lab=1.3,cex.axis=1.3,add=T)
}
legend('bottomright', legend=colnames(PredTest1)[-dim(PredTest1)[2]],
       col=col0[1:7], lty=c(type0[1],type0[4:9]), cex=0.75,lwd=rep(2,7),
       text.font=1,box.lwd=0,box.col='white',bg='lightgray')
dev.off()
#####
#heatmap
cd /overflow/tengfei/user/tengfei/projects/TMJ_baseline/updated_20230402/updated_interact_20240321
module load r/4.2.1;R
library(fields)
library(matlab)
A=read.csv('TMJOAI_112223_Norm.csv')
A=A[,-1]
cor0=cor(A)
m=74;n=74
pdf('fig/cor1.pdf')
par(mar=c(14.1,9.1,1.1,5.1))
par(font.axis = 2)
image(x=1:n, y=1:m,z=t(cor0),xlab='', ylab='', main="",yaxt="n", xaxt='n',zlim = c(-1,1),lwd=2,xpd=T,col=jet.colors(64),cex.lab=0.3)
axis(1,at=1:n,labels=colnames(cor0),tick=F,las=2,mgp = c(10, 0.1, 10))
axis(2,at=1:m,labels=rownames(cor0),tick=F,las=2,mgp = c(3, 0.1, 0))
#image.plot(x=1:n, y=1:m,z=t(cor0), legend.only=T,col=jet.colors(64))
#n=8;m=12
#grid(nx =n, ny = m, col = "1", lty = 1,lwd = 0.8, equilogs = TRUE)
#segments((0.5), 0.5, (0.5), (m+0.5), col=1,lty=1,lwd = 4)
#segments((n+0.5), 0.5, (n+0.5), (m+0.5), col=1,lty=1,lwd = 4)
#segments((n+m+0.5), (m+0.5),(0.5), (m+0.5), col=1,lty=1,lwd = 4)
#segments((n+0.5), (0.5),(0.5), (0.5), col=1,lty=1,lwd = 4)
#plot.new()
image.plot(legend.only=TRUE, zlim=c(-1,1),col=jet.colors(64)) 
#for(j1 in 1:dim(adj_pval0)[1]){for(j2 in 1:dim(adj_pval0)[2]){if((adj_pval0[j1,j2]<0.05)&(adj_pval0[j1,j2]>=0.01))text(j2,j1,'*',cex=2)
#if((adj_pval0[j1,j2]<0.01)&(adj_pval0[j1,j2]>=0.001))text(j2,j1,'**',cex=2)
#if((adj_pval0[j1,j2]<0.001))text(j2,j1,'***',cex=2)}}
dev.off()

cd /overflow/tengfei/user/tengfei/projects/TMJ_baseline/updated_20230402/updated_interact_20240321
module load r/4.2.1;R
library(fields)
library(matlab)
A=read.csv('TMJOAI_112223_inter.csv')
A=A[,-1]
cor0=cor(A)
m=752;n=752
pdf('fig/cor_inter.pdf')
par(mar=c(14.1,9.1,1.1,5.1))
par(font.axis = 2)
image(x=1:n, y=1:m,z=t(cor0),xlab='', ylab='', main="",yaxt="n", xaxt='n',zlim = c(-1,1),lwd=2,xpd=T,col=jet.colors(64),cex.lab=0.3)
axis(1,at=1:n,labels=colnames(cor0),tick=F,las=2,mgp = c(10, 0.1, 10))
axis(2,at=1:m,labels=rownames(cor0),tick=F,las=2,mgp = c(3, 0.1, 0))
#image.plot(x=1:n, y=1:m,z=t(cor0), legend.only=T,col=jet.colors(64))
#n=8;m=12
#grid(nx =n, ny = m, col = "1", lty = 1,lwd = 0.8, equilogs = TRUE)
#segments((0.5), 0.5, (0.5), (m+0.5), col=1,lty=1,lwd = 4)
#segments((n+0.5), 0.5, (n+0.5), (m+0.5), col=1,lty=1,lwd = 4)
#segments((n+m+0.5), (m+0.5),(0.5), (m+0.5), col=1,lty=1,lwd = 4)
#segments((n+0.5), (0.5),(0.5), (0.5), col=1,lty=1,lwd = 4)
#plot.new()
image.plot(legend.only=TRUE, zlim=c(-1,1),col=jet.colors(64)) 
#for(j1 in 1:dim(adj_pval0)[1]){for(j2 in 1:dim(adj_pval0)[2]){if((adj_pval0[j1,j2]<0.05)&(adj_pval0[j1,j2]>=0.01))text(j2,j1,'*',cex=2)
#if((adj_pval0[j1,j2]<0.01)&(adj_pval0[j1,j2]>=0.001))text(j2,j1,'**',cex=2)
#if((adj_pval0[j1,j2]<0.001))text(j2,j1,'***',cex=2)}}
dev.off()