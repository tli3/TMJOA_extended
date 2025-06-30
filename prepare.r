#cd /overflow/tengfei/user/tengfei/projects/TMJ_baseline/updated_20230402/updated_interact_20240321;module load r/4.3.2; R
library(Matrix)
library(ggplot2) # Data visualization
library(data.table)
library(xgboost)
library(caret)
A=read.csv('TMJOAI_112223_Norm.csv',check.names = FALSE)
y=A[,1]
X=A[,-1]
while(1)
{
	temp=abs(cor(X));temp1=apply(temp>0.7,1,sum);temp2=which.max(temp1);
	if(temp1[temp2]>1)X=X[,-temp2]
	if(temp1[temp2]==1)break
	print(dim(X)[2])
}
modality=unlist(lapply(strsplit(colnames(X),'\\+'),'[',1))
gsub1<-function(i,vec,vec1){return(gsub(vec[i],'',vec1[i]))}
modality2=unlist(lapply(1:length(modality),gsub1,vec=paste0(modality,'\\+'),vec1=colnames(X)))
x2 <- t(apply(X, 1, combn, 2, prod))
colnames(x2) <-paste(combn(modality, 2, paste, collapse="*"),combn(modality2, 2, paste, collapse="*"),sep='+')
X=cbind(X,x2)
while(1)
{
	temp=abs(cor(X));temp1=apply(temp>0.75,1,sum);temp2=which.max(temp1);
	if(temp1[temp2]>1)X=X[,-temp2]
	if(temp1[temp2]==1)break
	print(dim(X)[2])
}
#write.csv(cbind(y,X),file='TMJOAI_112223_inter.csv',row.names=F,quote=F)

ind00=union(union(grep('Sal\\*Sal',colnames(x2)),grep('Ser\\*Sal',colnames(x2))),grep('C\\*Sal',colnames(x2)))
x2=x2[,-ind00]
ind00=union(union(grep('Ser\\*Ser',colnames(x2)),grep('Ser\\*JS',colnames(x2))),grep('Sal\\*JS',colnames(x2)))
x2=x2[,-ind00]
ind00=union(union(grep('Af\\*Ser',colnames(x2)),grep('Af\\*Sal',colnames(x2))),grep('Af\\*Af',colnames(x2)))
x2=x2[,-ind00]
ind00=union(grep('C\\*Ser',colnames(x2)),grep('C\\*Sal',colnames(x2)))
x2=x2[,-ind00]
ind00=union(grep('C\\*C',colnames(x2)),grep('Af\\*Af',colnames(x2)))
x2=x2[,-ind00]
x2=x2[,c(74:78,146:490)]
X=cbind(X,x2)
write.csv(cbind(y,X),file='TMJOAI_112223_inter.csv',row.names=F,quote=F)



