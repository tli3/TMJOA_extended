#cd /overflow/tengfei/user/tengfei/projects/TMJ_baseline/updated_20230402/updated_interact_20240321;module load r/3.6.0; R
library(Matrix)
library(qqman)
library(ggplot2) # Data visualization
library(data.table)
library(xgboost)
library(caret)
#library(qvalue)
library(circlize)
library(car)
library(EnvStats)
require(sparseLDA)
require(sda)
library(stringr)
require(gridExtra)
A=read.csv('TMJOAI_112223_Norm.csv',check.names = FALSE)
#A=read.csv('TMJOAI_112223_inter.csv',check.names = FALSE)
y=A[,1]
X=A[,-1]
p=dim(X)[2]
#boxcox(X[,3], objective.name = "Shapiro-Wilk")
modality=unlist(lapply(strsplit(colnames(X),'\\+'),'[',1))
gsub1<-function(i,vec,vec1){return(gsub(vec[i],'',vec1[i]))}
modality2=unlist(lapply(1:length(modality),gsub1,vec=paste0(modality,'\\+'),vec1=colnames(X)))

out='out/'
AUC<-P0<-NULL
for(i in 1:p)
{
rocobj <- pROC::auc(y,X[,i],smooth=F,direction ='auto')
temp=wilcox.test(X[y==1,i],X[y==0,i])$p.value
AUC<-c(AUC,max(rocobj,1-rocobj))
P0=c(P0,(temp))
}
Q0=p.adjust(P0,'fdr')

stat0=data.frame(list(COR=AUC,pval=P0,qval=Q0))
rownames(stat0)=paste0(modality,'_',modality2)
PATH0='out/circ.pdf'
temp0=sort(-stat0$COR,ind=T)$ix
stat0=stat0[temp0,];
#stat0=stat0[stat0[,1]>0.6,]
##################################################
rownames(stat0)=gsub('_Serum','',gsub('_Saliva','',rownames(stat0)))
indd00=gsub("[^*]", "", rownames(stat0))
for(ii in which(nchar(indd00)>0)){temp00=strsplit(rownames(stat0)[ii],'_');rownames(stat0)[ii]=temp00[[1]][length(temp00[[1]])]}
rownames(stat0)=gsub('C_C_','C_',rownames(stat0))
rownames(stat0)=gsub('_BL','',rownames(stat0))
rownames(stat0)=gsub('demo_','',rownames(stat0))
rownames(stat0)=gsub('clinic_','',rownames(stat0))
rownames(stat0)=gsub('Af_Af_','Af_',rownames(stat0))
rownames(stat0)=gsub('.BL','',rownames(stat0))
rownames(stat0)=gsub('longRunHighGreyLevelEmphasis','LongRhighGLE',rownames(stat0))
rownames(stat0)=gsub('shortRunLowGreyLevelEmphasis','shortRlowGLE',rownames(stat0))
rownames(stat0)=gsub('highGreyLevelRunEmphasis','highGLRE',rownames(stat0))
rownames(stat0)=gsub('InterferenceWithDailyActivities','Interfere',rownames(stat0))
rownames(stat0)=gsub('shortRunHighGreyLevelEmphasis','shortRhighGLE',rownames(stat0))
rownames(stat0)=gsub('haralickCorrelation','harCor',rownames(stat0))
rownames(stat0)=gsub('longRunLowGreyLevelEmphasis','longRlowGLE',rownames(stat0))
rownames(stat0)=gsub('lowGreyLevelRunEmphasis','lowGLRE',rownames(stat0))
rownames(stat0)=gsub('inverseDifferenceMoment','invDifMom',rownames(stat0))
rownames(stat0)=gsub('greyLevelNonuniformity','GLNonUnif',rownames(stat0))
rownames(stat0)=gsub('_','\\.',rownames(stat0))
rownames(stat0)=gsub('_JS','',gsub('Af_','',gsub('C_','',gsub('SAL','',gsub('Seru','',rownames(stat0))))))
rownames(stat0)=gsub('MouthOpening','MouthOpen',gsub('clusterProminence','clustProm',gsub('MuscleSoreness','MuscleSore',rownames(stat0))))
rownames(stat0)=gsub('\\.1','',rownames(stat0))
circplot<-function(stat0,PATH0='fig/circ.pdf')#circ.pdf #circ_inter
{
	factorstemp=substr(as.character(stat0$COR+rnorm(length(stat0$COR),0,0.001)),1,7)
	factorstemp1=as.numeric(factorstemp);factorstemp1[duplicated(factorstemp1)]=factorstemp1[duplicated(factorstemp1)]+0.0001
	factorstemp1[duplicated(factorstemp1)]=factorstemp1[duplicated(factorstemp1)]+0.0001
	factorstemp1[duplicated(factorstemp1)]=factorstemp1[duplicated(factorstemp1)]+0.0001
	factorstemp1[duplicated(factorstemp1)]=factorstemp1[duplicated(factorstemp1)]+0.0001
	factorstemp1[duplicated(factorstemp1)]=factorstemp1[duplicated(factorstemp1)]+0.0001
	factorstemp=as.character(factorstemp1);
	factors2=factor(factorstemp,levels =factorstemp)
	gsub1<-function(i,vec,vec1){return(gsub(vec[i],'',vec1[i]))}
	modality=unlist(lapply(strsplit(rownames(stat0),'_'),'[',1))
	factors3=unlist(lapply(1:length(modality),gsub1,vec=paste0(modality,'_'),vec1=rownames(stat0)))
	grDevices::cairo_pdf(PATH0,width=20,height=18)
	par(mar=c(14,8,13,8), xpd=T)
	par(bg = "white")
	rbPal <- colorRampPalette(c("green", "yellow", "red"))
	allh <- stat0$COR
	col_hc<- rbPal(25)[as.numeric(cut(allh,breaks = 25))]
	col_hf<- rbPal(25)[as.numeric(cut(-log(stat0$pval)/log(10),breaks = 25))]
	col_hm<- rbPal(25)[as.numeric(cut(-log(stat0$qval)/log(10),breaks = 25))]
	par(bg = "white")
	circos.par("track.height" = 0.2)
	circos.initialize(factors2,xlim=c(0,3))
	print('succeed!!!')
	circos.track(factors = factors2,ylim = c(0, 1), panel.fun = function(x, y) {
	  chr = get.cell.meta.data("sector.index")
	  xlim = get.cell.meta.data("xlim")
	  ylim = get.cell.meta.data("ylim")
	  circos.rect(0, xlim[1], xlim[2], 1,border = NA
	  )
	  circos.text(mean(xlim),mean(ylim)-0.3,substr(allh,1,5)[which(factorstemp==chr)], 
	  cex = 1.3, adj = c(0, degree(0)),facing = "clockwise", niceFacing = T)
	  circos.text(mean(xlim),mean(ylim)+0.7,factors3[which(factorstemp==chr)], cex = 1.3, 
	  adj = c(0, degree(0)),facing = "clockwise", niceFacing = T)
	}, bg.border = NA,bg.col=col_hc)
	print('succeed!!!')
	circos.track(factors = factors2,ylim = c(0, 1), panel.fun = function(x, y) {
	  chr = get.cell.meta.data("sector.index")
	  xlim = get.cell.meta.data("xlim")
	  ylim = get.cell.meta.data("ylim")
	  circos.rect(0, xlim[1], xlim[2], 1,border = NA)
	  circos.text(mean(xlim),mean(ylim)-0.3, labels=substr(-log(stat0$pval)/log(10),1,5)[which(factorstemp==chr)], chr, cex = 1.3, adj = c(0, degree(0)),facing = "clockwise", niceFacing = T)
	  #print(get.cell.meta.data("xlim"))
	}, bg.border = NA,bg.col=col_hf)
	circos.track(factors = factors2,ylim = c(0, 1), panel.fun = function(x, y) {
	  chr = get.cell.meta.data("sector.index")
	  xlim = get.cell.meta.data("xlim")
	  ylim = get.cell.meta.data("ylim")
	  circos.rect(0, xlim[1], xlim[2], 1,border = NA)
	  circos.text(mean(xlim),mean(ylim)-0.3, labels=substr(-log(stat0$qval)/log(10),1,5)[which(factorstemp==chr)], chr, cex = 1.3, adj = c(0, degree(0)),facing = "clockwise", niceFacing = T)
	  #print(get.cell.meta.data("xlim"))
	}, bg.border = NA,bg.col=col_hm)
	circos.info()
	col_fun = colorRamp2(c(0,0.4,0.8), c("green", "yellow", "red"))
	tt1=-log(stat0$pval)/log(10)
	tt2=-log(stat0$qval)/log(10)
	A0=c(round(min(stat0$COR),2), round((min(stat0$COR)+max(stat0$COR))/2,2),round(max(stat0$COR),2))
	#lgd_links = Legend(at = A0,labels=A0, col_fun=colorRamp2(A0, c("green", "yellow", "red")),grid_width=NULL,legend_width=1,title_position = "topleft", title = "Outer: correlation", direction = "horizontal")
	A0=c(round(min(tt1),1), round((min(tt1)+max(tt1))/2,1),round(max(tt1),1))
	#lgd_links2 = Legend(at = A0,labels=A0, col_fun=colorRamp2(A0, c("green", "yellow", "red")),grid_width=NULL,legend_width=1,title_position = "topleft", title = expression("Middle: -log"["10"]*"p"), direction = "horizontal")
	A0=c(round(min(tt2),1), round((min(tt2)+max(tt2))/2,1),round(max(tt2),1))
	#lgd_links3 = Legend(at = A0,labels=A0, col_fun=colorRamp2(A0, c("green", "yellow", "red")),grid_width=NULL,legend_width=1,title_position = "topleft", title = expression("Inner: -log"["10"]*"q"), direction = "horizontal")
	#lgd_links = Legend(at = c(0.01, 0.4,0.8), col_fun=col_fun,title_position = "topleft", title = "Outer: correlation", direction = "horizontal")
	#lgd_links2 = Legend(at = c(0.01, 0.4,0.8), col_fun=col_fun,title_position = "topleft", title = expression("Middle: -log"["10"]*"p"), direction = "horizontal")
	#lgd_links3 = Legend(at = c(0.01, 0.4,0.8),col_fun=col_fun,title_position = "topleft", title = expression("Inner: -log"["10"]*"q"), direction = "horizontal")

	#lgd_list_vertical<-packLegend(lgd_links,lgd_links2,lgd_links3)
	#lgd_list_vertical
	#pushViewport(viewport(x = unit(20, "mm"), y = unit(30, "mm"),   width = grobWidth(lgd_list_vertical),  height = grobHeight(lgd_list_vertical), just = c("left", "bottom")))
	#grid.draw(lgd_list_vertical)
	#upViewport()
	circos.clear()
	dev.off()
}
circplot(stat0)
