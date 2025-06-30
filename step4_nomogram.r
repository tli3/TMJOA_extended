cd /overflow/tengfei/user/tengfei/projects/TMJ_baseline/updated_20230402/updated_20230427
module load r/4.2.2
R
library(rms)
A=read.csv('TMJOAI_040227_raw.csv')
colnames(A)[colnames(A)=='JS.JS']='JS'
colnames(A)[-1]=gsub('_Serum','',gsub('.1','',gsub('clinic.','',gsub('demo.','',gsub('_m','',gsub('_Saliva','',colnames(A)[-1]))))))
SelFea=c('Headaches','Ser.MMP.7','MouthOpening','Ser.VE.Cadherin','Ser.Angiogenin','Ser.PAI','Ser.VEGF','Sal.TGF','Ser.ENA.78','Sal.TRANCE','C.clusterProminence',
'Ser.TRANCE','Sal.TIMP.2','RestlessSleep','Ser.TIMP.2','MuscleSoreness','C.highGreyLevelRunEmphasis','Ser.BDNF','JS','Sal.VE.Cadherin','Sal.PAI')
A=A[,c(SelFea,'y')]
#A[,-dim(A)[2]]=scale(A[,-dim(A)[2]])
simplified_formula <- as.formula(paste("y ~", paste(SelFea, collapse = " + ")))
penalty_values = 10^seq(-6, 6, length.out = 300)
cv_results <- data.frame(penalty = penalty_values, 
                         Emax = rep(NA, length(penalty_values)), 
                         AUC = rep(NA, length(penalty_values)))

for (i in seq_along(penalty_values)) {
  penalty = penalty_values[i]
  logistic_rms_model <- lrm(simplified_formula, data = A, x = TRUE, y = TRUE, penalty = penalty)
  cv <- validate(logistic_rms_model, B = 10, method = "boot", bw = FALSE)
  cv_results$Emax[i] <- cv[5, 5]
  cv_results$AUC[i] <- (1+cv[1, 5])/2
}

par0=cv_results[which.min(cv_results$Emax),1]
#Efron's optimism is an estimate of the difference between the model's apparent performance 
#(how well it fits the data it was trained on) and its expected performance on new data. 
A[,-dim(A)[2]]=round(A[,-dim(A)[2]],2)
ddist <- datadist(A[,-dim(A)[2]])
options(datadist='ddist')
logistic_rms_model <- lrm(simplified_formula, data = A, x = TRUE, y = TRUE,penalty = par0)
nomogram <- nomogram(logistic_rms_model, fun = plogis, funlabel = "Probability")
pdf('nomo.pdf')
par(mar=c(0, 0.1, 0.1, 2.1), mgp=c(3, 1, 0), las=0)
plot(nomogram,xfrac=.5,cex.axis = 0.8,lmgp=0,minlength=2,nint=3,naxes=25,col.grid = gray(c(0.8, 0.95)))
dev.off()



