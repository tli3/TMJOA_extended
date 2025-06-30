cd /overflow/tengfei/user/tengfei/projects/TMJ_baseline/updated_20230402/updated_interact_20240321
module load r/4.3.2;R
sub0=Sys.glob('code*')
sub0=setdiff(sub0,c(paste0('code_',2022:2026)))
L=length(sub0)
for(ii in 1:L)
{
system(paste0('cd ',sub0[ii],';sh All.sh'))
}