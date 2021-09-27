checkStratif <- function(x=SS){
# Ana Fernandes
# makes a check for presence of stratification in SS
for (i in 1:length(x$SSid)){
if (length(a<-x[x$SSstratification!='N',]$SSid)>0){
	print("ATTENTION: there is stratification for SSid's:")
	print (a)
	stop()
}
}
}
