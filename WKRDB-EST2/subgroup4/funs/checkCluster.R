checkCluster <- function(x=SS){
# Ana Fernandes
# makes a check for presence of clustering in SS
for (i in 1:length(x$SSid)){
if (length(b<-x[x$SSclustering!='N',]$SSid)>0){
	print ("ATTENTION: there is clustering for SSid's:")
	print (b)
	stop()
	}
}
}
