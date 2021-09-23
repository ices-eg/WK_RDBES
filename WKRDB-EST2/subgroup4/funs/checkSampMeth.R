checkSampMeth <- function(x=SS){
# Ana Fernandes
# makes a check for Selection methods other than 'CENSUS'
for (i in 1:length(x$SSid)){
if (length(a <- x[x$SSselectMeth!='CENSUS',]$SSid)>0){
print("ATTENTION: selection method is not CENSUS for SSid's:")
print (a)
stop()
}
}
}
