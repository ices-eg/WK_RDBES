#load kirsten fct
source("../KirstenBirchHaakansson/fun_generic_su_object_upper_hie.R")

#load data
simH1<-readRDS("./data/simH1.rds")
out<-generic_su_object_upper_hie(simH1,1)
#seems ok : some dependencies on dplyr with type_of (?)
#try to simplify the function
# Varibale names for the output
var_names <- c("hierachy", "su", "recType", "idAbove", "id",
                 "stratification", "stratum", "clustering", "clusterName", 
                 "total", "sampled", "prob", "selectMeth", "selectMethCluster", "totalClusters", 
                 "sampledClusters", "probCluster")
fct1<-function(tab,var_names){
	#tab<-simH1[[4]]
	#id<-1
	if(any(grepl("prob",names(tab)))){
	idtab<-substr(names(tab)[grepl("recType",names(tab))],1,2)
	tabnames<-paste0(idtab,var_names)
	localnames<-names(tab)
	tab[,which(localnames%in%tabnames)]
	}
}
fct1(simH1[[3]],var_names)
lapply(names(simH1),function(tab){print(tab)})

simH1bis<-list(DE=simH1[["DE"]],SD=simH1[["SD"]],
		su1=list(VS=simH1[["VS"]],
			su2=list(FT=simH1[["FT"]],
				su3=list(FO=simH1[["FO"]],
					su4=list(SA=simH1[["SA"]])))))
fct1<-function(tab){tab[,which(grepl("prob",names(tab)))]}
rapply(simH1bis,fct1,how="replace")
rapply(simH1bis,`[[`,how="replace")

out2<-lapply(simH1,fct1,var_names)

pipo<-simH1[[1]]
piponame<- paste0(names(simH1)[1],var_names)
names(pipo)%in%piponame
pipo[,piponame[piponame%in%names(pipo)]]
fct1(simH1,var_names,5)

simH1[[3]][,paste0(names(simH1)[3],var_names)]
fct1(simH1,var_names,4)

#compute




