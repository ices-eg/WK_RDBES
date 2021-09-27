library(data.table)
df1<-readRDS("dataDNKupdte.rds")
colnames(df1)[2]<-"LEid"
df1<-data.table(df1)
length(unique(df1$LEid)) # 4080 LE
summary(df1[,.N, list(LEid)]$N) # from 1 to 13 spp per LEid
max(df1[,.N,list(SAspeciesCode, LEid)]$N) # no problem with duplicates
max(df1[,.N,list(SAspeciesCodeFAO, LEid)]$N)


# random sampling of species from a list restricted to positive occurrences: freq of occurrence and weight

nsims =500
samp_size = 100
res<-c()
res_wt<-c()
spp_list<-c("TOZ","WHB","SQR","ANE","SAN","DAB","HER")
spp_samp_size<-2 #mi = units sampled

for(i in 1:nsims){
print(i)
df1_sampled<-df1[df1$LEid %in% sample(unique(df1$LEid), size=samp_size, replace=TRUE),]
    # adds probInc and Pi for LE sampling
    df1_sampled$probIncLE<-samp_size/length(unique(df1$LEid))
    df1_sampled$sampWeightLE<-1/df1_sampled$probIncLE
ls1<-split(df1_sampled, df1_sampled$LEid)
ls2<-lapply(ls1, function(x){
# subset spp from list available in LEid
spp_list_present_in_LEid<-x[x$SAspeciesCodeFAO %in% spp_list,]$SAspeciesCodeFAO #Mi = cluster size
x$sppPresent<-length(unique(x$SAspeciesCodeFAO))
x$sppList<-length(unique(spp_list))
x$numTotal<-length(spp_list_present_in_LEid)
x$numSamp<-spp_samp_size
if(length(spp_list_present_in_LEid)<=spp_samp_size) {
    x<-x[x$SAspeciesCodeFAO %in% spp_list_present_in_LEid,] # take all
    x$probIncSS1<-1
    x$sampWeightSS1<-1/x$probIncSS1
    } else {
            x<-x[x$SAspeciesCodeFAO %in% sample(spp_list_present_in_LEid, size=spp_samp_size, replace=FALSE),] #take sample
            x$probIncSS1<-spp_samp_size/length(spp_list_present_in_LEid)
            x$sampWeightSS1<-1/x$probIncSS1
            }
    x
})

df1_sampled2<-do.call("rbind", ls2)
df1_sampled2$SAspeciesCodeFAO<-factor(df1_sampled2$SAspeciesCodeFAO, levels=spp_list)

res_repl<-tapply(df1_sampled2$sampWeightSS1,df1_sampled2$SAspeciesCodeFAO, sum)
res_repl[is.na(res_repl)]<-0
res<-rbind(res,res_repl)

res_repl<-tapply(df1_sampled2$sampWeightSS1*df1_sampled2$SAtotalWeightLive*df1_sampled2$sampWeightLE,df1_sampled2$SAspeciesCodeFAO, sum)
res_repl[is.na(res_repl)]<-0
res_wt<-rbind(res_wt,res_repl)

}

sim_res<-round(apply(res, 2, sum)/nsims,2); sim_res
true_res<-round(table(df1$SAspeciesCodeFAO[df1$SAspeciesCodeFAO %in% spp_list])/length(unique(df1$LEid))*100,2); true_res[spp_list]
sim_res-true_res[spp_list]

sim_res_wt<-round(apply(res_wt, 2, sum)/nsims,2); sim_res_wt<-round(sim_res_wt/10^6,3); sim_res_wt
true_res_wt<-round(tapply(df1$SAtotalWeightLive, df1$SAspeciesCodeFAO, sum)/10^6,3)[spp_list]; true_res_wt
round((sim_res_wt[true_res_wt!=0]-true_res_wt[true_res_wt!=0])/true_res_wt[true_res_wt!=0]*100,2)
