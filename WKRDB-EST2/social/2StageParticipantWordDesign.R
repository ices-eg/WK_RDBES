PSU<-read.table("Participants.txt", sep="\t")
SSU<-read.table("google-10000-english-usa-no-swears-long.txt")
colnames(SSU)<-"Word"
colnames(PSU)<-"Participant"
SSU$Word<-as.character(SSU$Word)
PSU$Participant<-as.character(PSU$Participant)

PSU<-PSU[PSU$Participant %in% c("Johnathan Ball","David Currie","Kirsten Birch HÃ¥kansson","Edvin Fuglebakk","Nuno Prista","Marta Suska"),]

for (i in 1:2)
{
print("===")
print(sample(PSU, size=1, replace=FALSE))
print("========")
print(sample(SSU$Word, size = 2, replace=FALSE))
}