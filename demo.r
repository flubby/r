#This is the descriptive analysis for the BAPQ paper
#Copyright (C) 2013 Frank Jackson
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#See <http://www.gnu.org/licenses/> for access to the GNU General Public License

main<-read.csv("georgedata.csv",header=T,sep=",")
attach(main)
main$srs60[ SRS.Total.Raw.Score>=60]<-1
main$srs60[ SRS.Total.Raw.Score<60]<-0
main$srsT[ SRS.Total.T.Score>=60]<-1
main$srsT[ SRS.Total.T.Score>75]<-2
main$srsT[ SRS.Total.T.Score<60]<-0
main$css[ADOS.CSS>10]<-1
main$css[ADOS.CSS<=10]<-0
detach(main)
print("Ages")
mean(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd" ,]$Age.at.Evaluation)
sd(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd" ,]$Age.at.Evaluation)
mean(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$Age.at.Evaluation)
sd(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$Age.at.Evaluation)

print("Genders")

ftable(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$Sex)
ftable(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$Sex)
1

print("SRS T")
ftable(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$srsT)
ftable(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$srsT)


print("BAPQ")
print("BAPQ + SRS")
print("BAPQ * SRS")
print("BAPQ mean median range SD")
print("SRS mean median range SD")
mean(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$SRS.Total.T.Score,na.rm=T)
sd(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$SRS.Total.T.Score,na.rm=T)
summary(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$SRS.Total.T.Score,na.rm=T)
mean(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$SRS.Total.T.Score,na.rm=T)
sd(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$SRS.Total.T.Score,na.rm=T)
summary(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$SRS.Total.T.Score,na.rm=T)
print("FS IQ")
print("Non Verbal IQ")
mean(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$NVIQ,na.rm=T)
sd(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$NVIQ,na.rm=T)
summary(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$NVIQ,na.rm=T)
mean(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd",]$NVIQ,na.rm=T)
sd(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$NVIQ,na.rm=T)
summary(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$NVIQ,na.rm=T)

print("Verbal IQ")
mean(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$VIQ,na.rm=T)
sd(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$VIQ,na.rm=T)
summary(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$VIQ,na.rm=T)
mean(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd",]$VIQ,na.rm=T)
sd(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$VIQ,na.rm=T)
summary(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$VIQ,na.rm=T)

print("CBCL internalizing")
mean(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$CBCL.Internalizing.Problems.T.Score,na.rm=T)
sd(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$CBCL.Internalizing.Problems.T.Score,na.rm=T)
summary(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$CBCL.Internalizing.Problems.T.Score,na.rm=T)
mean(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd",]$CBCL.Internalizing.Problems.T.Score,na.rm=T)
sd(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$CBCL.Internalizing.Problems.T.Score,na.rm=T)
summary(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$CBCL.Internalizing.Problems.T.Score,na.rm=T)

print("CBCL Externalizing")
mean(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd"  ,]$CBCL.Externalizing.Problems.T.Score,na.rm=T)
sd(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd" ,]$CBCL.Externalizing.Problems.T.Score,na.rm=T)
summary(main[main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd" ,]$CBCL.Externalizing.Problems.T.Score,na.rm=T)
mean(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$CBCL.Externalizing.Problems.T.Score,na.rm=T)
sd(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$CBCL.Externalizing.Problems.T.Score,na.rm=T)
summary(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$CBCL.Externalizing.Problems.T.Score,na.rm=T)
print("VABS Socialization Scores")
print("CSS")
mean(main[(main$css==0 )&(main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd") ,]$ADOS.CSS,na.rm=T)
sd(main[(main$css==0 )&(main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd") ,]$ADOS.CSS,na.rm=T)
summary(main[(main$css==0 )&(main$Research.Dx=="1-autism"|main$Research.Dx=="4-aspergers"|main$Research.Dx=="2-pdd") ,]$ADOS.CSS,na.rm=T)
mean(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$ADOS.CSS,na.rm=T)
sd(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$ADOS.CSS,na.rm=T)
summary(main[main$Research.Dx!="1-autism"&main$Research.Dx!="4-aspergers"&main$Research.Dx!="2-pdd" ,]$ADOS.CSS,na.rm=T)


