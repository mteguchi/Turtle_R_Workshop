#Intro:####
#setwd('~/Dropbox/home-NOAAtransitionfiles/MMTD_R')#change accordingly
getwd()#are you where you think you are?
list.files()#I run this sometimes just to check that what I think is there is there, copy and paste the data input file names, etc.
library(ggplot2);library(dplyr); library(tidyr)#these are the libraries called below, some may be redudant with previously run code

#Some genetics graphing applications:####

#1 Haplotype Allelic Frequency bar and pie charts####
#generate data to play with
set.seed(8) #set the seed number for the random number generator so results are reproducible
hap<-c('CcP1.1','CcP2.1','CcP2.2','CcP2.3') #create list of haplotypes
data<-data.frame(
  location=rep(c('Mainland MU', 'Yakushima MU','Ryukyu MU','Muroto bycatch'),each=10), #repeat command, create a list of site names, each 10 times 
  id=seq(1:40), #create sequence of numbers which identifies the samples
  haplotype=sample(hap,40, replace=T) #randomly pick from above list, 40 times
)
str(data) #examine structure of dataframe

#summarize data
summary(data) #summary stats of dataframe
summary.data<-count(data, location, haplotype, sort=T) #count is a function in dplyr to create a frequency table of our data
str(summary.data)

#another way that uses layered functions & adds a proportional variable
summary.data2<-data %>%
  group_by(location,haplotype) %>% #define groups
  summarize(n=n()) %>% #summarize counts the occurrences
  mutate(freq=n/sum(n)) #mutate creates a new variable that we call 'freq' that is the proportion given each group

#stacked bar graphs
ggplot(summary.data,aes(x=location,y=n, fill=haplotype))+geom_bar(stat='identity') #counts
ggplot(summary.data2,aes(x=location,y=freq, fill=haplotype))+geom_bar(stat='identity')#proportions

a<-ggplot(summary.data2,aes(x=location,y=freq, fill=haplotype))+geom_bar(stat='identity')#proportions
b<-a+theme_bw()+scale_fill_manual(values=c("blue","forestgreen","grey70","purple"))+
                  theme(axis.title.x = element_text(face="bold", colour="black", size=20),
                  axis.text.x  = element_text(angle=30, vjust=0.5, size=14),
                  axis.title.y = element_text(face="bold", colour="black", size=20),
                  axis.text.y  = element_text(angle=90, vjust=0.5, size=14))+
                  guides(fill=guide_legend(title="Haplotype"))+xlab("Location")+ylab("Proportion")
b
#Pie charts
ggplot(summary.data2,aes(x=location,y=freq, fill=haplotype))+geom_bar(stat='identity')+
  coord_polar(theta = "y", start=0)
#ggplot doesn't do pie charts very well (I think that is a decision they made intentionally)
p<-ggplot(summary.data2,aes(x=1,y=freq,fill=haplotype))+geom_bar(stat="identity")+facet_grid(.~location)+coord_polar(theta='y')
p <- p +
  geom_bar(stat="identity", color='black')+  # black border around pie slices
  guides(fill=guide_legend(override.aes=list(colour=NA)))+ # remove black diagonal line from legend
  theme(axis.ticks=element_blank(),  # the axis ticks
        axis.title=element_blank(),  # the axis labels
        axis.text.y=element_blank()) # the 0.75, 1.00, 1.25 labels.
p#can play around with formatting, etc. to get #s, labels, colors how you want

#Note, can also use base plotting in R to make Pie Charts, I have code for this if people want (can go over separately another day)

###now using some realish data-(for afternoon exercise?)
mtdna<-read.csv('data/mtDNA_freq.csv')
str(mtdna) #data is in wide format

mtdna.long<-gather(mtdna,"haplotype","freq", 2:9) #'gather' converts wide to long format

ggplot(mtdna.long,aes(x=Location,y=freq, fill=haplotype))+geom_bar(stat='identity') +
  scale_fill_brewer(palette="Spectral")+theme_bw()#play around with the themes and color scales as you want...

#2 Mixed Stock Analysis Bar Plots ####
#Simple example:
MSA<-read.csv("data/MSA_Ccar.csv")
str(MSA)
bar1<-ggplot(MSA, aes(x=Nesting.Stock,y=Mean,fill=Weight ))  +
  geom_bar(colour="black",stat="identity",position=position_dodge(),width=0.7)+ 
  theme_bw()+  
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=0,position=position_dodge(0.7))+
  scale_fill_manual(values=c("blue","forestgreen"))

bar2<-bar1+ theme(axis.title.x = element_text(face="bold", colour="black", size=20),
                  axis.text.x  = element_text(angle=0, vjust=0.5, size=16),
                  axis.title.y = element_text(face="bold", colour="black", size=20),
                  axis.text.y  = element_text(angle=90, vjust=0.5, size=16),
                  legend.title=element_blank(),legend.position=c(0.07,0.95))+xlab("Nesting Stock")+ylab("Estimated Contribution")
bar2 #change colors, legend positioning etc as desired...

#Now make your own (MJ's data): (for afternoon exercise?)
MJ<-read.csv("data/MJ_models_combined.csv", header=TRUE)
str(MJ)

bar1<-ggplot(MJ, aes(x=STOCK,y=MEAN, fill=Model))  +
  geom_bar(colour="black",stat="identity",position=position_dodge(),width=0.7)+ 
  theme_bw()+  
  geom_errorbar(aes(ymin=MEAN-SD, ymax=MEAN+SD), width=0,position=position_dodge(0.7))+
  scale_fill_manual(values=c("blue","forestgreen"))

bar2<-bar1+ theme(axis.title.x = element_text(face="bold", colour="black", size=18),
                  axis.text.x  = element_text(angle=70, vjust=0.5, size=14),
                  axis.title.y = element_text(face="bold", colour="black", size=18),
                  axis.text.y  = element_text(angle=90, vjust=0.5, size=14),
                  legend.title=element_blank(),legend.position=c(0.9,0.85))+
  xlab("Nesting Stock")+ylab("Estimated Contribution")+coord_cartesian(ylim=c(-0.01,0.78))
bar2

#write to file:
ppi=300 #define a pixels per inch term
png("Figure1.png",width=10*ppi, height=6*ppi, res=ppi) #define your png file
bar2 #create the plot
dev.off() #stop!

#3. MSat Allele Freq graph####
Msat.Ei.geno <- read.csv("data/EiMsatData020915.csv") 
Msat.Ei.strata <- read.csv("data/Ei_Msat_strata.csv") #LABID and population from analysis set.
Msat_Ei<-merge(Msat.Ei.geno,Msat.Ei.strata)
Msat_Ei_subset<-Msat_Ei[,c(52,2:13)]
str(Msat_Ei_subset)
Ei_long<-gather(Msat_Ei_subset,"Locus","allele_ID",2:13)
Ei_long2<-Ei_long%>%
  separate(Locus,into=c("Locus","allele"),sep="_")

hist<-ggplot(Ei_long2,aes(x=allele_ID,fill=Population))+
  geom_histogram()+theme_bw()
hist#this puts all our Locus together-not what we want
hist2<-hist+facet_wrap(~Locus,scales="free")#the scales=free is important when have different ranges among groups (as is the case with Msats, etc.)
quartz(10,10)#this is for Mac, just pops out a big external graphing window-I find this useful when using two monitors; there is an analagous command for PC
hist2

ppi=300 #define a pixels per inch term
png("Figure2.png",width=20*ppi, height=12*ppi, res=ppi) #define your png file
hist2
dev.off()

#4. Bonus####
#show graph of Haplotype Freqs overlaid on map from tutorial from K. Gilbert
#Could be for future workshop as people advance if people are interested
