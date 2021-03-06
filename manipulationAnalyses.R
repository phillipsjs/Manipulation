#### Author and title program ####
#######   Authors: Phillips, J. & Shaw, A.       ###
#######   Title: Manipulating Morality           ###
#######   Contact: jonathan.phillips@yale.edu    ###

rm(list=ls())

## Set working directory as the folder 'Manipulation' which contains 
## this R file; All directories are set relative to that folder. 
setwd("####")

source("manipulationSource.R")

##NB: Multiple mediation is calculated in SPSS using  Preacher and Hayes (2008) INDIRECT
##    The SPSS macro for INDIRECT can be downloaded from http://afhayes.com/spss-sas-and-mplus-macros-and-code.html
##    The INDIRECT macro outputs are included in this file for ease of reference

#### Data and Demographics ---------------------------------------------------------------

Study1.data <- read.csv("Study_1/Study1_data.csv")
Study2.data1 <- read.csv("Study_2/Study2_data.csv")
Study2.data2 <- read.csv("Study_2/Study2_data2.csv")
Study3.data <- read.csv("Study_3/Study3_data.csv")
Study4a.data <- read.csv("Study_4/Study4a_data.csv")
Study4b.data <- read.csv("Study_4/Study4b_data.csv")
Study5.data <- read.csv("Study_5/Study5_data.csv")

Study1.data$expt <- "Study 1"
Study2.data1$expt <- "Study 2"
Study2.data2$expt <- "Study 2"
Study3.data$expt <- "Study 3"
Study4a.data$expt <- "Study 4a"
Study4b.data$expt <- "Study 4b"
Study5.data$expt <- "Study 5"

##demographic data
demog.data <- rbind(Study1.data[,c(8:12,14)],
                    Study2.data1[,c(6:10,12)],
                    Study2.data2[,c(6:10,12)],
                    Study3.data[,c(10:15)], 
                    Study4a.data[,c(16:18,20,21,24)],
                    Study4b.data[,c(9:11,13,14,20)],
                    Study5.data[,c(15:17,19:20,23)]
                   )

demog.data$Gender <- factor(c("Male","Female")[demog.data$Gender], exclude=NULL)
demog.data$Ethnicity <- factor(c("Black/African American","Hispanic/Latino","Asian/Pacific Islander",
                                 "Native American/American Indian","White/Caucasian")[demog.data$Ethnicity])
demog.data$Education <- factor(c("Grammar School","Highschool or Equivalent","Vocational/Technical School",
                                 "Some College","College Graduate (4 years)","Master's Degree",
                                 "Doctoral Degree (PhD)","Professional Degree (JD,MD,etc.)","Other")[demog.data$Education])

demog.age <- aggregate(Age~expt, demog.data, FUN=function(x) c(M =mean(x), SD =sd(x)))
demog.gender <- aggregate(Gender~expt, demog.data, FUN=table)
print(cbind(demog.age,demog.gender[,2]))

demog.ethnicity <- aggregate(Ethnicity~expt, demog.data, FUN=table)
demog.education <- aggregate(Education~expt, demog.data, FUN=table)

###

#######                                Study 1                                   ####### 
###

d1 <- Study1.data

d1$Participant <- c(1:length(d1$Consent)) #Assigns each participant a number
d1$Condition <- factor(c("Accidental","Intentional")[d1$Condition+1])
d1$Condition <- factor(d1$Condition, levels= c("Intentional","Accidental"))

# Convert to long form
d1.long <- melt(d1[,c(3,5,6,14,15)],id.var=c("Condition","Participant","expt"))
colnames(d1.long) <- c("Condition","Participant","Experiment","Agent","Blame")
d1.long$Agent <- factor(c("Proximal Agent","Distal Agent")[d1.long$Agent])
d1.long$Agent <- factor(d1.long$Agent, levels=c("Proximal Agent","Distal Agent"))

# 2 (Condition) x 2 (Agent) mixed within/between-subjects ANOVA
d1.anova <- aov(Blame~Condition*Agent + Error(Participant/Agent),data=d1.long)
summary(d1.anova)

#Mean, SD and n for Blame attributions
d1.table <- aggregate(Blame~Condition*Agent,data=d1.long,
                          FUN=function(x) c(M =mean(x), SD =sd(x), n =length(x)))
print(d1.table)

# t-test for effect of condition on *distal* agent blame
attach(d1.long)
var.test(Blame[which(Condition=="Accidental"& Agent=="Distal Agent")],
         Blame[which(Condition=="Intentional"& Agent=="Distal Agent")])

t.test(Blame[which(Condition=="Accidental"& Agent=="Distal Agent")],
       Blame[which(Condition=="Intentional"& Agent=="Distal Agent")])

cohensD(Blame[which(Condition=="Accidental"& Agent=="Distal Agent")],
        Blame[which(Condition=="Intentional"& Agent=="Distal Agent")])

# t-test for effect of condition on *proximal* agent blame
var.test(Blame[which(Condition=="Accidental" & Agent=="Proximal Agent")],
         Blame[which(Condition=="Intentional" & Agent=="Proximal Agent")])

t.test(Blame[which(Condition=="Accidental" & Agent=="Proximal Agent")],
       Blame[which(Condition=="Intentional" & Agent=="Proximal Agent")], var.equal=T)

cohensD(Blame[which(Condition=="Accidental" & Agent=="Proximal Agent")],
        Blame[which(Condition=="Intentional" & Agent=="Proximal Agent")])
detach(d1.long)

## Study 1 Graph
mss.1 <- aggregate(Blame ~ Participant + Condition + Agent, d1.long,mean)
ms.1 <- aggregate(Blame ~  Condition + Agent, mss.1,mean)
ms.1$ci.h <- aggregate(Blame ~  Condition + Agent, mss.1, ci.high)$Blame
ms.1$ci.l <- aggregate(Blame ~  Condition + Agent, mss.1, ci.low)$Blame

d1.graph <- ggplot(ms.1,aes(x=Agent, y=Blame, fill=Condition)) + 
  ylab("Blame Attribution") +
  coord_cartesian(ylim = c(1, 7)) +
  xlab("") +
  scale_fill_manual(values=blackGreyPalette) + 
  geom_bar(position="dodge",stat="identity",width=.75) +
  geom_linerange(aes(ymin=Blame - ms.1$ci.l, ymax=Blame + ms.1$ci.h),
                 position=position_dodge(.75)) +
  theme(
    plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   ,legend.title=element_blank()
   ,legend.text=element_text(size=rel(1.25))
   ,axis.text=element_text(size=rel(1.25))
   ,axis.title=element_text(size=rel(1.25))
    )
d1.graph
#ggsave(file="Study_1/Study1_graph.tiff")


###

#######                                Study 2                                   ####### 
###

d2 <- rbind(Study2.data1,Study2.data2) ## was collected in two batches because mturk slows down when requesting a large number of HITs

d2$Participant <- c(1:length(d2$Consent))
d2$Condition <- factor(c("Consistent","Deviant")[d2$Condition])

# Convert to long form
d2.long <- melt(d2[,c("Condition","Participant","Blame_Prox","Blame_Dist")],
                id.var=c("Condition","Participant"))
colnames(d2.long) <- c("Condition","Participant","Agent","Blame")
d2.long$Agent <- factor(c("Proximal Agent","Distal Agent")[d2.long$Agent])
d2.long$Agent <- factor(d2.long$Agent, levels=c("Proximal Agent","Distal Agent"))

# 2 (Condition) x 2 (Agent) mixed within/between-subjects ANOVA
d2.anova <- aov(Blame~Condition*Agent + Error(Participant/Agent),data=d2.long)
summary(d2.anova)

#Mean, SD and n for Blame attributions
d2.table <- aggregate(Blame~Condition*Agent,data=d2.long,FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d2.table)

attach(d2.long)
# t-test for effect of condition on *proximal* agent blame
var.test(Blame[which(Condition=="Deviant" & Agent=="Proximal Agent")],
       Blame[which(Condition=="Consistent" & Agent=="Proximal Agent")])

t.test(Blame[which(Condition=="Deviant" & Agent=="Proximal Agent")],
       Blame[which(Condition=="Consistent" & Agent=="Proximal Agent")], var.equal=T)

cohensD(Blame[which(Condition=="Deviant" & Agent=="Proximal Agent")],
        Blame[which(Condition=="Consistent" & Agent=="Proximal Agent")])

# t-test for effect of condition on *distal* agent blame
var.test(Blame[which(Condition=="Deviant"& Agent=="Distal Agent")],
       Blame[which(Condition=="Consistent"& Agent=="Distal Agent")])

t.test(Blame[which(Condition=="Deviant"& Agent=="Distal Agent")],
       Blame[which(Condition=="Consistent"& Agent=="Distal Agent")], var.equal=T)

cohensD(Blame[which(Condition=="Deviant" & Agent=="Distal Agent")],
        Blame[which(Condition=="Consistent" & Agent=="Distal Agent")])
detach(d2.long)

# Study 2 Graph 
mss.2 <- aggregate(Blame ~ Participant + Condition + Agent, d2.long,mean)
ms.2 <- aggregate(Blame ~  Condition + Agent, mss.2,mean)
ms.2$ci.h <- aggregate(Blame ~  Condition + Agent, mss.2, ci.high)$Blame
ms.2$ci.l <- aggregate(Blame ~  Condition + Agent, mss.2, ci.low)$Blame
ms.2$n <- aggregate(Participant ~  Condition + Agent, mss.2, n.unique)$Blame

d2.graph <- ggplot(ms.2,aes(x=Agent, y=Blame, fill=Condition)) + 
  ylab("Blame Attribution") +
  coord_cartesian(ylim = c(1, 7)) +
  xlab("") +
  scale_fill_manual(values=blackGreyPalette) + 
  geom_bar(position="dodge",stat="identity",width=.75) +
  geom_linerange(aes(ymin=Blame - ms.2$ci.l, ymax=Blame + ms.2$ci.h),
                 position=position_dodge(.75)) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.2))
    ,axis.text=element_text(size=rel(1.2))
    ,axis.title=element_text(size=rel(1.2))
  )
d2.graph
#ggsave(file="Study_2/Study2_graph.tiff")

####

#######                                Study 3                                   ####### 
####

d3 <- Study3.data

d3$Participant <- c(1:length(d3$Consent))
d3$Condition <- factor(c("Accidental","Intentional")[d3$Condition])
d3$Condition <- factor(d3$Condition, levels= c("Intentional","Accidental"))

#Mean, SD and n for *blame* attributions to *proximal* agent
d3.blame <- aggregate(Blame~Condition,data=d3,FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d3.blame)

# t-test for effect of condition on *proximal* agent blame
attach(d3)
var.test(Blame[which(Condition=="Accidental")],
       Blame[which(Condition=="Intentional")])

t.test(Blame[which(Condition=="Accidental")],
       Blame[which(Condition=="Intentional")], var.equal=T)

cohensD(Blame[which(Condition=="Accidental")],
        Blame[which(Condition=="Intentional")])

#causal scale
d3.causeScale <- d3[,c(5:7)]
alpha(d3.causeScale)
d3$causeScale <- (d3$Cause + d3$Made + d3$Because)/3

#Mean, SD and n for *cause* attributions to *distal* agent
d3.cause <- aggregate(causeScale~Condition,data=d3,FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d3.cause)

# t-test for effect of condition on *proximal* agent blame
var.test(d3$causeScale[which(Condition=="Accidental")],
         d3$causeScale[which(Condition=="Intentional")])

t.test(d3$causeScale[which(Condition=="Accidental")],
       d3$causeScale[which(Condition=="Intentional")], var.equal=T)

cohensD(d3$causeScale[which(Condition=="Accidental")],
        d3$causeScale[which(Condition=="Intentional")])

### Mediation analysis output from SPSS using Preacher and Hayes (2008) INDIRECT
# Dependent, Independent, and Proposed Mediator Variables: 
# DV =   Blame 
# IV =   Condition 
# MEDS = causeScale 
#  
# Sample size 
#         125 
#  
# IV to Mediators (a paths) 
#              Coeff        se         t         p 
# causeSca    1.0421     .2057    5.0674     .0000 
#  
# Direct Effects of Mediators on DV (b paths) 
#              Coeff        se         t         p 
# causeSca    -.2604     .1196   -2.1767     .0314 
#  
# Total Effect of IV on DV (c path) 
#              Coeff        se         t         p 
# Conditio    -.8528     .2769   -3.0793     .0026 
#  
# Direct Effect of IV on DV (c' path) 
#              Coeff        se         t         p 
# Conditio    -.5815     .3000   -1.9385     .0549 
#  
# Model Summary for DV Model 
#       R-sq  Adj R-sq         F       df1       df2         p 
#      .1063     .0916    7.2542    2.0000  122.0000     .0011 
#  
# ***************************************************************** 
#  
#            BOOTSTRAP RESULTS FOR INDIRECT EFFECTS 
#  
# Indirect Effects of IV on DV through Proposed Mediators (ab paths) 
#               Data      Boot      Bias        SE 
# TOTAL       -.2713    -.2787    -.0073     .1550 
# causeSca    -.2713    -.2787    -.0073     .1550 
#  
# Bias Corrected Confidence Intervals 
#              Lower     Upper 
# TOTAL       -.6152    -.0062 
# causeSca    -.6152    -.0062 
#  
# ***************************************************************** 
#  
# Level of Confidence for Confidence Intervals: 
#   95 
#  
# Number of Bootstrap Resamples: 
#   5000

# Study 3 Graph 
## Converting to long form to make the aggregation easier
d3.long <- melt(d3[,c("Condition","Participant","Blame","causeScale")],id.var=c("Condition","Participant"))
colnames(d3.long) <- c("Condition","Participant","Question","Rating")
d3.long$Question <- factor(c("Proximal Agent Blame","Distal Agent Causation")[d3.long$Question])

mss.3 <- aggregate(Rating ~ Participant + Condition + Question, d3.long,mean)
ms.3 <- aggregate(Rating ~  Condition + Question, mss.3,mean)
ms.3$ci.h <- aggregate(Rating ~  Condition + Question, mss.3, ci.high)$Rating
ms.3$ci.l <- aggregate(Rating ~  Condition + Question, mss.3, ci.low)$Rating

d3.graph <- ggplot(ms.3,aes(x=Question, y=Rating, fill=Condition)) + 
  ylab("Agreement Rating") +
  coord_cartesian(ylim = c(1, 7)) +
  xlab("") +
  scale_fill_manual(values=blackGreyPalette) + 
  geom_bar(position="dodge",stat="identity",width=.75) +
  geom_linerange(aes(ymin=Rating - ms.3$ci.l, ymax=Rating + ms.3$ci.h),
                 position=position_dodge(.75)) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.2))
    ,axis.text=element_text(size=rel(1.2))
    ,axis.title=element_text(size=rel(1.2))
  )
d3.graph
#ggsave(file="Study_3/Study3_graph.tiff")

###

######                                 Study 4a                                  #######      
###

d4a <- Study4a.data

d4a$Participant <- c(1:length(d4a$Consent))
d4a$Condition <- factor(c("Accidental","Intentional")[d4a$Condition+1])
d4a$Scenario <- factor(c("Industrial Workers","Mining Collapse","Mother in Law","Orange Riot")[d4a$Scenario])

# 2(Condition) x 4(Scenario) ANOVA on *blame* for the *proximal* agent
d4a.Blame.anova <- aov(Blame~Condition*Scenario, data=d4a)
summary(d4a.Blame.anova)
etaSquared(d4a.Blame.anova)

#Mean, SD and n for proximal agent blame in both conditions, collapsing across scenario
d4a.table1 <- aggregate(Blame~Condition,data=d4a,FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d4a.table1)

#causal scale
d4a.cause <- d4a[,7:9]
alpha(d4a.cause)
d4a$causeScale <- (d4a$Made+d4a$Because+d4a$Cause)/3

# Distal Agent Causation Analyses
d4a.Cause.anova <- aov(causeScale~Condition*Scenario, data=d4a)
summary(d4a.Cause.anova)
etaSquared(d4a.Cause.anova)

#Mean, SD and n for dital agent *causation* in both condition, collapsing across scenario
d4a.Cause.table1 <- aggregate(causeScale~Condition,data=d4a,FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d4a.Cause.table1)

#moral scale
d4a.moral <- d4a[,10:12]
alpha(d4a.moral)
d4a$moralScale <- (d4a$Blame.1+d4a$Wrong+d4a$Bad)/3

# Distal Agent Moral Responsibility Analyses
d4a.Moral.anova <- aov(moralScale~Condition*Scenario, data=d4a)
summary(d4a.Moral.anova)
etaSquared(d4a.Moral.anova)

#Mean, SD and n for dital agent *morality* in both condition, collapsing across scenario
d4a.Moral.table1 <- aggregate(moralScale~Condition,data=d4a,FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d4a.Moral.table1)

# Mediation analysis output from SPSS using Preacher and Hayes (2008) INDIRECT
##Multiple Mediation
# Dependent, Independent, and Proposed Mediator Variables: 
# DV =   Blame_A 
# IV =   Conditio 
# MEDS = Causatio 
#        Moral 
#  
# Sample size 
#         193 
#  
# IV to Mediators (a paths) 
#              Coeff        se         t         p 
# Causatio    1.7599     .2093    8.4072     .0000 
# Moral       2.9504     .1742   16.9343     .0000 
#  
# Direct Effects of Mediators on DV (b paths) 
#              Coeff        se         t         p 
# Causatio    -.4524     .1018   -4.4443     .0000 
# Moral        .1264     .1223    1.0339     .3025 
#  
# Total Effect of IV on DV (c path) 
#              Coeff        se         t         p 
# Conditio    -.9620     .2582   -3.7257     .0003 
#  
# Direct Effect of IV on DV (c' path) 
#              Coeff        se         t         p 
# Conditio    -.5389     .3884   -1.3873     .1670 
#  
# Model Summary for DV Model 
#       R-sq  Adj R-sq         F       df1       df2         p 
#      .1676     .1544   12.6891    3.0000  189.0000     .0000 
#  
# ***************************************************************** 
#  
#            BOOTSTRAP RESULTS FOR INDIRECT EFFECTS 
#  
# Indirect Effects of IV on DV through Proposed Mediators (ab paths) 
#               Data      Boot      Bias        SE 
# TOTAL       -.4231    -.4374    -.0144     .2848 
# Causatio    -.7961    -.7951     .0010     .1930 
# Moral        .3731     .3577    -.0154     .3143 
#  
# Bias Corrected Confidence Intervals 
#              Lower     Upper 
# TOTAL       -.9929     .1024 
# Causatio   -1.2021    -.4447 
# Moral       -.2595     .9625 
#  
# ***************************************************************** 
#  
# Level of Confidence for Confidence Intervals: 
#   95 
#  
# Number of Bootstrap Resamples: 
#   5000 

##Partial correlation (controlling for condition) from SPSS
####      #     #     #     #     #     #       #
##Control Variables		       #Causation	 #Moral #
####      #     #     #     #     #     #       #
##Causation	    Correlation	 #1.000	     #.553  #
##	 Significance (2-tailed) # -         #.000  #
##		                    df #0	         #190   #
####      #     #     #     #     #     #       #
##Moral	        Correlation	 #.553	     #1.000 #
##   Significance (2-tailed) #.000	     # -    #
##		                    df #190        #0     #
####      #     #     #     #     #     #       #

##Or, if you want a quick look at the overall (non-paritial) matrix, here's that 
d4a.corr <- (d4a[,7:12])
corstarsl(d4a.corr)

#Single mediation with only the moral scale
# Dependent, Independent, and Proposed Mediator Variables: 
# DV =   Blame_A 
# IV =   Conditio 
# MEDS = Moral 
#  
# Sample size 
#         193 
#  
# IV to Mediators (a paths) 
#           Coeff        se         t         p 
# Moral    2.9504     .1742   16.9343     .0000 
#  
# Direct Effects of Mediators on DV (b paths) 
#           Coeff        se         t         p 
# Moral    -.1744     .1068   -1.6334     .1040 
#  
# Total Effect of IV on DV (c path) 
#              Coeff        se         t         p 
# Conditio    -.9620     .2582   -3.7257     .0003 
#  
# Direct Effect of IV on DV (c' path) 
#              Coeff        se         t         p 
# Conditio    -.4474     .4066   -1.1004     .2725 
#  
# Model Summary for DV Model 
#       R-sq  Adj R-sq         F       df1       df2         p 
#      .0807     .0710    8.3350    2.0000  190.0000     .0003 
#  
# ***************************************************************** 
#  
#            BOOTSTRAP RESULTS FOR INDIRECT EFFECTS 
#  
# Indirect Effects of IV on DV through Proposed Mediators (ab paths) 
#            Data      Boot      Bias        SE 
# TOTAL    -.5145    -.5320    -.0175     .2966 
# Moral    -.5145    -.5320    -.0175     .2966 
#  
# Bias Corrected Confidence Intervals 
#           Lower     Upper 
# TOTAL   -1.1269     .0450 
# Moral   -1.1269     .0450 
#  
# ***************************************************************** 
#  
# Level of Confidence for Confidence Intervals: 
#   95 
#  
# Number of Bootstrap Resamples: 
#   5000

## The additional analyses can be most easily replicated using the provided SPSS file, but are not replicated here
## as a way of keeping this document manageable. 

##convert to long form
d4a.long <- melt(d4a[,c("Participant","Condition","Scenario","Blame","causeScale","moralScale")],id.var=c("Condition","Participant","Scenario"))
colnames(d4a.long) <- c("Condition","Participant","Scenario","Question","Rating")
d4a.long$Question <- factor(c("Proximal Blame","Distal Causation","Distal Morality")[d4a.long$Question])
d4a.long$Question <- factor(d4a.long$Question, levels=c("Proximal Blame","Distal Causation","Distal Morality"))
d4a.long$Scenario <- factor(c("Scenario 1","Scenario 2","Scenario 3","Scenario 4")[d4a.long$Scenario])
d4a.long$Condition <- factor(d4a.long$Condition, levels=c("Intentional","Accidental"))

mss.4a <- aggregate(Rating ~ Participant + Condition + Scenario + Question, d4a.long,mean)
ms.4a <- aggregate(Rating ~  Condition + Scenario + Question, mss.4a,mean)
ms.4a$ci.h <- aggregate(Rating ~  Condition + Scenario + Question, mss.4a, ci.high)$Rating
ms.4a$ci.l <- aggregate(Rating ~  Condition + Scenario + Question, mss.4a, ci.low)$Rating

d4a.graph <- ggplot(ms.4a,aes(x=Question, y=Rating, fill=Condition)) + 
  ylab("Agreement Rating") +
  coord_cartesian(ylim = c(1, 7)) +
  xlab("") +
  facet_wrap( ~Scenario) +
  scale_fill_manual(values=blackGreyPalette) + 
  geom_bar(position="dodge",stat="identity",width=.75) +
  geom_linerange(aes(ymin=Rating - ms.4a$ci.l, ymax=Rating + ms.4a$ci.h),
                 position=position_dodge(.75)) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.2))
    ,axis.text=element_text(size=rel(1.2))
    ,axis.title=element_text(size=rel(1.2))
  )
d4a.graph
#ggsave(file="Study_4/Study4a_graph.tiff")

###

#######                      Study 4b                              ####### 
###

d4b <- Study4b.data

d4b$Participant <- c(1:length(d4b$Consent))
d4b$Condition <- factor(c("Accidental","Intentional")[d4b$Condition+1])
d4b$Condition <- factor(d4b$Condition, levels= c("Intentional","Accidental"))
d4b$Scenario <- factor(c("Industrial Workers","Mining Collapse","Mother in Law","Orange Riot")[d4b$Scenario])

# 2(Condition) x 4(Scenario) ANOVA on *blame* for the *proximal* agent
d4b.Blame.anova <- aov(Blame~Condition*Scenario, data=d4b)
summary(d4b.Blame.anova)
etaSquared(d4b.Blame.anova)

#Mean, SD and n for proximal agent blame in both conditions, collapsing across scenario
d4b.table1 <- aggregate(Blame~Condition,data=d4b,FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d4b.table1)

# Distal Agent Causation Analyses
d4b.Cause.anova <- aov(Made~Condition*Scenario, data=d4b)
summary(d4b.Cause.anova)
etaSquared(d4b.Cause.anova)

#Mean, SD and n for dital agent *causation* in both condition, collapsing across scenario
d4b.Cause.table1 <- aggregate(Made~Condition,data=d4b,FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d4b.Cause.table1)

# Distal Agent Moral Responsibility Aanalyses
d4b.Moral.anova <- aov(Wrong~Condition*Scenario, data=d4b)
summary(d4b.Moral.anova)
etaSquared(d4b.Moral.anova)

#Mean, SD and n for dital agent *morality* in both condition, collapsing across scenario
d4b.Moral.table1 <- aggregate(Wrong~Condition,data=d4b,FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d4b.Moral.table1)

## VIF analysis
lm1 <- lm(d4b$Blame ~ d4b$Made + d4b$Wrong)
vif(lm1)

# Mediation analysis output from SPSS using Preacher and Hayes (2008) INDIRECT
##Multiple Mediation with Made and Wrong
# Dependent, Independent, and Proposed Mediator Variables: 
# DV =   Blame 
# IV =   Conditio 
# MEDS = Made 
#        Wrong 
#  
# Sample size 
#         405 
#  
# IV to Mediators (a paths) 
#           Coeff        se         t         p 
# Made     1.9996     .1841   10.8606     .0000 
# Wrong    2.3918     .1397   17.1209     .0000 
#  
# Direct Effects of Mediators on DV (b paths) 
#           Coeff        se         t         p 
# Made     -.2456     .0479   -5.1319     .0000 
# Wrong     .0093     .0631     .1468     .8833 
#  
# Total Effect of IV on DV (c path) 
#              Coeff        se         t         p 
# Conditio    -.7252     .1786   -4.0596     .0001 
#  
# Direct Effect of IV on DV (c' path) 
#              Coeff        se         t         p 
# Conditio    -.2562     .2370   -1.0811     .2803 
#  
# Model Summary for DV Model 
#       R-sq  Adj R-sq         F       df1       df2         p 
#      .1002     .0935   14.8835    3.0000  401.0000     .0000 
#  
# ***************************************************************** 
#  
#            BOOTSTRAP RESULTS FOR INDIRECT EFFECTS 
#  
# Indirect Effects of IV on DV through Proposed Mediators (ab paths) 
#            Data      Boot      Bias        SE 
# TOTAL    -.4689    -.4702    -.0013     .1796 
# Made     -.4911    -.4923    -.0012     .1118 
# Wrong     .0221     .0221     .0000     .1559 
#  
# Bias Corrected Confidence Intervals 
#           Lower     Upper 
# TOTAL    -.8233    -.1214 
# Made     -.7299    -.2844 
# Wrong    -.2831     .3350 
#  
# ***************************************************************** 
#  
# Level of Confidence for Confidence Intervals: 
#   95 
#  
# Number of Bootstrap Resamples: 
#   5000 

#Graphs
## Transform data from wide to long form
d4b.long <- melt(d4b[,c("Participant","Condition","Scenario","Blame","Wrong","Made")],id.var=c("Condition","Participant","Scenario"))
colnames(d4b.long) <- c("Condition","Participant","Scenario","Question","Rating")
d4b.long$Question <- factor(c("Proximal Blame","Distal Wrong","Distal Made")[d4b.long$Question])
d4b.long$Question <- factor(d4b.long$Question, levels=c("Proximal Blame","Distal Made","Distal Wrong"))
d4b.long$Scenario <- factor(c("Scenario 1","Scenario 2","Scenario 3","Scenario 4")[d4b.long$Scenario])
d4b.long$Condition <- factor(d4b.long$Condition, levels=c("Intentional","Accidental"))

mss.4b <- aggregate(Rating ~ Participant + Condition + Scenario + Question, d4b.long,mean)
ms.4b <- aggregate(Rating ~  Condition + Scenario + Question, mss.4b,mean)
ms.4b$ci.h <- aggregate(Rating ~  Condition + Scenario + Question, mss.4b, ci.high)$Rating
ms.4b$ci.l <- aggregate(Rating ~  Condition + Scenario + Question, mss.4b, ci.low)$Rating

d4b.graph <- ggplot(ms.4b,aes(x=Question, y=Rating, fill=Condition)) + 
  ylab("Agreement Rating") +
  coord_cartesian(ylim = c(1, 7)) +
  xlab("") +
  facet_wrap( ~Scenario) +
  scale_fill_manual(values=blackGreyPalette) + 
  geom_bar(position="dodge",stat="identity",width=.75) +
  geom_linerange(aes(ymin=Rating - ms.4b$ci.l, ymax=Rating + ms.4b$ci.h),
                 position=position_dodge(.75)) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.2))
    ,axis.text=element_text(size=rel(1.2))
    ,axis.title=element_text(size=rel(1.2))
  )
d4b.graph
#ggsave(file="Study_4/Study4b_graph.tiff")

#######                      Study 5                              ####### 

d5 <- Study5.data 

d5$Participant <- c(1:length(d5$Consent))
d5$Condition <- factor(c("Accidental","Intentional")[d5$Condition+1])
d5$Scenario <- factor(c("Mother in Law","Mining Collapse","Industrial Workers","Orange Riot")[d5$Scenario])
d5$causeQuestion <- factor(c("Situation Caused","Agent Made")[d5$causeQuestion+1])

# 2(Condition) x 4(Scenario) ANOVA on *blame* for the *proximal* agent
d5.Blame.anova <- aov(Blame~Condition*Scenario, data=d5)
summary(d5.Blame.anova)
etaSquared(d5.Blame.anova)

#Mean, SD and n for proximal agent blame in both conditions, collapsing across scenario
d5.table1 <- aggregate(Blame~Condition,data=d5,FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d5.table1)

# 2(Condition) x 4(Scenario) ANOVA on *made* judgments for the *distal* agent
d5.Made.anova <- aov(Cause~Condition*Scenario, data=d5[which(d5$causeQuestion=="Agent Made"),])
summary(d5.Made.anova)
etaSquared(d5.Made.anova)

#Mean, SD and n for dital agent *made* judgments for the *distal* agent, collapsing across scenario
d5.Made.table1 <- aggregate(Cause~Condition,data=d5[which(d5$causeQuestion=="Agent Made"),],FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d5.Made.table1)

# 2(Condition) x 4(Scenario) ANOVA on *caused* judgments for the *situation*
d5.Caused.anova <- aov(Cause~Condition*Scenario, data=d5[which(d5$causeQuestion=="Situation Caused"),])
summary(d5.Caused.anova)
etaSquared(d5.Caused.anova)

#Mean, SD and n for dital agent *caused* judgments for the *situation*, collapsing across scenario
d5.Caused.table1 <- aggregate(Cause~Condition,data=d5[which(d5$causeQuestion=="Situation Caused"),],FUN=function(x) c(M =mean(x), SD =sd(x), n = length(x)))
print(d5.Caused.table1)

### Mediations from SPSS
## Agent Made ##
# Dependent, Independent, and Proposed Mediator Variables: 
# DV =   blameDV 
# IV =   condition 
# MEDS = Cause 
#  
# Sample size 
#         152 
#  
# IV to Mediators (a paths) 
#           Coeff        se         t         p 
# Cause    1.7590     .2975    5.9127     .0000 
#  
# Direct Effects of Mediators on DV (b paths) 
#           Coeff        se         t         p 
# Cause    -.2251     .0840   -2.6810     .0082 
#  
# Total Effect of IV on DV (c path) 
#              Coeff        se         t         p 
# conditio    -.7487     .3122   -2.3981     .0177 
#  
# Direct Effect of IV on DV (c' path) 
#              Coeff        se         t         p 
# conditio    -.3527     .3398   -1.0381     .3009 
#  
# Model Summary for DV Model 
#       R-sq  Adj R-sq         F       df1       df2         p 
#      .0812     .0689    6.5878    2.0000  149.0000     .0018 
#  
# ***************************************************************** 
#  
#            BOOTSTRAP RESULTS FOR INDIRECT EFFECTS 
#  
# Indirect Effects of IV on DV through Proposed Mediators (ab paths) 
#            Data      Boot      Bias        SE 
# TOTAL    -.3960    -.3992    -.0032     .1674 
# Cause    -.3960    -.3992    -.0032     .1674 
#  
# Bias Corrected Confidence Intervals 
#           Lower     Upper 
# TOTAL    -.8046    -.1113 
# Cause    -.8046    -.1113 
#  
# ***************************************************************** 
#  
# Level of Confidence for Confidence Intervals: 
#   95 
#  
# Number of Bootstrap Resamples: 
#   1000 

## Situation Caused ##
# Dependent, Independent, and Proposed Mediator Variables: 
# DV =   blameDV 
# IV =   conditio 
# MEDS = Cause 
#  
# Sample size 
#         152
# IV to Mediators (a paths) 
#           Coeff        se         t         p 
# Cause     .1952     .2107     .9264     .3557 
#  
# Direct Effects of Mediators on DV (b paths) 
#           Coeff        se         t         p 
# Cause    -.1469     .1130   -1.2996     .1958 
#  
# Total Effect of IV on DV (c path) 
#              Coeff        se         t         p 
# conditio   -1.2009     .2885   -4.1630     .0001 
#  
# Direct Effect of IV on DV (c' path) 
#              Coeff        se         t         p 
# conditio   -1.1722     .2886   -4.0613     .0001 
#  
# Model Summary for DV Model 
#       R-sq  Adj R-sq         F       df1       df2         p 
#      .1164     .1042    9.5508    2.0000  145.0000     .0001 
#  
# ***************************************************************** 
#  
#            BOOTSTRAP RESULTS FOR INDIRECT EFFECTS 
#  
# Indirect Effects of IV on DV through Proposed Mediators (ab paths) 
#            Data      Boot      Bias        SE 
# TOTAL    -.0287    -.0347    -.0060     .0483 
# Cause    -.0287    -.0347    -.0060     .0483 
#  
# Bias Corrected Confidence Intervals 
#           Lower     Upper 
# TOTAL    -.1957     .0250 
# Cause    -.1957     .0250 
#  
# ***************************************************************** 
#  
# Level of Confidence for Confidence Intervals: 
#   95 
#  
# Number of Bootstrap Resamples: 
#   1000 
# 

#Graphs
d5$Scenario <- factor(c("Scenario 1","Scenario 2","Scenario 3","Scenario 4")[d5$Scenario])
d5$Condition <- factor(d5$Condition, levels=c("Intentional","Accidental"))
d5$causeQuestion <- factor(c("Distal Made","Situation Caused")[d5$causeQuestion])
##Blame graph
mss.5.1 <- aggregate(Blame ~ Participant + Condition, d5,mean)
ms.5.1 <- aggregate(Blame ~  Condition, mss.5.1,mean)
ms.5.1$ci.h <- aggregate(Blame ~  Condition, mss.5.1, ci.high)$Blame
ms.5.1$ci.l <- aggregate(Blame ~  Condition, mss.5.1, ci.low)$Blame

d5.1.graph <- ggplot(ms.5.1,aes(x=Condition, y=Blame, fill=Condition)) + 
  ylab("Agreement Rating") +
  coord_cartesian(ylim = c(1, 7)) +
  xlab("Blame Attribution") +
  scale_fill_manual(values=blackGreyPalette) + 
  geom_bar(position="dodge",stat="identity") +
  geom_linerange(aes(ymin=Blame - ms.5.1$ci.l, ymax=Blame + ms.5.1$ci.h),
                 position=position_dodge(1)) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.text.x = element_blank()
    ,legend.position = "none"
    ,axis.text=element_text(size=rel(1.2))
    ,axis.title=element_text(size=rel(1.2))
  )

##Cause graph
mss.5.2 <- aggregate(Cause ~ Participant + Condition + causeQuestion, d5,mean)
ms.5.2 <- aggregate(Cause ~  Condition + causeQuestion, mss.5.2,mean)
ms.5.2$ci.h <- aggregate(Cause ~  Condition + causeQuestion, mss.5.2, ci.high)$Cause
ms.5.2$ci.l <- aggregate(Cause ~  Condition + causeQuestion, mss.5.2, ci.low)$Cause

d5.2.graph <- ggplot(ms.5.2,aes(x=causeQuestion, y=Cause, fill=Condition)) + 
  coord_cartesian(ylim = c(1, 7)) +
  scale_fill_manual(values=blackGreyPalette) + 
  geom_bar(position="dodge",stat="identity") +
  geom_linerange(aes(ymin=Cause - ms.5.2$ci.l, ymax=Cause + ms.5.2$ci.h),
                 position=position_dodge(.9)) +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.2))
    ,axis.title=element_blank()
    ,axis.text=element_text(size=rel(1.2))
    ,axis.title=element_text(size=rel(1.2))
  )

jpeg(filename = "Figures/Figure_9.jpg", width=836, height=623, pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE)
multiplot(d5.1.graph , d5.2.graph, layout=matrix(c(1,1,2,2,2,2),1,6))
dev.off()
