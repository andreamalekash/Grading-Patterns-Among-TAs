#load packages
if (!require("stats", "psych", "agricolae", "nlme", "emmeans"))install.packages("stats", "psych", "agricolae", "nlme", "emmeans", repos = 'http://cran.us.r-project.org')
library("stats")
library("psych")
library("agricolae")
library("nlme")
library("emmeans")
library("plyr")

#Load data
scores <- read.csv("your data here") #save student scores as csv file (rows = Students, columns = Score, TA)
scores$TA <- factor(scores$TA) #if your TA identifiers are numeric, this will make them factors
scores$TA <- revalue(scores$TA, c("1" = "A", "2" = "B", "3" = "C", "4" = "D", "5" = "E", "6"="F", "7" = "G", "8" = "H", "9" = "I", "10" = "J", "11" = "K", "12" = "L", "13" = "M", "14" = "N", "15" = "O", "16" = "P", "17" = "Q") ) #Changing numbers to characters to represent TA's (optional) 


#Get a summary of each groups's mean and variance 
group.summary <- describeBy(x = scores$Score, group = scores$TA, mat=TRUE) #"mat=TRUE" creates a data frame
print(group.summary)

#Make a boxplot to visualize your data and get a sense of the difference between TA's
boxplot(Score ~ TA, data = scores)

#Use a simple ANOVA to compare groups of students graded by the same TA
model.TA <- aov(Score ~ TA, data= scores)
summary(model.TA)

model.TA <- aov(AveScore ~ TA, data= Spring)
summary(model.TA)

#Checking ANOVA assumptions and possible problem spots
plot(model.TA,1) #Homogeneity of variance - do some score groups vary more widely than others?  
plot(model.TA,2) #Normality of score distribution -  check whether the data are normally distributed. 

#ANOVA is fairly robust to violations of homogeneity of variance. Most likely you can use Plot 1 for informational
#purposes only, to help you understand differences in variance between groups.

#Remember that TA's are still confounded with student groups - you can't interpret a TA effect on 
#its own at this point.

#If significant p-value, you can continue with post-hoc analysis to get a more detailed picture of mean differences.
print(TukeyHSD(x = model.TA))

#Or compare using estimated marginal means
emmeans(ref_grid(model.TA), pairwise~TA, adjust="none")

#Remember, group differences could be due to TA's OR other factors like lab section, lab time, student schedules, etc. 
#We don't know and can't interpret this - it's only for informational purposes to help understand patterns in the data.

####################METHOD 2: Mixed effect modeling##########################
#Load data - this data only includes TA's that had at least two lab sections (1 TA from each semester removed)
Fall <- read.csv("/Users/andreaash/OneDrive - University of Iowa/Projects/Bio Rater Effects/Data/R/R_fall_no_missing_noSydney.csv") #save student scores as csv file (rows = Students, columns = Score, TA)

Fall$TA <- as.factor(Fall$TA) #make factor so we can revalue TA names
Fall$Lab <- as.factor(Fall$Lab)
Fall$TA <- revalue(Fall$TA, c("1" = "A", "2" = "B", "3" = "C", "4" = "D", "5" = "E", "6"="F", "7" = "G", "8" = "H", "9" = "I", "10" = "J", "11" = "K", "12" = "L", "13" = "M", "14" = "N", "15" = "O", "16" = "P", "17" = "Q") ) #Characters are needed to allow us to pick a different TA reference

Spring <- read.csv("/Users/andreaash/OneDrive - University of Iowa/Projects/Bio Rater Effects/Data/R/R_spring_no_missing_noCindy.csv") #save student scores as csv file (rows = Students, columns = Score, TA)
Spring$TA <- as.factor(Spring$TA) #make factor so we can revalue TA names
Spring$Lab <- as.factor(Spring$Lab)
Spring$TA <- revalue(Spring$TA, c("1" = "A", "2" = "B", "3" = "C", "4" = "D", "5" = "E", "6"="F", "7" = "G", "8" = "H", "9" = "I", "10" = "J", "11" = "K", "12" = "L", "13" = "M", "14" = "N", "15" = "O", "16" = "P", "17" = "Q") )

#Creating ICC function to see intraclass correlations with LME object
ICClme <- function(out) {
  varests <- as.numeric(VarCorr(out)[1:2])
  return(paste("ICC =", varests[1]/sum(varests)))
}

#Check ICC's within TA's vs. lab's for each semester
iccSpring_TA <-  lme(Score1 ~ 1, data=Spring, random = ~1|TA, method = "ML")
iccSpring_lab <-  lme(Score1 ~ 1, data=Spring, random = ~1|Lab, method = "ML")
iccFall_TA <-  lme(Score1 ~ 1, data=Fall, random = ~1|TA, method = "ML")
iccFall_lab <-  lme(Score1 ~ 1, data=Fall, random = ~1|Lab, method = "ML")
ICClme(iccSpring_TA)
ICClme(iccSpring_lab)
ICClme(iccFall_TA)
ICClme(iccFall_lab )

#Look at level-specific ICC's for TA vs. Lab when Lab is nested within TA
mod <- lme(Score1~1, data=Spring, random=~1|TA/Lab)
VarCorr(mod)

mod1 <- lme(Score1~1, data=Fall, random=~1|TA/Lab)
VarCorr(mod1)

mode3level <- lme(Score~1, data=Spring_l, random=~1|TA/Lab/Time) #Have to use these to do manual calculations for level specific ICC
VarCorr(mode3level)

#To meaningfully the TA's meaningfully, I want to choose a  TA to be the reference group, one which has a group mean close to the grand mean.
SpringScore1.summary <- describeBy(x=Spring$Score1, group=Spring$TA, mat=TRUE) #I choose TA B for Spring
SpringScore2.summary <- describeBy(x=Spring$Score2, group=Spring$TA, mat=TRUE)
summary(Spring)
summary(Fall)
FallScore1.summary <- describeBy(x=Fall$Score1, group=Fall$TA, mat=TRUE) #I choose TA J for Fall
FallScore2.summary <- describeBy(x=Fall$Score2, group=Fall$TA, mat=TRUE)
SpringScore1.summary
SpringScore2.summary
FallScore1.summary
FallScore2.summary


#Now relevel to define reference group
Spring$TA <- relevel(Spring$TA, ref = "C")
Fall$TA <- relevel(Fall$TA, ref="H")

#Model 1: Lab as level 2 random effect (grouping variable), TA as level 1 fixed effect predictor
modSpring <- lme(AveScore ~ as.factor(TA), data=Spring, random = ~1|Lab, method = "ML")
summary(modSpring)
anova(modSpring)
ICClme(modSpring)
VarCorr(modSpring) #See variance components


modFall <-  lme(AveScore ~ as.factor(TA), data=Fall, random = ~1|Lab, method = "ML")
summary(modFall)
ICClme(modFall)
anova(modFall)
VarCorr(modFall)

#Model 2: Both scores used: Lab as level 3, Student as level 2, TA as fixed effect predictor
#reshape wide to long for Scores
Spring_l <- reshape(Spring, varying = c("Score1", "Score2"), v.names = "Score", timevar = "Time", idvar = "Student", times = c(1,2), direction="long")
Fall_l <- reshape(Fall, varying = c("Score1", "Score2"), v.names = "Score", timevar = "Time", idvar = "Student", times = c(1,2), direction="long")

#Model 2
mod2Spring <- lme(Score ~ as.factor(TA), data=Spring_l, random = ~1|Lab/Student, method = "ML")
summary(mod2Spring)
anova(mod2Spring)
VarCorr(mod2Spring)

mod2Fall <-  lme(AveScore ~ as.factor(TA), data=Fall_l, random = ~1|Lab/Student, method = "ML")
summary(mod2Fall)
anova(mod2Fall)
VarCorr(mod2Fall)

###########################Compare variances from Spring to Fall####################
All <- read.csv("/Users/andreaash/R_all_data_variances.csv")
summary(All)
describeBy(Spring$Score1, Spring$TA)
describeBy(Fall$Score1, Fall$TA)
describeBy(All$Score2, All$Semester)

All$AveScore <- mean(All$Score1,All$Score2, na.rm=TRUE) 

All$TA <- as.factor(All$TA)
All$TA <- revalue(All$TA, c("1" = "A", "2" = "B", "3" = "C", "4" = "D", "5" = "E", "6"="F", "7" = "G", "8" = "H", "9" = "I", "10" = "J", "11" = "K", "12" = "L", "13" = "M", "14" = "N", "15" = "O", "16" = "P", "17" = "Q") )
bartlett.test(Score1~Semester, data=subset(All, TA == "A", select= c(Score1, Semester)))
bartlett.test(Score1~Semester, data=subset(All, TA == "B", select= c(Score1, Semester)))
bartlett.test(Score1~Semester, data=subset(All, TA == "C", select= c(Score1, Semester)))
bartlett.test(Score1~Semester, data=subset(All, TA == "H", select= c(Score1, Semester)))
bartlett.test(Score1~Semester, data=subset(All, TA == "J", select= c(Score1, Semester)))

fligner.test(Score1~Semester, data=subset(All, TA == "A", select= c(Score1, Semester)))
fligner.test(Score1~Semester, data=subset(All, TA == "B", select= c(Score1, Semester)))
fligner.test(Score1~Semester, data=subset(All, TA == "C", select= c(Score1, Semester)))
fligner.test(Score1~Semester, data=subset(All, TA == "H", select= c(Score1, Semester)))
fligner.test(Score1~Semester, data=subset(All, TA == "J", select= c(Score1, Semester)))

fligner.test(Score1~Semester, data=All)
bartlett.test(Score1~Semester, data=All)

######################Plots##################################
All$Semester <- factor(All$Semester,levels = c("Spring", "Fall")) #Change order 
All_l$Semester <- factor(All_l$Semester,levels = c("Spring", "Fall")) #Change order 

#Score 1 by Semester 
ggplot(All, aes(x = Score2, fill = Semester)) +
  geom_density(alpha = .3) +
  labs(x="Assignment 2") #Score 1 by Semester 

#Make a long version of All data
All_l <- reshape(All, varying = c("Score1", "Score2"), v.names = "Score", timevar = "Time", idvar = "Student", times = c(1,2), direction="long")
All_l$Time <- as.factor(All_l$Time)

#Spring Score by Assignment
ggplot(subset(All_l, Semester == "Spring", select= c(Score, Time)), aes(x = Score, fill = Time)) +
  geom_density(alpha = .3) +
  labs(fill="Assignment")
#Fall Score by Assignment
ggplot(subset(All_l, Semester == "Fall", select= c(Score, Time)), aes(x = Score, fill = Time)) +
  geom_density(alpha = .3) +
  labs(fill="Assignment")

#Sort Spring to Fall to get re-ordered key (Spring then Fall)
All_l %>%
  mutate(Semester=fct_relevel(Semester,"Fall","Spring)"))

#Assignment 1 Score by Semester
ggplot(subset(All_l, Time ==1 , select= c(Score, Semester)), aes(x = Score, fill = Semester)) +
  geom_density(alpha = .3) +
  labs(fill="Semester", x="Assignment 1", y = "")

#Assignment 2 Score by Semester
ggplot(subset(All_l, Time == 2, select= c(Score, Semester)), aes(x = Score, fill = Semester)) +
  geom_density(alpha = .3) +
  labs(fill="Semester", x="Assignment 2", y="")

#BOXPLOT Score 1 by semester and by TA
All$AveScore <- All$Score1 + All$Score2

ggplot(All,aes(x = TA, y=AveScore, fill = Semester)) +
  geom_boxplot() +
  labs(fill="Semester", y="Average of Assignments 1 and 2")
##########Mean Adjustments###################################
describeBy(Spring$Score1, Spring$TA)
#Make a table with spring means for each TA
by_TA <- Spring %>% group_by(TA) #grouping by TA first
SpringMeans <-by_TA %>% dplyr::summarize(
  mean1=mean(Score1), mean2 = mean(Score2)
)
#Make a table with fall means for each TA
Fallby_TA <- Fall %>% group_by(TA) #grouping by TA first
FallMeans <-Fallby_TA %>% dplyr::summarize(
  mean1=mean(Score1), mean2 = mean(Score2)
)

#Create new column in "Spring" that include the mean-adjusted score
Spring$Score1Mean=Spring$Score1 #new column for mean
Spring$Score1Adj=Spring$Score1 #new column for adjusted score
Spring$Score2Mean=Spring$Score2 #new column for mean
Spring$Score2Adj=Spring$Score2 #new column for adjusted score

#Take the mean of each TA and put it into the new column
Spring$Score1Mean[which(Spring$TA=="A")]=SpringMeans$mean1[which(SpringMeans$TA=="A")]
Spring$Score1Mean[which(Spring$TA=="B")]=SpringMeans$mean1[which(SpringMeans$TA=="B")]
Spring$Score1Mean[which(Spring$TA=="C")]=SpringMeans$mean1[which(SpringMeans$TA=="C")]
Spring$Score1Mean[which(Spring$TA=="D")]=SpringMeans$mean1[which(SpringMeans$TA=="D")]
Spring$Score1Mean[which(Spring$TA=="E")]=SpringMeans$mean1[which(SpringMeans$TA=="E")]
Spring$Score1Mean[which(Spring$TA=="F")]=SpringMeans$mean1[which(SpringMeans$TA=="F")]
Spring$Score1Mean[which(Spring$TA=="H")]=SpringMeans$mean1[which(SpringMeans$TA=="H")]
Spring$Score1Mean[which(Spring$TA=="I")]=SpringMeans$mean1[which(SpringMeans$TA=="I")]
Spring$Score1Mean[which(Spring$TA=="J")]=SpringMeans$mean1[which(SpringMeans$TA=="J")]
#score2
Spring$Score2Mean[which(Spring$TA=="A")]=SpringMeans$mean2[which(SpringMeans$TA=="A")]
Spring$Score2Mean[which(Spring$TA=="B")]=SpringMeans$mean2[which(SpringMeans$TA=="B")]
Spring$Score2Mean[which(Spring$TA=="C")]=SpringMeans$mean2[which(SpringMeans$TA=="C")]
Spring$Score2Mean[which(Spring$TA=="D")]=SpringMeans$mean2[which(SpringMeans$TA=="D")]
Spring$Score2Mean[which(Spring$TA=="E")]=SpringMeans$mean2[which(SpringMeans$TA=="E")]
Spring$Score2Mean[which(Spring$TA=="F")]=SpringMeans$mean2[which(SpringMeans$TA=="F")]
Spring$Score2Mean[which(Spring$TA=="H")]=SpringMeans$mean2[which(SpringMeans$TA=="H")]
Spring$Score2Mean[which(Spring$TA=="I")]=SpringMeans$mean2[which(SpringMeans$TA=="I")]
Spring$Score2Mean[which(Spring$TA=="J")]=SpringMeans$mean2[which(SpringMeans$TA=="J")]
#Subtract each person's Score1 from that new column and replace the value in the column with the new adjusted score
Spring$Score1Adj <- (Spring$Score1)-(Spring$Score1Mean)
Spring$Score2Adj <- (Spring$Score2)-(Spring$Score2Mean)
#Now add Score1Adj to overall score 1 mean to get the new score
Spring$Score1New <- (Spring$Score1Adj)+(mean(Spring$Score1))
Spring$Score2New <- (Spring$Score2Adj)+(mean(Spring$Score2))

#Create new columns in "Fall" data
Fall$Score1Mean=Fall$Score1 #new column for mean
Fall$Score1Adj=Fall$Score1 #new column for adjusted score
Fall$Score2Mean=Fall$Score2 #new column for mean
Fall$Score2Adj=Fall$Score2 #new column for adjusted score

#Take the mean of each TA and put it into the new column
Fall$Score1Mean[which(Fall$TA=="A")]=FallMeans$mean1[which(FallMeans$TA=="A")]
Fall$Score1Mean[which(Fall$TA=="B")]=FallMeans$mean1[which(FallMeans$TA=="B")]
Fall$Score1Mean[which(Fall$TA=="C")]=FallMeans$mean1[which(FallMeans$TA=="C")]
Fall$Score1Mean[which(Fall$TA=="H")]=FallMeans$mean1[which(FallMeans$TA=="H")]
Fall$Score1Mean[which(Fall$TA=="J")]=FallMeans$mean1[which(FallMeans$TA=="J")]
Fall$Score1Mean[which(Fall$TA=="K")]=FallMeans$mean1[which(FallMeans$TA=="K")]
Fall$Score1Mean[which(Fall$TA=="L")]=FallMeans$mean1[which(FallMeans$TA=="L")]
Fall$Score1Mean[which(Fall$TA=="M")]=FallMeans$mean1[which(FallMeans$TA=="M")]
Fall$Score1Mean[which(Fall$TA=="O")]=FallMeans$mean1[which(FallMeans$TA=="O")]
Fall$Score1Mean[which(Fall$TA=="P")]=FallMeans$mean1[which(FallMeans$TA=="P")]
Fall$Score1Mean[which(Fall$TA=="Q")]=FallMeans$mean1[which(FallMeans$TA=="Q")]
#score2
Fall$Score2Mean[which(Fall$TA=="A")]=FallMeans$mean2[which(FallMeans$TA=="A")]
Fall$Score2Mean[which(Fall$TA=="B")]=FallMeans$mean2[which(FallMeans$TA=="B")]
Fall$Score2Mean[which(Fall$TA=="C")]=FallMeans$mean2[which(FallMeans$TA=="C")]
Fall$Score2Mean[which(Fall$TA=="H")]=FallMeans$mean2[which(FallMeans$TA=="H")]
Fall$Score2Mean[which(Fall$TA=="J")]=FallMeans$mean2[which(FallMeans$TA=="J")]
Fall$Score2Mean[which(Fall$TA=="K")]=FallMeans$mean2[which(FallMeans$TA=="K")]
Fall$Score2Mean[which(Fall$TA=="L")]=FallMeans$mean2[which(FallMeans$TA=="L")]
Fall$Score2Mean[which(Fall$TA=="M")]=FallMeans$mean2[which(FallMeans$TA=="M")]
Fall$Score2Mean[which(Fall$TA=="O")]=FallMeans$mean2[which(FallMeans$TA=="O")]
Fall$Score2Mean[which(Fall$TA=="P")]=FallMeans$mean2[which(FallMeans$TA=="P")]
Fall$Score2Mean[which(Fall$TA=="Q")]=FallMeans$mean2[which(FallMeans$TA=="Q")]
#Subtract each person's Score1 from that new column and replace the value in the column with the new adjusted score
Fall$Score1Adj <- (Fall$Score1)-(Fall$Score1Mean)
Fall$Score2Adj <- (Fall$Score2)-(Fall$Score2Mean)
#Now add Score1Adj to overall score 1 mean to get the new score
Fall$Score1New <- (Fall$Score1Adj)+(mean(Fall$Score1))
Fall$Score2New <- (Fall$Score2Adj)+(mean(Fall$Score2))

#Graphing new adjusted score distributions#################################
Fall$TA <- factor(Fall$TA, levels=c("A","B","C","H","J","K","L","M","O","P","Q"))
Spring$TA <- factor(Spring$TA, levels=c("A","B","C","D","E","F","H","I","J"))
#Putting New scores into All_l
All_l<- All_l[order(All_l$Student),]
Fall<- Fall[order(Fall$Student),]
Spring<- Spring[order(Spring$Student),] #ordering

All_l$Score1New <- All_l$Score
All_l$Score1New[which(All_l$Time==1)]=Spring$Score1New
Fall$Score2Mean[which(Fall$TA=="A")]=FallMeans$mean2[which(FallMeans$TA=="A")]

#Assignment 1 Score by Semester
Fall$TA <- factor(Fall$TA, levels=c("A","B","C","H","J","K","L","M","O","P","Q"))
ggplot(Fall,aes(x=TA, y=Score1New))+
        geom_boxplot() +
  labs(x="TA", y= "Score 1")
#Assignment 1 Score by Semester
Fall$TA <- factor(Fall$TA, levels=c("A","B","C","H","J","K","L","M","O","P","Q"))
ggplot(Fall,aes(x=TA, y=Score2New))+
  geom_boxplot() +
  labs(x="TA", y= "Mean-Adjusted Score 2 (Fall)")
#Assignment 2 Score by Semester
ggplot(subset(All_l, Time == 2, select= c(Score, Semester)), aes(x = Score, fill = Semester)) +
  geom_density(alpha = .3) +
  labs(fill="Semester")
