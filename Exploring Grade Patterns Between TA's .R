#load packages
if (!require("stats", "psych", "agricolae"))install.packages("stats", "psych", "agricolae", repos = 'http://cran.us.r-project.org')
library("stats")
library("psych")
library("agricolae")

#Load data
scores <- read.csv("your file path here.csv") #save student scores as csv file (rows = Students, columns = Score, TA)
scores$TA <- factor(scores$TA) #if your TA identifiers are numeric, this will make them factors

#Get a summary of each groups's mean and variance 
group.summary <- describeBy(x = scores$Score, group = scores$TA, mat=TRUE) #"mat=TRUE" creates a data frame
print(group.summary)

#Make a boxplot to visualize your data and get a sense of the difference between TA's
boxplot(Score ~ TA, data = scores)

#Use a simple ANOVA to compare groups of students graded by the same TA
model.TA <- aov(Score ~ TA, data= scores)

#Checking ANOVA assumptions and possible problem spots

plot(model.TA,1) #Homogeneity of variance - do some score groups vary more widely than others?  
plot(model.TA,2) #Normality of score distribution -  check whether the data are normally distributed. 

#ANOVA is fairly robust to violations of homogeneity of variance. Most likely you can use Plot 1 for informational
#purposes only, to help you understand differences in variance between groups.

#If scores are not normal (likely in course grading!)
skewness(scores$Score) #Check existing skewness of score distribution
scores$Score.log10 <- log10(scores$Score) #Use a transformation of your choosing (log10 is used here)
skewness(scores$Score.log10) #Check the skewness again - if improved, use 
                                          #this transformed value as your dependent value in the ANOVA

#Run a revised ANOVA using the transformed dependent variable, if you wish.
model.TA <- aov(Score.log10 ~ TA, data= scores)

#Remember that TA's are still confounded with student groups - you can't interpret a TA effect on 
#its own at this point.

#If significant p-value, you can continue with post-hoc analysis to get a more detailed picture of mean differences.
print(TukeyHSD(x = model.TA))

#Remember, group differences could be due to TA's OR other factors like lab section, lab time, student schedules, etc. 
#We don't know and can't interpret this - it's only for inforamtional purposes to help understand patterns in the data.
