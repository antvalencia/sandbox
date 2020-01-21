library(plyr)
library(stats)
library(base)
library(dplyr)


setwd("/home/kia/Dropbox/Teaching/BU-Teaching/BU-CS555-Data-Analytics-Class/New-Assignments/Assignment-Solutions/")

# Load the smoker data set. 
data<- read.csv("dataAssignment5.csv")
attach(data)
# print a small part of the data 
head(data)

# (1) How many students are in each group?  Summarize the data relating to both test score and age by the student group (separately).
# Use appropriate numerical and/or graphical summaries.  – 3 points

is.factor(data$group)

data$group<-as.factor(data$group)
summary(data$group)
summary(data$group)/nrow(data)*100



ddply(data, "group", summarise, N=length(iq) , mean = mean(iq), sd=sd(iq))

boxplot(iq~group, data=data , main="IQ by student type",  xlab=" ", ylab="IQ")


ddply(data, "group", summarise, N=length(age) , mean = mean(age), sd=sd(age))


# (2) Do the test scores vary by student group?  Perform a one way ANOVA using the aov or Anova function in R to assess.  
# Summarize the results using the 5 step procedure.  If the results of the overall model are significant, 
# perform the appropriate pairwise comparisons using Tukey’s procedure to adjust for multiple comparisons 
# and summarize these results. – 7 points 


# H_0 is that mean of all group are the same 
# H_1 at least one mean that is different 

m<-aov(iq~group, data=data)
summary(m)


pairwise.t.test(iq, group, p.adjust.method = "none")
pairwise.t.test(iq, group, p.adjust.method = "bonferroni")
TukeyHSD(m, conf.level = 0.95)

TukeyHSD(m)


# (3) Create an appropriate number of dummy variables for student group and re-run the one-way ANOVA using 
# the lm function with the newly created dummy variables.  Set chemistry students as the reference group. 
# Confirm if the results are the same.  What is the interpretation of the beta estimates from the regression model? – 4 points

data$p<-ifelse(data$group =="Physics student" , 1,0 )
data$m<-ifelse(data$group =="Math student" , 1,0 )

# We use Chemistry as reference group 
m2<- lm(iq~p+m, data=data) 
summary(m2)


# The global F test can match with the previous one, but not the other results



# (4) Re-do the one-way ANOVA adjusting for age.  
# Focus on the output relating to the comparisons of test score by student type.  
# Explain how this analysis differs from the analysis in step 2 above (not the results
# but how does this analysis differ in terms of the questions it answers as opposed to the one above). 
# Did you obtain different results?  Summarize briefly (no need to go through the 5 –step procedure here).   
# Present the least square means and interpret these. – 6 points
library(car)

# install.packages("emmeans")
library(lsmeans) # is depricated 
library(estimability)
library(emmeans)

options(contrasts =c("contr.treatment","contr.poly") )

m3<-lm(iq~group+age, data=data)
summary(m3)


# after adjusting for age we see the group is not siginificat here 
# And age is drving here 

Anova(lm(iq~group+age, data=data), type=3)

lsmeans(m3, pairwise~group, adjust="Tukey" )

lsmeans(lm(iq ~ group+age, data=data), pairwise~group, adjust="Tukey" )

str(data)




