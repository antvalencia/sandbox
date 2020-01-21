library(plyr)
setwd("/Users/anthony.valencia/met/cs555/assignments/05/")
students <- read.csv("students.csv")

# (1)	How many students are in each group?  Summarize the data relating to both test
# score and age by the student group (separately).  Use appropriate numerical and/or
# graphical summaries.  (3 points)
table(students$group)
# Chemistry: 15
# Math: 15
# Physics: 15

boxplot(iq~group, data=students, main="Students IQ", xlab="major", ylab="IQ")
tapply(students$iq, students$group, summary)
# Chemistry
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 40.00   44.00   46.00   46.27   48.00   52.00 
#
# Math
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 24.0    36.0    38.0    37.6    40.5    45.0 
#
# Physics
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 25.00   31.50   34.00   34.13   37.50   42.00 

boxplot(age~group, data=students, main="Students Age", xlab="major", ylab="age")
tapply(students$age, students$group, summary)
# Chemistry
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 32.00   38.00   41.00   40.07   43.00   46.00 
# 
# Math
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 16.00   19.00   20.00   20.73   22.50   28.00 
# 
# Physics
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.00   16.00   17.00   17.13   18.50   20.00 


# (2)	Do the test scores vary by student group?  Perform a one way ANOVA using the
# aov or Anova function in R to assess.  Summarize the results using the 5 step
# procedure.  If the results of the overall model are significant, perform the
# appropriate pairwise comparisons using Tukey’s procedure to adjust for multiple
# comparisons and summarize these results.  (7 points)
n <- nrow(students)
k <- ncol(students)
a <- 0.05
# 1 (Set up hypothesis and select alpha level)
# h0: uc = um = up (all underlying population IQ score means are equal)
# h1: ui != uj for some i and j  (not all of the underlying population IQ score means are equal)
#
# 2 (Select appropriate test statistic)
# F = MSB/MSW with k-1=2 and n-k=42 degrees of freedom
df1 <- k-1
df2 <- n-k
#
# 3 (State the decision rule)
# F-distribution with 2,42 degrees of freedom and associated with a=0.05
qf(1-a, df1=df1, df2=df2)
# F(2,42,0.05)=3.219942
# Decision Rule: Reject h0 if F>=3.219942
# Otherwise, do not reject h0
#
# 4 (Compute the test statistic)
x.bar <- sum(students$iq)/length(students$iq)
students.math <- students[students$group=='Math student', ]
students.math.n <- nrow(students.math)
students.math.iq <- students.math$iq
x.math.bar <- sum(students.math.iq)/students.math.n
students.math.iq.sqrd <- students.math.n*((x.math.bar-x.bar)^2)
students.chemistry <- students[students$group=='Chemistry student', ]
students.chemistry.n <- nrow(students.chemistry)
students.chemistry.iq <- students.chemistry$iq
x.chemistry.bar <- sum(students.chemistry.iq)/students.chemistry.n
students.chemistry.iq.sqrd <- students.chemistry.n*((x.chemistry.bar-x.bar)^2)
students.physics <- students[students$group=='Physics student', ]
students.physics.n <- nrow(students.physics)
students.physics.iq <- students.physics$iq
x.physics.bar <- sum(students.physics.iq)/students.physics.n
students.physics.iq.sqrd <- students.physics.n*((x.physics.bar-x.bar)^2)

ssb <- sum(students.math.iq.sqrd) + 
  sum(students.chemistry.iq.sqrd) + 
  sum(students.physics.iq.sqrd)
msb <- ssb/df1
# msb=585.8667
ssw <- sum((students.math.iq-x.math.bar)^2) + 
  sum((students.chemistry.iq-x.chemistry.bar)^2) + 
  sum((students.physics.iq-x.physics.bar)^2)
msw <- ssw/df2
# msw=22.05397

f <- msb/msw
f
# f=26.56514
m <- aov(students$iq~students$group, data=students)
summary(m)
#
# 5 (Conclusion)
# reject h0 (all underlying population IQ score means are equal) since 26.56514>=3.219942.
# Therefore, the test is significant.

is.factor(students$group)
aggregate(students$iq, by=list(students$group), summary)
aggregate(students$iq, by=list(students$group), sd)
boxplot(students$iq~students$group, data=students, main="IQ by Major")
pairwise.t.test(students$iq, students$group, p.adj="bonferroni")
# Chemistry/Math: 2.7e-05
# Math/Physics: 0.15
# Chemistry/Physics: 3.4e-08                
  TukeyHSD(m)
  #                                     diff        lwr        upr     p adj
  # Math student-Chemistry student     -8.666667 -12.832756 -4.5005778 0.0000262
  # Physics student-Chemistry student -12.133333 -16.299422 -7.9672445 0.0000000
  # Physics student-Math student       -3.466667  -7.632756  0.6994222 0.1194835

# (3)	Create an appropriate number of dummy variables for student group and re-run
# the one-way ANOVA using the lm function with the newly created dummy variables.
# Set chemistry students as the reference group.  Confirm if the results are the same.
# What is the interpretation of the beta estimates from the regression model?  (4 points)
students$dummy.m <- ifelse(students$group=='Math student',1,0)
students$dummy.c <- ifelse(students$group=='Chemistry student',1,0)
students$dummy.p <- ifelse(students$group=='Physics student',1,0)
m.c <- lm(iq~dummy.m+dummy.p, data=students)
summary(m.c)
#l:
#  lm(formula = iq ~ dummy.m + dummy.p, data = students)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-13.6000  -2.1333  -0.1333   2.7333   7.8667 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     46.267      1.213  38.157  < 2e-16 ***
#  dummy.m       -8.667      1.715  -5.054 8.93e-06 ***
#  dummy.p      -12.133      1.715  -7.076 1.13e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.696 on 42 degrees of freedom
#Multiple R-squared:  0.5585,	Adjusted R-squared:  0.5375 
#F-statistic: 26.57 on 2 and 42 DF,  p-value: 3.496e-08
# f-statistic is 26.57 vs 26.56514 as calculated above. So, results are the same.
# y_hat = 46.267 - 8.667*group.m - 12.133*group.p
# math group performs negatively (-8.667) with respect to reference (chemistry) group
# physics group performs even more negatively (-12.133) than math group with respect to reference (chemistry) group


# (4)	Re-do the one-way ANOVA adjusting for age.   Focus on the output relating to the
# comparisons of test score by student type.  Explain how this analysis differs from the
# analysis in step 2 above (not the results but how does this analysis differ in terms
# of the questions it answers as opposed to the one above).  Did you obtain different
# results?  Summarize briefly (no need to go through the 5 –step procedure here).
# Present the least square means and interpret these. (6 points)
# install.packages('car')
library(car)
Anova(lm(students$iq~students$group+students$age), type=3)
# Anova Table (Type III tests)
#
# Response: students$iq
#                Sum Sq Df F value   Pr(>F)   
# (Intercept)    152.74  1  7.8294 0.007797 **
# students$group  21.89  2  0.5610 0.574969   
# students$age   126.42  1  6.4804 0.014763 * 
# Residuals      799.84 41                    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# install.packages('lsmeans')
library(lsmeans)
options(contrasts=c("contr.treatment", "contr.poly"))
lsmeans(lm(students$iq~students$group+students$age), pairwise~students$group, adjust="none")
# $lsmeans
# students$group      lsmean       SE df lower.CL upper.CL
# Chemistry student 32.96513 1.229289 41 30.48253 35.44773
# Math student      34.06032 1.140782 41 31.75646 36.36418
# Physics student   32.96513 1.229289 41 30.48253 35.44773
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                             estimate        SE df t.ratio p.value
# Chemistry student - Math student    -1.095193 0.4302201 41  -2.546  0.0148
# Chemistry student - Physics student  0.000000 0.0000000 41     NaN     NaN
# Math student - Physics student       1.095193 0.4302201 41   2.546  0.0148

m <- aov(students$iq~students$group, data=students)
summary(m)