library(plyr)


# Question 1: read data
setwd("/Users/anthony.valencia/met/cs555/assignments/03/")
fish <- read.csv("data.csv")
fish <- rename(fish, c("Number.of.meals.with.fish"="meals.w.fish", "Total.Mercury.in.mg.g"="total.hg"))


# Question 2: scatter & describe
plot(
  fish$meals.w.fish,
  fish$total.hg,
  main="Meals w/fish vs Hg (mg/g)",
  xlab="meals w/fish",
  ylab="Hg (mg/g)",
  cex=0.2
  )
# explanatory variable (x) is the number of meals with fish, since we hypothesize this will explain the response
# response variable (y) is total mercury (mg/g) which we assume is a response to the aforementioned explanatory variable
# Form: seems to trend upward.  Almost linear, yet the seemingly scattered enough to almost seem random.
# Direction: the points appear to be positively associated.  As the number of meals with fish increase the mercury content increases.
#         There is certainly almost no mercury content when no fish is consumed, while there is the greatest mercury content
#         when the most fish is consumed.
# Strength of relationship: the points show a loose association particularly between 1 and 10 meals with fish.  The relationship
#         is stronger between 10 and 15 meals with fish, but again is loosely related between 15 and 20 meals with fish.


# Question 3: correlation
cor(
  fish$meals.w.fish,
  fish$total.hg
  )
# correlation value 0.6991094 suggests a reasonably strong positive correlation.
cor.test(
  fish$meals.w.fish,
  fish$total.hg,
  alternative='two.sided'
  )
# Hypothesis:
# ρ=0 (H0: there is no linear association)
# ρ≠0 (H1: there is a linear association).
# for t = 9.6793, df = 98, p-value = 6.013e-16
# alternative hypothesis: ρ≠0, so there IS a linear association
# for a 95 percent confidence interval between 0.5827074 and 0.7874031


# Question 4: least squares regression
# equation: ...
lm.result <- lm(fish$total.hg~fish$meals.w.fish)
abline(lm.result, lty=1, lwd=3, col="blue")  



# Question 5: interpretation of beta0 & beta1
x.bar <- mean(fish$meals.w.fish)
sx <- sd(fish$meals.w.fish)
y.bar <- mean(fish$total.hg)
sy <- sd(fish$total.hg)
r <- cor(
  fish$meals.w.fish,
  fish$total.hg
)
beta_1 <- r*sy/sx
beta_1
# beta_1: 0.2759503
# interpretation: ...
beta_0 <- y.bar - beta1*x.bar
beta_0
# beta_0: 1.687643
# interpretation: ...


# Question 6: ANOVA, R squared, 90% confidence interval
qf(0.9,df1=1, df2=98)
anova(lm.result)
