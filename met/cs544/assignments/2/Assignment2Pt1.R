# Part 1a
# A1: event that person has lung disease
# A2: event that person does not have lung disease
# B: event that person selected is a smoker
# 
# P(A1) = 0.07
# P(A2) = 0.93
# P(B|A1) = 0.90
# P(B|A2) = 0.25
# 
# # probability random smoker has lung disease
# P(A1|B) = 
#   (P(A1) * P(B|A1))/(P(A1) * P(B|A1) + P(A2) * P(B|A2))
#   (0.07*0.9)/(0.07*0.9 + 0.93*0.25)
#   0.213
# 
# # probability random smoker does not have lung disease
#   P(A1|B) = 
#     (P(A2) * P(B|A2))/(P(A1) * P(B|A1) + P(A2) * P(B|A2))
#   (0.93*0.25)/(0.07*0.9 + 0.93*0.25)
#   0.787

  
  bayes <- function (prior, likelihood) {
    numerators <- prior * likelihood
    return (numerators / sum(numerators))
  }
  
  prior <- c(0.07, 0.93)
  likelihood <- c(0.9, 0.25)
  
  bayes(prior, likelihood)

  
# Part 1b
A1: event of Republican
A2: event of Democrat
A3: event of Independent

P(A1) = 0.5
P(A2) = 0.4
P(A3) = 0.1

B: event supports tax
P(B|A1) = 0.4
P(B|A2) = 0.7
P(B|A3) = 0.2


# P(A1|B) = 
#   (P(A1) * P(B|A1))/(P(A1) * P(B|A1) + P(A2) * P(B|A2) + P(A3) * P(B|A3))
#   (0.5*0.4)/(0.5*0.4 + 0.4*0.7 + 0.1*0.2)
#   0.4
# P(A2|B) = 
#   (P(A1) * P(B|A1))/(P(A1) * P(B|A1) + P(A2) * P(B|A2) + P(A3) * P(B|A3))
#   (0.4*0.7)/(0.5*0.4 + 0.4*0.7 + 0.1*0.2)
#   0.56
# P(A3|B) = 
#   (P(A1) * P(B|A1))/(P(A1) * P(B|A1) + P(A2) * P(B|A2) + P(A3) * P(B|A3))
#   (0.1*0.2)/(0.5*0.4 + 0.4*0.7 + 0.1*0.2)
#   0.04

prior <- c(0.5, 0.4, 0.1)
likelihood <- c(0.4, 0.7, 0.2)
bayes(prior, likelihood)


