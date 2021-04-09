# Program: USIPC - Log Odds
# Name: Maya Lu
# Date: 8/25/2020

# =========
# SET UP
# =========
rm(list = ls())
library(gmodels)
d <- read.csv("practice-082520.csv")

# ============
# Overall Odds
# ============
# QUESTION: What are the odds that the voter is a member of the Sikh community?
# ANSWER: The odds of a voter being Sikh is 4.0765.

CrossTable(d$var1)
d$sikh <- NA
d$sikh[d$var1 == "No"] <- 0
d$sikh[d$var1 == "Yes"] <- 1

CrossTable(d$sikh) # The odds that the voter is Sikh is 106/26 or 4.0769

# ----
# QUESTION: What are the log odds that the voter is a member of the Sikh community?
# ANSWER: The log odds are 1.4053. 

# Predictor is if individual is a participant of the survey, outcome is if they are Sikh
d$participant <- 1
logodds <- glm(formula = sikh ~ participant,family = binomial(link = "logit"), data = d)
summary(logodds) # the log odds are 1.4053. 

# ----
# QUESTION: What is the odds ratio that the voter is a member of the Sikh community?
# ANSWER: 4.0769

odds_ratio <- exp(1.4053) #The odds ratio with e^1.4053 = 4.0769

#======================================================
# Is there a difference in accuracy/inaccuracy by state?
#======================================================
# ANSWER: The only state with a statitically significant difference in accuracy is Florida (less accurate).
#         The odds ratio of being Sikh in Florida compared to other states is 0.264 (Log odds of -1.3317).

# Create a for loop that will create dummy variable indicating whether they are in that state. 
# For each iteration, it will print the name of the state and a summary of the logistic regression. 
states <- unique(d$var2)
for(state in states){
  d$state <- 0
  d$state[d$var2 == state] <- 1
  summary <- summary(glm(sikh ~ state, family = binomial(link = "logit"), data = d))
  print(state)
  print(summary)
}

# FL: Log odds are -1.3317 relative to the rest of the states.
fl_odds <- exp(-1.3317)  # Odds ratio of being Sikh in Florida to being Sikh in other states is 0.264.
                         # 3.787 times less likely to contact a Sikh voter in FL than other states recorded in the data.
# NV, GA, AZ, OH, NC, PA, MI: Not statistically significant

#======================================================================
# Just for fun, what specific states differ from Florida in accuracy?
#=======================================================================
d$var2 <- relevel(d$var2, ref = "FL")
logstates <- glm(sikh ~ var2, binomial(link = "logit"), data = d)
summary(logstates)

# MI, NC, and PA have greater odds of finding a Sikh voter than in FL.
exp(1.57819) # the odds ratio of finding a sikh voter in MI versus FL is 4.86176
exp(2.27133) # the odds ratio of finding a sikh voter in NC versus FL is 9.692283 
exp(1.93486) # the odds ratio of finding a sikh voter in PA versus FL is 6.923075



