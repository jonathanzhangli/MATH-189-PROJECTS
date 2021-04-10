# ======
# SETUP
# ======
df = read.delim("babies.txt", sep = "")

# ---------------------------------------------------------------------------
# 2.1. Summarize numerically the two distributions of birth weight for 
# babies born to women who smoked during their pregnancy and for babies born to
# women who did not smoke during their pregnancy. 
# ---------------------------------------------------------------------------

# split into two dataframes for smokers and nonsmokers

# check smoke, clean up mistakes
table(df$smoke)
df$smoke[df$smoke == 9] <- 0 #recode random 9 to 0

smokers = df[df$smoke == 1,]
nonsmokers = df[df$smoke == 0,]

# birthweight - smokers
smokers_bwt = smokers$bwt
summary(smokers_bwt)

# birthweight - nonsmokers
nonsmokers_bwt = nonsmokers$bwt
summary(nonsmokers_bwt)


# ---------------------------------------------------------------------------
# 2.2. Use graphical methods to compare the two distributions of birth weight. 
# ---------------------------------------------------------------------------

library(ggplot2)

# density plot
df$smoke = as.factor(df$smoke) # change from numerical to factor
ggplot(df, aes(x = bwt, color = smoke)) + geom_density() +
  ggtitle("Infant Birthweight (oz): Smokers vs Nonsmokers") +
  xlab("birthweight") 

# qqplot
qqplot(x= smokers_bwt, y = nonsmokers_bwt,
       pch = 1, cex = 0.75, 
       xlab = "birthweight (smokers)", 
       ylab = "birthweight (nonsmokers)",
       main = "Infant Birthweight (oz): Smokers vs Nonsmokers", 
       xlim= c(50, 180), ylim = c(50, 180))

abline(0, 1, lty = "dashed", col = "gray")

# boxplot
boxplot(smokers_bwt, nonsmokers_bwt, 
        names = c("Smokers", "Nonsmokers"),
        boxwex = 0.25, 
        main = "Infant Birthweight (oz): Smokers vs Nonsmokers",
        ylab = "birthweight (oz)")

# 2 mean T-test
test = t.test(nonsmokers_bwt, smokers_bwt)
t.test(nonsmokers_bwt, smokers_bwt)

# Odds Ratio
df_adjusted <- df
df_adjusted$low[df_adjusted$bwt >= 88] <- 0
df_adjusted$low[df_adjusted$bwt < 88] <- 1

logodds <- glm(formula = low ~ smoke,family = binomial(link = "logit"), data = df_adjusted)
summary(logodds) # the log odds are 1.4053. 

# ----
# QUESTION: What is the odds ratio that the mother is a smoker?
# ANSWER: 4.0769

odds_ratio <- exp(1.4053) #The odds ratio with e^1.4053 = 4.0769


# ---------------------------------------------------------------------------
# 2.3
# ---------------------------------------------------------------------------
# number of low birth weights and size of each sample
lbwt_smokers = length(smokers_bwt[smokers_bwt <= 88])
lbwt_nonsmokers = length(nonsmokers_bwt[nonsmokers_bwt <= 88])
n_smokers = nrow(smokers)
n_nonsmokers = nrow(nonsmokers)

# proportion of low birth rates in two groups
phat_s = lbwt_smokers/n_smokers
phat_n = lbwt_nonsmokers/n_nonsmokers

# calculate z score to evaluate reliability
pstar = (lbwt_smokers + lbwt_nonsmokers)/ nrow(df)
qstar = (length(smokers_bwt[smokers_bwt > 88]) + 
           length(nonsmokers_bwt[nonsmokers_bwt > 88]))/ nrow(df)
z = (phat_s - phat_n) / sqrt((pstar*qstar)/n_smokers + 
                               (pstar*qstar)/n_nonsmokers )

# calculate pvalue
pvalue =  pnorm(x, lower = F) * 2
