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

# check dataset, clean up mistakes
table(df$smoke)
df$smoke[df$smoke == 9] <- 0 #recode random 9 to 0

smokers = df[df$smoke == 1,]
nonsmokers = df[df$smoke == 0,]

# create skewness and kurtosis functions
skewness <- function(X) {
  Xbar = mean(X)
  Z <- (X - Xbar) / sd(X)
  return(mean(Z^3))
}

kurtosis <- function(X) {
  Xbar = mean(X)
  Z <- (X - Xbar) / sd(X)
  return(mean(Z^4))
}

# birthweight - smokers
smokers_bwt = smokers$bwt
summary(smokers_bwt)
smokers_skew = skewness(smokers_bwt)
smokers_kurtosis = kurtosis(smokers_bwt)

# birthweight - nonsmokers
nonsmokers_bwt = nonsmokers$bwt
summary(nonsmokers_bwt)
skewness(nonsmokers_bwt)
kurtosis(nonsmokers_bwt)


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

par(mfrow = c(1, 3))
## qqnorm - check if is a normal distribution
qqnorm(smokers_bwt, 
       main = "Infant Birth Weights (Smokers) vs Normal Distribution",
       xlab = "Normal Distribution", 
       ylab = "Smokers")
abline(0, 1, lty = "dashed", col = "gray")


qqnorm(nonsmokers_bwt, 
       main = "Infant Birth Weights (Nonsmokers) vs Normal Distribution",
       xlab = "Normal Distribution", 
       ylab = "Nonsmokers")
abline(0, 1, lty = "dashed", col = "gray")

# boxplot
boxplot(smokers_bwt, nonsmokers_bwt, 
        names = c("Smokers", "Nonsmokers"),
        boxwex = 0.25, 
        main = "Infant Birthweight (oz): Smokers vs Nonsmokers",
        ylab = "birthweight (oz)")


# ---------------------------------------------------------------------------
# 2.3 Compare the incidence (frequency) of low-birth-weight babies for the 
# two groups. How reliable do you think your estimates are? 
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
pvalue =  pnorm(z, lower = F) * 2

# check work
obs = phat_s - phat_n
pnorm(obs, lower.tail = F)
prop.test(c(lbwt_smokers, lbwt_nonsmokers), 
          c(n_smokers, n_nonsmokers), alternative = "greater")

# -------
# 3.1 
# -------
df$low_bwt <- as.numeric(df$bwt <= 88)
mod <- glm(low_bwt ~ smoke + gestation + parity + age + height + weight,  
           family = "binomial", data = df)

summary(mod)
low_bwt.coef <- mod$coefficients["low_bwt"] #p-value: <2e-16 ***
log.odds <- exp(low_bwt.coef) #2,94 
                          # (i.e. smokers are 2.94 times more likely to have a 
                          # baby w/ low birth weight)
log.odds.Intercept <- exp(8.729095)
log.odds.smoke1 <- exp(1.058535)
log.odds.gestation <- exp(-0.033075)
log.odds.parity <- exp(-0.011258)
log.odds.age <- exp(0.016399)
log.odds.height <- exp(-0.048051)
log.odds.weight <- exp(-0.003452)

# -------
# 3.2 
# -------
t.test(nonsmokers$bwt, smokers$bwt, alternative = "greater")
      # p-val: 2.2 e-16

diff = mean(nonsmokers_bwt) - mean(smokers$bwt) 
      # 8.98621 ounces which is about 254.755 g

