#'------------------------------------------------------------------------------
#'      .    .    .    ...   .     .      .   
#'     /\    |\   |  .'   `. |     |     /\   
#'    /  \   | \  |  |     |  \    /    /  \  
#'   /--- \  |  \ |  |     |   \  /    /--- \ 
#'  /      \ |   \|   `.__.'    \/    /      \
#'------------------------------------------------------------------------------
#'
#' One-way (single factor) ANalysis Of VAriance.
#' MAT150 - Statistics for Life Sciences (Fall 2022)
#' Section 15.1
#' 
#' 2022-12-01
#' jquinlan
#'------------------------------------------------------------------------------  

## Load Data

# If you get an error in this step, you do not have the data file in the proper
# location.  In other words, it cannot find the file. 

setwd("C://Users//you//Documents//MAT150")
data = read.csv('ex154.csv', header = T)   	# <-- EDIT THIS AS NEEDED

# ------------------------------------------------------------------------------


## Check Assumptions

# 1. Normality

# Split off each treatment group
PP = data[data$treatment =='PP',]
PS = data[data$treatment =='PS',]
GS = data[data$treatment =='GS',]
GP = data[data$treatment =='GP',]


# histograms
par(mfrow = c(2,2))   
hist(PP$mass, main = 'Histogram of PP')
hist(PS$mass)
hist(GS$mass)
hist(GP$mass)

# Reset Parameters of plot
dev.off()

# Normality Test (H0: data is normally distributed)
# Not required to do this
shapiro.test(PP$mass)
shapiro.test(PS$mass)
shapiro.test(GS$mass)
shapiro.test(GP$mass)


# 2. Homoscedasticity

# Here are the variances per level (of the single factor)
c(var(PP$mass, na.rm = T), 
  var(PS$mass, na.rm = T), 
  var(GS$mass, na.rm = T), 
  var(GP$mass, na.rm = T))

# Box plots (look to see if variances are equal)
boxes = boxplot(mass ~ treatment, data = data, xlab = "Factor", ylab = "mass")

# Test for equal variances (H0: variances are equal among groups)
# Not required to do this
bartlett.test(mass ~ treatment, data = data)

# 3. Outliers?  (check from boxplots)
# Visually there are none (see graph of boxplot) and numerically we check using:
boxes$out


# ------------------------------------------------------------------------------

## ANOVA Test
# H0: mu1 = mu2 = mu3 = mu4 (i.e., mean diff. in fat mass the same)

# (a). Create model
model = aov(mass ~ treatment, data = data)

# (b). Get summary of model (Compare with Table 15.3)
summary(model)



## Mult. Comparisons
TukeyHSD(model)

