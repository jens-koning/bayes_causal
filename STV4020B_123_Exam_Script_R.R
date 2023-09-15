########### Exam STV4020B, Fall 2021, Script, Candidate: 123 ############
# These scripts are also included at the end of each week in the PDF document. 

#### Exam Week 1 ####
install.packages("tidyverse")
install.packages("stargazer")
library(tidyverse)
library(stargazer)

## Question 1 ci, A & B)

library(readr)
X4020b_2021_week1_Q1_waterways_sim_data <- read_csv("4020b_2021_week1_Q1_waterways_sim-data.csv")
View(X4020b_2021_week1_Q1_waterways_sim_data)

Mod1 <- lm(chemicals ~ treatment, data = X4020b_2021_week1_Q1_waterways_sim_data)
Mod2 <- lm(trash ~ treatment, data = X4020b_2021_week1_Q1_waterways_sim_data)
stargazer(Mod1, Mod2, type = "text") 
stargazer(Mod1, Mod2)
# Our treatment is estimated to have a negative effect on water chemical levels, 
# with the Average Treatment Effect (ATE) being -13.727 for chemicals, being significant on a **p<0.05 and
# a Standard Error(SE) of 6.190.  The ATE for trash is -8.468, being significant on 
# ***p<0.01 with a SE of 1.278. 

## Question 1 cii, A & B)

Mod3 <- lm(chemicals ~ treatment + industry + local_income + flow_rate, data = X4020b_2021_week1_Q1_waterways_sim_data)
Mod4 <- lm(trash ~ treatment + industry + local_income + flow_rate, data = X4020b_2021_week1_Q1_waterways_sim_data)
stargazer(Mod3, Mod4, type = "text") 
# Our treatment is estimated to have a negative effect on water chemical levels, 
# with the Average Treatment Effect (ATE) being -13.727, being significant on a p<0.05.
# The Standard Error(SE) of this estimate = 6.190. 


## Question 1 cii C)

stargazer(Mod1, Mod2, Mod3, Mod4, type = "text") 
stargazer(Mod1, Mod2, Mod3, Mod4)
# Both ATE are slightly smaller when including the covariates, 
# for chemicals (-13.727 -> )
# The SE of treatment is slightly smaller when including the covariates
# for chemicals (from 6.190 to 6.010) and trash (from 1.278 to 1.256).


# This SE is slightly smaller than the SE of our previous bivariate model, suggesting
# that our estimates are slightly less uncertain. This happens as we are able to control for the effect 
# of the covariates. We are taking them out of the error term and including them in the equation.


## Question 1 c iii)

Mod6 <- lm(chemicals ~ treatment + trash, data = X4020b_2021_week1_Q1_waterways_sim_data)
stargazer(Mod6, type = "text")
Mod7 <- lm(chemicals ~ treatment, data = X4020b_2021_week1_Q1_waterways_sim_data)
stargazer(Mod7, type = "text")

stargazer(Mod6, Mod7, type = "text") #shows how our SE are effected by including trash on the right-hand side of the model.
# SE increases, which is worse meaning that now our estimates are more uncertain. 

cor.test(X4020b_2021_week1_Q1_waterways_sim_data$treatment, X4020b_2021_week1_Q1_waterways_sim_data$trash)
# A simple cor.test shows that treatment affects trash (-0.2053185), introducing weak multicollinearity. 

library(ggplot2)

Mod8 <- lm(chemicals ~ trash, data = X4020b_202)

stargazer(Mod7, Mo8, )
# Yes, doing so would introduce various biases. First, the treatment and the trash variable are related to each other 
# which introduces multicolinearity which inflates the SE. Second, Mod8 shows that the variables chemcials and 
# trash are related to each other (**p<0.05), meaning that it introduces more bias through endogeneity. 

## Question 3 a

install.packages("dagitty")
library(dagitty)

DAG_Q3 <- dagitty(dag{bb="0,0,1,1"
"STATE CAPACITY" [outcome,pos="0.450,0.438"]
COL [pos="0.182,0.555"]
CON [pos="0.284,0.439"]
NAT [pos="0.185,0.318"]
COL -> "STATE CAPACITY"
COL -> CON
CON -> "STATE CAPACITY"
NAT -> "STATE CAPACITY"
NAT -> CON})
plot(DAG_Q3)


## Question 3 b 

X4020b_2021_week1_Q3_country_conflict_sim_data <- read_csv("4020b_2021_week1_Q3_country_conflict_sim-data.csv")
View(X4020b_2021_week1_Q3_country_conflict_sim_data)

Mod31 <- lm(statecapacity ~ colonialism, data = X4020b_2021_week1_Q3_country_conflict_sim_data)
stargazer(Mod31, type = "text") 
# The "effect" of colonialism on state capacity is -3.823, significant on ***p<0.01. 

## Question 3 c

conflict_data <- X4020b_2021_week1_Q3_country_conflict_sim_data
summary(conflict_data$conflict)

conflict_data2 <- conflict_data %>%
  filter(conflict > sd(conflict))
Mod32 <- lm(statecapacity ~ colonialism, data = conflict_data2)
stargazer(Mod31, Mod32, type = "text") 
stargazer(Mod31, Mod32)
# This table shows that where conflict is SD >1, the effect of colonialism is 
# 0.286 and not significant, and R2 very small. The effect is meaningfully different as we are introducing selection
# bias by keeping countries with conflict.  

## Question 3 d

Mod33 <- lm(statecapacity ~ conflict + colonialism, data = conflict_data)
stargazer(Mod33, type = "text") 
# In this regression table the "effect" of colonialism on statecapacity is positive (1.958)
# and significant on ***p<0.01. This is misleading given collider bias. Collider

## Question 3 e

Mod34 <- lm(statecapacity ~ conflict + colonialism + resources, data = conflict_data)
stargazer(Mod34, type = "text") 
# In this regression where we include resources table the "effect" of colonialism on statecapacity is negative (-1.086)
# and significant on ***p<0.01. This is also misleading given collider bias.

## Question 3 f)
stargazer(Mod31, Mod32, Mod33, Mod34, type = "text")

stargazer(Mod31, Mod32, Mod33, Mod34)



#### Exam Week 2 ####
install.packages("tidyverse")
library(tidyverse)
library(dplyr)

### 1 RDD

## Question 1

#Loading data set
exam_part_2_RDD <- readRDS("~/OneDrive - PRIO/Exam W2 STV4020B/exam_part_2_RDD.RDS")
govt_audit_data <- exam_part_2_RDD #renaming

#Making a plot with "b" = x and "audit" = y

library(ggplot2)
ggplot(govt_audit_data2, #making plot using ggplot2
       aes(y = audit, 
           x = b)) +
  geom_point() + 
  labs(title = "Municipal budget and Audit") +
  labs(x="Municipal Budget (Dollar)", y="Audit, Yes=1/No=0")
theme_bw()

#Given the cutoff it initially seems like a good starting point for a harp RDD design, 
# however it seems hard to separate confounding treatments at the 1B$ cutoff. 

## Question 2

br=seq(from=min(govt_audit_data$b),to=max(govt_audit_data$b),by=5E7) #code for setting binwidt
midpoints=(br[-1]+br[-length(br)])/2 #code for setting midpoints
govt_audit_data$b=cut(govt_audit_data$b,breaks=br) # applying bins

B=data.frame(cbind(midpoints,by(govt_audit_data$voting,govt_audit_data$b,mean))) # new data frame, attached binned b by voting and running variable b.  
names(B)=c('midpoints','audit') #renaming the axis 

##making a plot
ggplot(B, aes(midpoints, audit)) +
  geom_point() +
  labs(title = "Municipal budget and Audit") +
  geom_vline(xintercept = 1E09, colour = "red", linetype = 5) +
  labs(x="Municipal Budget (Dollar)", y="Voter turnout (%)") +
  theme_bw()

#Eyeballing suggests that there may be a slight jump around the cutoff, from 40 to 46~8% voter turnout. 

## Question 3 

govt_audit_data2 <- readRDS("~/OneDrive - PRIO/Exam W2 STV4020B/exam_part_2_RDD.RDS")
govt_audit_data2$billion = govt_audit_data2$b - 1e9

install.packages("rdrobust")
library(rdrobust)

govt_audit_data2$billion = govt_audit_data2$b - 1E09 #adding new variable


Model = rdrobust(govt_audit_data2$voting, govt_audit_data2$billion)
summary(Model)
# Coef. = 5.534
# SE = 5.603
# p-value = 0.323, 
# not significant on 5% level
# The LATE here is substantially important 
# as the estimated effect is not significant possibly due to 
# coarse observations surrounding the midpoint (SE = 5.603).

### 2 IV 

## Question 2

#load data and rename
coledu_income_data <- readRDS("~/OneDrive - PRIO/Exam W2 STV4020B/4020b_2021_week2_Q2_education_sim-data.RDS")

#run bivariate regression
library(stargazer)
Mod1 <- lm(Income ~ College, data = coledu_income_data)
stargazer(Mod1, type = "text")
# Coef. = 26,445.100, significant on ***p<0.01. 
# SE = 1,028.488

## Question 3
install.packages("ivreg")
library(ivreg)

#run IV regression
IV1 = ivreg(
  formula = Income ~ College | RichParent, data = coledu_income_data)
stargazer(IV1, type = "text")
summary(IV1)

#comparing 
library(stargazer)
stargazer(Mod1, IV1, type = "text") 
stargazer(Mod1, IV1)
# Coef. =  44,705.730, signficant on ***p<0.01
# SE = 3,971.551 



#### Exam W3 ####
library(tidyverse)

### Part 1 - Differences in Differences

### Question 1 

#load file
stv4020b_week3_exam_p1_input <- 
  readRDS("~/OneDrive - PRIO/Exam W3 STV4020B/stv4020b_week3_exam_p1_input.RDS")
#renaming
tax_subsidyRD <- stv4020b_week3_exam_p1_input

tax_subsidyRDmod <- tax_subsidyRD 

#creating dummy
tax_subsidyRDmod <- mutate(tax_subsidyRDmod,
                           regionDummy = ifelse(Region == "A", 1, 0))

#subsetting data using which() 
tax_subsidyRDmod1 <- tax_subsidyRDmod[which(tax_subsidyRDmod$Time != -1),]

#estimation DiD two-way fixed effects

Mod1 <- lm('ResDev ~ regionDummy + Time + regionDummy*Time', data=tax_subsidyRDmod1)
library(stargazer)
stargazer(Mod1, type = "text")
# The estimated treatment effect is 3.738 with a SE of 0.170.
# Firm subsidy increases R&D spending with 3.738 million euros on average.


## Question 2 

#subsetting data using which() 
tax_subsidyRDmod2 <- tax_subsidyRDmod[which(tax_subsidyRDmod$Time < 1),]

#Second model, DiD two-way fixed effects
Mod2 <- lm('ResDev ~ regionDummy + Time + regionDummy*Time', data=tax_subsidyRDmod2)
stargazer(Mod1, Mod2, type = "text")
stargazer(Mod1, Mod2)


### Part 2 

#Drawing Indonesia 
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")
install.packages("cowplot")
install.packages("googleway")
install.packages("ggrepel")
install.packages("libwegeom")
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(cowplot)
library(googleway)
library(ggrepel)
library(libwegeom)

world <- ne_countries(scale = "medium", returnclass = "sf") 
class(world)

ggplot(data = world) +
  geom_sf(color = "black", fill = "lightgreen") +
  coord_sf(expand = FALSE) +
  coord_sf(xlim = c(92.66, 142.80), ylim = c(6.67, -12.71), expand = FALSE) + #coordinates for Indonesia
  ggtitle("Coverage, Hansen et al. (2013)", subtitle = "Indonesia, 1982-2018. Green = data availible.")
theme_bw()




#### Exam Week 4 ####
library(rethinking)
library(rstan)

### Question 1 

## a)

#Pr(false positive test | omicron) = 0.1 
pr_falsepos <- 0.1 # unconditional prob  
#Pr(true positive test | omicron) = 0.9
pr_truepos <- 0.9 # unconditional prob  

#Pr(false negative test | omicron) = 0.1 
pr_falseneg <- 0.1 # unconditional prob    
#Pr(true negative test | omicron) = 0.9
pr_trueneg <- 0.9 # unconditional prob  
#Pr(positive in population | omicron ) = 0.01
pr_popomi <- 0.01 # unconditional prob  

#Pr(positive | omicron) = 0.08333333
Pr_positive <- pr_truepos * pr_positiveomi + pr_falsepos * (1 - pr_popomi)
Pr_positive # 0.108

## b)

#Pr(omicron | positive) = 0.08333333
Pr_Truepos_Trueomi <- (pr_truepos * pr_popomi) / Pr_positive
Pr_Truepos_Trueomi # 0.08333333


###Question 2 

#loding states2
load("~/OneDrive - PRIO/Bayes/states2.Rda")
precis(states2) #exploring

## a)

precis(states2$density) # exploring the data
dens(states2$density) # exploring the data
states2$density_centered <- (states2$density - mean(states2$density)) # making a new centered variable
precis(states2$density_centered)

xbar <- mean(states2$density) 


#Making a linear model with quap
Mod1 <- quap(
  alist(
    obama08 ~ dnorm(mu , sigma) ,
    mu <- a + b1*(density - xbar)  ,
    a ~ dnorm( 50 , 20 ) ,
    b1 ~ dnorm( 0 , 1 ) , #
    sigma ~ dunif( 0 , 20 )
  ) , data=states2 )


plot( Vote_Obama ~ density , data=states2, col=rangi2 ) #centered density
post <- extract.samples( Mod1 )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )

precis(post)
precis(states2$obama08)



## b and c) 

##Using Hamiltonian Monte Carlo (HMC)

# generating data for priors
set.seed(1234)
N <- 100
a <- rnorm( N , 50, 20)
b1 <- rnorm( N , 0 , 10)


Ulam1 <- ulam(
  alist(
    obama08 ~ dnorm(mu , sigma) ,
    mu <- a + b1*(density - 194.962) ,
    a ~ dnorm( 50 , 20 ) ,
    b1 ~ dnorm( 0 , 10 ) , 
    sigma ~ dunif( 0 , 20 ) 
  ) , data=states2 , chains=4, log_lik = TRUE
)

#generating summary for first model 1
precis(Ulam1, depth = 2)
traceplot(Ulam1, chains=4)
trankplot(Ulam1)

## d)

#renaming variable
Vote_Obama <- states2$obama08

set.seed(1234)
N <- 100
a <- rnorm( N , 50, 20)
b <- rnorm( N , 0 , 10)

#Plotting postrior mean estimate
plot( Vote_Obama ~ density , data=states2, col=rangi2 , 
      xlab="Population Density (State)" , ylab="Vote Share for Obama",
      main = "Posterior Mean Estimate" )
post <- extract.samples( Ulam1 ) #extracting samples from previous model
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE ) # adding a line for the posterior mean estimate

## e)


#Making a logged model

#log transforming variables
states2$density_logged <- log(states2$density)  
states2$obama08_logged <- log(states2$obama08)
xbar_logged <- log(xbar)

# generating data for priors
set.seed(1234)
N <- 100
a <- rnorm( N , 50, 20)
b_log <- rnorm( N , 0 , 10)

#Specifying logged model
Ulam1_log <- ulam(
  alist(
    obama08 ~ dnorm(mu , sigma) ,
    mu <- a + b_log*(density_logged - 5.272805) ,
    a ~ dnorm( 50 , 20 ) ,
    b_log ~ dnorm( 0 , 10 ) , 
    sigma ~ dunif( 0 , 20 ) 
  ) , data=states2 , chains=4, log_lik = TRUE
)


###Making scatterplot with raw values
plot( Vote_Obama ~ density , data=states2, col=rangi2,
      xlab="Population Density (State)" , ylab="Vote Share for Obama",
      main = "Posterior Scatterplot")
post <- extract.samples( Ulam1_log ) 
a_map <- mean(post$a)
b_map <- mean(post$b)

#Adding posterior mean estimate on logged model
plot( Vote_Obama ~ density , data=states2, col=rangi2, 
      xlab="Population Density (State)" , ylab="Vote Share for Obama",
      main = "Posterior Mean Estimate ")
post <- extract.samples( Ulam1_log ) 
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - 5.272805) , add=TRUE ) # adding a line for the posterior mean estimate

##Fitting a logged model on raw values
plot( Vote_Obama ~ density , data=states2, col=rangi2, 
      xlab="Population Density (State)" , ylab="Vote Share for Obama",
      main = "Posterior Mean Curve (logged)")
post <- extract.samples( Ulam1_log ) 
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(log(x) - 5.272805) , add=TRUE ) 
# The logged curvelinear estimates suggests diminishing returns on the population 
# density of the state on voter share for obama 


compare(Ulam1, Ulam1_log, func = WAIC)
# WIAC comparison suggests that the logged model produces better estimates,
# having a lower WAIC value and standard error.


### Question 3

## a)

# Making slimmer dataset with relevant variables
dslim <- list(obama08 = states2$obama08,
              density = states2$density,
              college = states2$college,
              prcapinc = states2$prcapinc/1000
)

#adding logged density to build on 2.e
dslim$density_logged <- log(dslim$density)


#transforming income variable
xbardensity <- mean(dslim$density)
xbarcollege <- mean(dslim$college)
xbarprcapinc <- mean(dslim$prcapinc)
xbardensity_logged <- mean(dslim$density_logged)

# hoosing priors
set.seed(1234)
N <- 100
a <- rnorm(N, 50, 10)
b_dens <- rnorm(N, 0, 5)
b1_inc <- rnorm(N, 0, 10)
b2_col <- rnorm(N, 0, 10)

#Model 1 - Including income as predictor for share of Obama voters
Ulam_inc <- ulam(
  alist(
    obama08 ~ dnorm(mu , sigma) ,
    mu <- a + b1_inc*(prcapinc - 31.9511) + 
      b_dens*(density_logged - 4.493501),
    a ~ dnorm( 50 , 10 ) ,
    b_dens ~ dnorm( 0 , 5 ) , 
    b1_inc ~ dnorm( 0 , 10 ) , 
    sigma ~ dunif( 0 , 20 ) 
  ) , data=dslim , chains=4, log_lik = TRUE
)

precis(Ulam_inc, depth = 2)

#Model 2 - Including college as predictor for share of Obama voters
Ulam_col <- ulam(
  alist(
    obama08 ~ dnorm(mu , sigma) ,
    mu <- a + b2_col*(college - 25.846) + 
      b_dens*(density_logged - 4.493501),
    a ~ dnorm( 50 , 10 ) ,
    b_dens ~ dnorm( 0 , 5 ) , 
    b2_col ~ dnorm( 0 , 10 ) , 
    sigma ~ dunif( 0 , 20 ) 
  ) , data=dslim , chains=4, log_lik = TRUE
)

precis(Ulam_col, depth = 2)

# Model 3 - Including both college and income as predictors

Ulam_col_inc <- ulam(
  alist(
    obama08 ~ dnorm(mu , sigma) ,
    mu <- a + b1_inc*(prcapinc - 31.9511) +
      b2_col*(college - 25.846) + 
      b_dens*(density_logged - 4.493501),
    a ~ dnorm( 50 , 10 ) ,
    b_dens ~ dnorm( 0 , 5 ) , 
    b1_inc ~ dnorm( 0 , 10 ) , 
    b2_col ~ dnorm( 0 , 10 ) , 
    sigma ~ dunif( 0 , 20 ) 
  ) , data=dslim , chains=4, log_lik = TRUE
)

precis(Ulam_col_inc, depth = 2)

## b & c)

compare(Ulam_col, Ulam_inc , Ulam_col_inc , func = WAIC)
# Whammie, the model with college and density as predictors has the lowest
# WAIC value. 

## d)

pairs(Ulam_col_inc) # Producing a pairs plot.




#### Exam Week 5 ####
library(rstan)
library(rethinking)

## Question 1

#loading data
load("~/OneDrive - PRIO/Exam W5 STV4020B/states2.Rda")

states2$college_centered <- states2$college - mean(states2$college)
mean(states2$college_centered) # checking centered college variable
colcen <- mean(states2$college)

# Prior predictive distribution of alpha (intercept) on the outcome scale

Alpha_prior <- quap(
  alist(
    obama_win08 ~ dbinom( 1 , p ) ,
    logit(p) <- a ,
    a ~ dnorm( 0 , 1.5 ) 
  ) , data=states2 )

set.seed(1234)
prior <- extract.prior( Alpha_prior , n=1e4 )

p <- inv_logit( prior$a )
dens( p , adj=0.1 , col="red", main = "Alpha Prior")


# Prior predictive simulations of beta on the outcome scale

Prior_simulation1 <- quap(
  alist(
    obama_win08 ~ dbinom( 1 , p ) ,
    logit(p) <-  a + b*(college-25.8) ,
    a ~ dnorm( 0 , 1.5 ) ,
    b ~ dnorm( 0 , 1) #
  ) , data=states2 )


Prior_simulation2 <- quap(
  alist(
    obama_win08 ~ dbinom( 1 , p ) ,
    logit(p) <-  a + b*(college-25.8) ,
    a ~ dnorm( 0 , 1 ) ,
    b ~ dnorm( 0 , 10) #  
  ) , data=states2 )

Prior_simulation3 <- quap(
  alist(
    obama_win08 ~ dbinom( 1 , p ) ,
    logit(p) <-  a + b*(college-25.8) ,
    a ~ dnorm( 0 , 1 ) ,
    b ~ dnorm( 0 , 0.5) #  
  ) , data=states2 )

Prior_simulation4 <- quap(
  alist(
    obama_win08 ~ dbinom( 1 , p ) ,
    logit(p) <-  a + b*(college-25.8) ,
    a ~ dnorm( 0 , 1 ) ,
    b ~ dnorm( 0 , 2) #  
  ) , data=states2 )

Prior_simulation5 <- quap(
  alist(
    obama_win08 ~ dbinom( 1 , p ) ,
    logit(p) <-  a + b*(college-25.8) ,
    a ~ dnorm( 0 , 1 ) ,
    b ~ dnorm(0.2 , 0.5) #
  ) , data=states2 )



set.seed(1234) # setting seed
prior_sim <- extract.prior( Prior_simulation , n=1e4 ) #extracting priors
prior_sim2 <- extract.prior( Prior_simulation2 , n=1e4)
prior_sim3 <- extract.prior( Prior_simulation3 , n=1e4)
prior_sim4 <- extract.prior( Prior_simulation4 , n=1e4)
prior_sim5 <- extract.prior( Prior_simulation5 , n=1e4)

p10 <- inv_logit(prior_sim$b) #only simulating beta, hence the (prior_sim$b)
p11 <- inv_logit(prior_sim2$b)
p12 <- inv_logit(prior_sim3$b)
#p13 <- inv_logit(prior_sim4$b)
p14 <- inv_logit(prior_sim5$b)

plot(density(p11, adj=0.1 ), main="Prior Predictive Simulations (Beta)", xlab="Beta (College/Higher Edu.)")
lines(density(p10 , adj=0.1), col="red") #  b ~ dnorm( 0 , 1) 
lines(density(p11 , adj=0.1), col="blue") # a ~ dnorm( 0 , 10 ) 
lines(density(p12 , adj=0.1), col="green") # a ~ dnorm( 0 , 1.5 ) 
#lines(density(p13 , adj=0.1), col="purple") #  b ~ dnorm( 0 , 2)
lines(density(p14 , adj=0.1), col="orange") #  b ~ dnorm( 0.2 , 0.5 )

legend("top", legend = c("b ~ dnorm( 0 , 1 )", "a ~ dnorm( 0 , 10 )", "a ~ dnorm( 0 , 1.5 )", "b ~ dnorm( 0.2 , 0.5 )"), #plotting explanations
       col = c("red", "blue", "green", "orange"), lwd=3) 



## Question 2
summary(states2$college) # Min value is 17.00, Max is 35.9
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.00   23.12   25.10   25.85   29.00   35.90 
college.seq <- seq(from=15, to=40, by=1 )
mu <- link(Prior_simulation5, data=data.frame(college=college.seq)) # college made into seqence and attached to mu
str(mu)
plot(states2$college, states2$obama_win08, pch = 17, col="darkred", xlab="Pop. with College/higher ed. (%)" , ylab="Obama Wins (probability)" ,
     main = "Posterior Mean Prediction (Obama Win)") #making plot
for (i in 1:250)
  points(college.seq , mu[i,], pch=16, col=col.alpha(rangi2, 0.1), # simulating blue dots
  )
mu.mean <- apply( mu, 2 , mean) #applying mean
mu.PI <- apply (mu, 2, PI, prob= 0.89) # applying posterior interval (PI). 
lines(college.seq, mu.mean) # adding line graph to show mean. 
shade(mu.PI, college.seq) # adding shade on graph to show PI. 



