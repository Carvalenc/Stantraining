# install.packages("ggplot2") 
# install.packages("faraway")
library(ggplot2) #Opening libraries
library(rstan)
library(reshape2)
library(dplyr)
library(faraway)


###################################################
#----------- Understanding Data
###################################################

data(psid, package="faraway") #Loading the data
head(psid)
str(psid)

summary(psid)

psid20 <- filter(psid, person <= 20)

ggplot(psid20, aes(x=year, y=income)) + 
  geom_line() +
  facet_wrap(~ person)

ggplot(psid20, aes(x=year, y=income+100, group=person)) + 
  geom_line() +
  facet_wrap(~ sex) +
  scale_y_log10()



###################################################
#-----------Setting up the data as a list
###################################################

psid$cyear <- psid$year-78  # Age centering    
lmod <- lm(log(income) ~ cyear*sex +age+educ, psid) # Model
x <- model.matrix(lmod) # Extracts the inputs of the model
psiddat <- list(Nobs = nrow(psid),
                Npreds = ncol(x),
                Ngroups = length(unique(psid$person)),
                y = log(psid$income),
                x = x,
                timevar = psid$cyear,
                group = psid$person)

####################################################
#-----------Fitting the model
####################################################

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
rt = stanc("longitudinal.stan")
sm = stan_model(stanc_ret = rt, verbose=FALSE)
set.seed(123)
system.time(fit <- sampling(sm, data=psiddat))


pname <- "sigmaeps"
pname <- 'beta[2]'
muc <- rstan::extract(fit, pars=pname,  permuted=FALSE, inc_warmup=FALSE)
mdf <- melt(muc)
ggplot(mdf,aes(x=iterations,y=value,color=chains)) + geom_line() + ylab(mdf$parameters[1])





# === Ourput
print(fit,par=c("beta","sigmaint","sigmaslope","sigmaeps"))



postsig <- rstan::extract(fit, pars=c("sigmaint","sigmaslope","sigmaeps"))
ref <- melt(postsig)
colnames(ref)[2:3] <- c("logincome","parameter")
ggplot(data=ref,aes(x=logincome))+geom_density()+facet_wrap(~parameter,scales="free")


ref <- melt(rstan::extract(fit, pars="beta"))
colnames(ref)[2:3] <- c("parameter","logincome")
ref$parameter <- factor(colnames(x)[ref$parameter])
ref$OR <- exp(ref$logincome)
ggplot(ref, aes(x=OR))+
  geom_density()+
  geom_vline(xintercept=1)+
  facet_wrap(~parameter,scales="free")


sessionInfo()


