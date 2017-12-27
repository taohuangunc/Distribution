library(shiny)

# To be called from server.R
####################################################
# continuous distributions
####################################################
# F distribution
f.func <- function(df1, df2, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) df(x, df1=df1, df2=df2, ncp=ncp)
  } else {
    func <- function(x) pf(x, df1=df1, df2=df2, ncp=ncp)
  }
  return(func)
}

# non-cerntral F distribution
ncf.func <- function(df1, df2, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) df(x, df1=df1, df2=df2, ncp=ncp)
  } else {
    func <- function(x) pf(x, df1=df1, df2=df2, ncp=ncp)
  }
  return(func)
}

# chi-squares distribution
chisq.func <- function(df, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dchisq(x, df=df, ncp=ncp)
  } else {
    func <- function(x) pchisq(x, df=df, ncp=ncp)
  }
  return(func)
}

#  non-central chi-square distribution
ncChisq.func <- function(df, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dchisq(x, df=df, ncp=ncp)
  } else {
    func <- function(x) pchisq(x, df=df, ncp=ncp)
  }
  return(func)
}

# gamma distribution
gamma.func <- function(shape, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dgamma(x, shape=shape, scale=scale)
  } else {
    func <- function(x) pgamma(x, shape=shape, scale=scale)
  }
  return(func)
}

# cauchy distribution
cauchy.func <- function(location, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dcauchy(x, location=location, scale=scale)
  } else {
    func <- function(x) pcauchy(x, location=location, scale=scale)
  }
  return(func)
}

# exponential distribution
exp.func <- function(rate, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dexp(x, rate=rate)
  } else {
    func <- function(x) pexp(x, rate=rate)
  }
  return(func)
}

# normal distribution
norm.func <- function(mean, sd, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dnorm(x, mean=mean, sd=sd)
  } else {
    func <- function(x) pnorm(x, mean=mean, sd=sd)
  }
  return(func)
}

# log-normal distribution
lnormal.func <- function(meanlog, sdlog, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dlnorm(x, meanlog, sdlog)
  } else {
    func <- function(x) plnorm(x, meanlog, sdlog)
  }
  return(func)
}

# t distribution
t.func <- function(df, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dt(x, df=df, ncp=ncp)
  } else {
    func <- function(x) pt(x, df=df, ncp=ncp)
  }
  return(func)
}

# non-central t distribution
nct.func <- function(df, ncpncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dt(x, df=df, ncp=ncp)
  } else {
    func <- function(x) pt(x, df=df, ncp=ncp)
  }
  return(func)
}

#  Beta distribution
beta.func <- function(shape1, shape2, p_or_c){
  ncp <- 0
  if(p_or_c == "p"){
    func <- function(x) dbeta(x, shape1=shape1, shape2=shape2, ncp=ncp)
  } else {
    func <- function(x) pbeta(x, shape1=shape1, shape2=shape2, ncp=ncp)
  }
  return(func)
}

# non-central beta distribution
ncbeta.func <- function(shape1, shape2, ncp, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dbeta(x, shape1=shape1, shape2=shape2, ncp=ncp)
  } else {
    func <- function(x) pbeta(x, shape1=shape1, shape2=shape2, ncp=ncp)
  }
  return(func)
}

# uniform distribution
unif.func <- function(min, max, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dunif(x, min=min, max=max)
  } else {
    func <- function(x) punif(x, min=min, max=max)
  }
  return(func)
}

# logistic distribution
logis.func <- function(location, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dlogis(x, location=location, scale=scale)
  } else {
    func <- function(x) plogis(x, location=location, scale=scale)
  }
  return(func)
}

# weibull distribution
weibull.func <- function(shape, scale, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dweibull(x, shape=shape, scale=scale)
  } else {
    func <- function(x) pweibull(x, shape=shape, scale=scale)
  }
  return(func)
}

####################################################
# discrete distributions
####################################################
# geometric distribution
geom.func <- function(prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dgeom(x, prob=prob)
  } else {
    func <- function(x) pgeom(x, prob=prob)
  }
  return(func)
}

# hypergeometric distribution
hyper.func <- function(m, n, k, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dhyper(x, m=m, n=n, k=k)
  } else {
    func <- function(x) phyper(x, m=m, n=n, k=k)
  }
  return(func)
}

# binomial distribution
binom.func <- function(size, prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dbinom(x, size=size, prob=prob)
  } else {
    func <- function(x) pbinom(x, size=size, prob=prob)
  }
  return(func)
}

# negative binomial distribution
nbinom.func <- function(size, prob, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dnbinom(x, size=size, prob=prob)
  } else {
    func <- function(x) pnbinom(x, size=size, prob=prob)
  }
  return(func)
}

# poisson distribution
pois.func <- function(lambda, p_or_c){
  if(p_or_c == "p"){
    func <- function(x) dpois(x, lambda=lambda)
  } else {
    func <- function(x) ppois(x, lambda=lambda)
  }
  return(func)
}

# # discrete 
# dunif.func <- function(min, max, p_or_c){
#   if(p_or_c == "p"){
#     func <- function(x) dunif(x, min, max)
#   } else {
#     func <- function(x) punif(x, min, max)
#   }
#   return(func)
# }

