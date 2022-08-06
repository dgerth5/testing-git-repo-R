library(rstan)
library(dplyr)
library(lme4)

data("sleepstudy", package = "lme4")
summary(sleepstudy$Reaction)

# get fixed and random effects matricies
n = lmer(Reaction ~ Days + (1|Subject), data = sleepstudy)

x = getME(n, "X")
z = as.matrix(getME(n, "Z"))

# write model

stan_mod <- "
data {
  int <lower = 0> N; //number of predictors
  vector[N] y; //response

  int <lower = 0> num_fix; //number of fixed effects including intercept
  matrix[N,num_fix] fix_mat;

  int <lower = 0> num_random; //number of subjects
  matrix[N,num_random] ran_mat;

}

parameters {
  real <lower = 0> pop_var;
  vector[num_fix] beta_fix;
  vector[num_random] beta_ran;

}

model {

  beta_fix[1] ~ normal(255,20); // intercept
  beta_fix[2] ~ normal(10,10); // days
  pop_var ~ normal(0, 10);
  beta_ran ~ normal(0, 35); // random effect

  y ~ normal(fix_mat*beta_fix + ran_mat*beta_ran, pop_var);

}

generated quantities {
  vector[N] y_pred;
  vector[N] mu;

  mu = fix_mat*beta_fix + ran_mat*beta_ran;
  
  for (i in 1:N){
    y_pred[i] = normal_rng(mu[i], pop_var);
  }
  
}
"

md <- stan_model(model_code = stan_mod)

dat = list(N = length(sleepstudy$Reaction),
           y = sleepstudy$Reaction,
           num_fix = ncol(x),
           num_random = ncol(z),
           fix_mat = x,
           ran_mat = z)

fit <- sampling(md,
                data = dat,
                chains = 4, iter = 2000)
print(fit, pars = c("pop_var","beta_fix","beta_ran"))

library(bayesplot)
y_rep = extract(fit)[["y_pred"]]
samp101 = sample(nrow(y_rep), length(sleepstudy$Reaction))
color_scheme_set("green")
ppc_dens_overlay(sleepstudy$Reaction, y_rep[samp101,])
 