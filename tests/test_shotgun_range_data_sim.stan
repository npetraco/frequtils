data {
  int<lower=0>  n;               // observations, rows
  int<lower=0>  p;               // distances, cols
  matrix[n,p]   X;               // data: obs x dists
  vector<lower=0>[p] mu_hyp;     // mean for each dist hyperparam. Should be positive because they are physical areas
  vector<lower=0>[p] sigma_hyp;  // sd for each dist hyperparam
  real<lower=0> sd_sigma_hyp; // sd for the sd's of each dist hyperparam.
  int<lower=0>  nu_t_fix;        // df. for liklihood
}
parameters {
  vector<lower=0>[p] mu;    // keep mu's positive
  vector<lower=0>[p] sigma;
}
model {

  //Prior: Assume means for each group are pretty tight, so use normal prior instead of student
  for(i in 1:p){
    mu[i] ~ normal(mu_hyp[i], sigma_hyp[i]/sqrt(n));
    sigma[i] ~ normal(sigma_hyp[i], sd_sigma_hyp);
  }

  //likelihood (vectorized)
  for(i in 1:p){
    X[,i] ~ student_t(nu_t_fix, mu[i], sigma[i]);
  }

}
//generated quantities {
  //ppd BROKEN!!!!!!!!!!
  //posterior predictive distribution
  //real Xpred;
  //for(i in 1:p){
  //  Xpred[i] = student_t_rng(nu_t_fix, mu[i], sigma[i]);
  //}
  //Xpred = student_t_rng(nu_t_fix, mu, sigma);
//}
