// This is the conditional version of the pitch distribution model, meaning that the league
// hyperparameters are assumed to be known, and we are estimating the pitcher parameters
// conditional on those league hyperparameters. We have to estimate the complete model before we
// can estimate the conditional model.

data {
  int<lower=0> n; //number of observations
  int<lower=0> s; //number of player seasons
  int<lower=2> c; //Number of Characteristics
  matrix[n,c] v; //observed values
  vector[n] sz_top; //Strike zone top
  vector[n] sz_bottom; //strike zone bottom
  vector[n] balls;
  vector[n] strikes;
  vector[n] hand;
  array[n] int bsh; //context vector
  array[n] int p; //Player Season id vector
  row_vector[24] bsh_weights; //context weights
  vector[c] tau_fixed; //centrality prior
  vector<lower=0>[c] gamma_fixed; //SD in centrality parameter
  vector<lower=0>[c] epsilon_fixed; //Mean for spread prior
  vector<lower=0>[c] eta_fixed; //SD for spread prior
  matrix[24,c] lambda_fixed; //count coefficients
  matrix<lower=0>[24,c] zeta_fixed; //count spread coefficients
  cholesky_factor_corr[c] leagueRho_fixed; // prior correlation
}

parameters {
  matrix<lower=0>[s,c] sigma; //spread coefficients
  matrix[s,c] mu; //centrality coefficients
  array[s] cholesky_factor_corr[c] Rho; // player correlation
  vector[c] theta; //sz_top
  vector[c] kappa; //sz_bottom
  matrix[s-1,c] nunorm; //ball coefficients
  matrix[s-1,c] xinorm; //strike coefficients
  matrix[s-1,c] pinorm; //handedness coefficients
}

transformed parameters {
  matrix[24,c] lambda;      // count effect on mean (additive)
  matrix[24,c] zeta;        // count effect on standard deviation (multiplicative)
  matrix[s,c] nu;           // ball coefficients
  matrix[s,c] xi;           // strike coefficients
  matrix[s,c] pivar;        // handedness coefficients
  lambda = lambda_fixed;    // we do this just so that the value of lambda is captured in model fit
  zeta = zeta_fixed;        // we do this just so that the value of eta is captured in model fit
  nu[2:s,1:c]=nunorm*0.05;
  xi[2:s,1:c]=xinorm*0.05;
  pivar[2:s,1:c]=pinorm*0.1;
  for(i in 1:c){
    nu[1,i]=-sum(nu[2:s,i]);
    xi[1,i]=-sum(xi[2:s,i]);
    pivar[1,i]=-sum(pivar[2:s,i]);
  }
}

model {
  matrix[n,c] means;
  matrix[n,c] sds;
  matrix[n,c] zscores;
  for(i in 1:n){
    for(j in 1:c){
      means[i,j] = mu[p[i],j] + lambda_fixed[bsh[i],j] + sz_top[i] * theta[j] + sz_bottom[i] * kappa[j] + pivar[p[i],j] * hand[i] + nu[p[i],j] * balls[i] + xi[p[i],j] * strikes[i];
      sds[i,j] = sigma[p[i],j] * zeta_fixed[bsh[i],j];
    }
  }
  zscores=(v-means)./sds;
  target+=-sum(log(sds)); //due to transformation
  for (i in 1:n){
    zscores[i,1:c] ~ multi_normal_cholesky(to_row_vector(rep_array(0.0,c)), Rho[p[i]]);
  }
  lambda_fixed[1,1:c] ~ normal(0,0.2);
  zeta_fixed[1,1:c] ~ normal(1,0.1);
  to_vector(nunorm) ~ std_normal();
  to_vector(xinorm) ~ std_normal();
  to_vector(pinorm) ~ std_normal();
  nu[1,1:c] ~ normal(0,0.05); //Not sure these priors are best instead of some hyperparameterization (true elsewhere but esp for these pitcher specific)
  xi[1,1:c] ~ normal(0,0.05);
  pivar[1,1:c] ~ normal(0,0.1);
  for(i in 1:c){
    mu[1:s,i] ~ normal(tau_fixed[i], gamma_fixed[i]);
    sigma[1:s,i] ~ normal(epsilon_fixed[i], eta_fixed[i]);
  }
  for(i in 2:c){
    for(j in 1:(i-1)){
      for(k in 1:s){
        Rho[k][i,j] ~ normal(leagueRho_fixed[i,j], 0.1);
      }
    }
  }
  for(i in 1:s){
    Rho[i] ~ lkj_corr_cholesky(1);
  }
}
