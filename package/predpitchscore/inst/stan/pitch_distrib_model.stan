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
  int bsh[n]; //context vector
  int p[n]; //Player Season id vector
  row_vector[24] bsh_weights; //context weights
}

parameters {
  vector<lower=0>[c] gamma; //SD in centrality parameter
  vector<lower=0>[c] epsilon; //Mean for spread prior
  vector<lower=0>[c] eta; //SD for spread prior
  matrix[23,c] lambdanorm; //count coefficients normalized
  matrix<lower=-20>[23,c] zetanorm; //count std normalized
  matrix<lower=0>[s,c] sigma; //spread coefficients
  vector[c] tau; //centrality prior
  matrix[s,c] mu; //centrality coefficients
  cholesky_factor_corr[c] leagueRho; // prior correlation
  cholesky_factor_corr[c] Rho[s]; // player correlation
  vector[c] theta; //sz_top
  vector[c] kappa; //sz_bottom
  matrix[s-1,c] nunorm; //ball coefficients
  matrix[s-1,c] xinorm; //strike coefficients
  matrix[s-1,c] pinorm; //handedness coefficients
}

transformed parameters {
  matrix[24,c] lambda; //count coefficients
  matrix<lower=0>[24,c] zeta; //count spread coefficients
  matrix[s,c] nu; //ball coefficients
  matrix[s,c] xi; //strike coefficients
  matrix[s,c] pivar; //handedness coefficients
  lambda[2:24,1:c]=lambdanorm*0.2;
  zeta[2:24,1:c]=zetanorm*0.1+1;
  lambda[1,1:c]=-(bsh_weights[2:24] * lambda[2:24,1:c])/bsh_weights[1]; //Honestly not certain how necessary this is but like having it
  zeta[1,1:c]=1+-(bsh_weights[2:24] * (zeta[2:24,1:c]-1))/bsh_weights[1];
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
      means[i,j]=mu[p[i],j]+lambda[bsh[i],j]+sz_top[i]*theta[j]+sz_bottom[i]*kappa[j]+pivar[p[i],j]*hand[i]+nu[p[i],j]*balls[i]+xi[p[i],j]*strikes[i];
      sds[i,j]=sigma[p[i],j]*zeta[bsh[i],j];
    }
  }
  zscores=(v-means)./sds;
  target+=-sum(log(sds)); //due to transformation
  for (i in 1:n){
    zscores[i,1:c] ~ multi_normal_cholesky(to_row_vector(rep_array(0.0,c)), Rho[p[i]]);
  }
  to_vector(lambdanorm) ~ std_normal();
  to_vector(zetanorm) ~ std_normal();
  epsilon ~ normal(0.7,0.5);
  gamma ~ normal(0.7,0.5);
  eta ~ normal(0.5,0.5);
  tau ~ normal(0,0.2);
  theta ~ normal(0,0.1);
  kappa ~ normal(0,0.1);
  lambda[1,1:c] ~ normal(0,0.2);
  zeta[1,1:c] ~ normal(1,0.1);
  to_vector(nunorm) ~ std_normal();
  to_vector(xinorm) ~ std_normal();
  to_vector(pinorm) ~ std_normal();
  nu[1,1:c] ~ normal(0,0.05); //Not sure these priors are best instead of some hyperparameterization (true elsewhere but esp for these pitcher specific)
  xi[1,1:c] ~ normal(0,0.05);
  pivar[1,1:c] ~ normal(0,0.1);
  for(i in 1:c){
    mu[1:s,i] ~ normal(tau[i],gamma[i]);
    sigma[1:s,i] ~ normal(epsilon[i],eta[i]);
  }
  for(i in 2:c){
    for(j in 1:(i-1)){
      for(k in 1:s){
        Rho[k][i,j]~normal(leagueRho[i,j],0.1);
      }
    }
  }
  for(i in 1:s){
    Rho[i] ~ lkj_corr_cholesky(1);
  }
  leagueRho ~ lkj_corr_cholesky(1);
}