data {
  int<lower=0> n; //number of observations
  int<lower=0> s; //number of player seasons
  int y[n]; //observed values
  int<lower=0> k; //pitch type numbers
  vector[n] sz_top; //Strike zone top
  vector[n] sz_bottom; //strike zone bottom
  vector[n] balls;
  vector[n] strikes;
  vector[n] hand;
  int bsh[n]; //context vector
  int p[n]; //Player Season id vector
}
parameters {
  matrix[s,k] mu; //player specific intercepts
  matrix[24,k] lambda; //count effects intercepts
  vector[k] tau; //prior mean
  vector<lower=0>[k] delta; //difference to other node
  vector<lower=0.2>[k] gamma; //prior sd
  vector<lower=0,upper=1>[k] epsilon; //likelihood of main node
  matrix[s,k] nu; //ball coefficients
  matrix[s,k] xi; //strike coefficients
  matrix[s,k] pivar; //handedness coefficients
  vector[k] theta; //sz_top
  vector[k] kappa; //sz_bottom
  //matrix[24,k] phi[s]; //player count effects removed as too many variables
}

model {
  vector[s] sums;
  for(i in 1:s){
    sums[i]=sum(exp(mu[i,1:k])); //Fixes it so that the mu matrices have each row adding to 1 (done as logistic model is scale insensitive and wanted each mu column to be interpretable)
  }
  for (i in 1:n) {
    y[i] ~ categorical_logit(mu[p[i],1:k]'+lambda[bsh[i],1:k]'+hand[i]*pivar[p[i],1:k]'+balls[i]*nu[p[i],1:k]'+strikes[i]*xi[p[i],1:k]'+sz_top[i]*theta+sz_bottom[i]*kappa);//+phi[p[i]][bsh[i],1:k]');
  }
  //Priors
  epsilon ~ uniform(0,1);
  tau ~ normal(-1.5,1.5);
  gamma ~ inv_gamma(2,1); //Zero avoiding prior
  delta ~ normal(5,1.5);
  to_vector(lambda) ~ normal(0,0.75);
  to_vector(nu) ~ normal(0,0.05);
  to_vector(xi) ~ normal(0,0.05);
  to_vector(pivar) ~ normal(0,0.1);
  //to_vector(phi) ~ normal(0,0.03);
  theta ~ normal(0,0.1);
  kappa ~ normal(0,0.1);
  sums ~ normal(1,0.0001);
  //Did this as a mixture of normals with the idea being one node for pitchers who have the pitch and another for pitchers that never/rarely would pitch it
  for(i in 1:s){
    for(j in 1:k){
      target+= log_sum_exp(log(epsilon[j]) + normal_lpdf(mu[i,j] | tau[j], gamma[j]),
                  log(1-epsilon[j]) + normal_lpdf(mu[i,j] | tau[j]-delta[j], 1)); //Fixed sd as otherwise think MLE posterior would be 0
    }
  }
}


