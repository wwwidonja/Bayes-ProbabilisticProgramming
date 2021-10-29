data {
  // independent
  int<lower=1> n; // total number of data points
  vector<lower=0>[n] research;
  vector<lower=0>[n] marketing;
  vector<lower=0>[n] admin;
  
  //dependent
  vector<lower=0>[n] profit;
}

parameters {
  real br; // research
  real ba; //admin
  real bm; //admin
  real<lower=0> sigma; // stdev
}

model {
  // beta prior
  br ~ normal(0, 20);
  ba ~ normal(0, 20);
  bm ~ normal(0, 20);
  sigma ~ cauchy(0, 20);
  
  // model
  profit ~ normal(research * br + admin * ba + marketing * bm, sigma);
}
