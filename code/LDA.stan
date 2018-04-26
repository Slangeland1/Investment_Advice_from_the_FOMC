data {
  int<lower=2> K;               // num topics
  int<lower=2> V;               // num words
  int<lower=1> M;               // num docs
  int<lower=1> N;               // total word instances [length(dtm)]
  int<lower=1,upper=V> w[N];    // number of times word n appears [c(dtm)]
  int<lower=1,upper=M> doc[N];  // doc ID for word n (rep(1:M, each = V))
  real<lower=0> alpha_mean;     // hyperparameter for prior on alpha
  vector<lower=0>[V] beta;      // hyperparameter for prior on beta

  matrix[M, V] DTM; // this should be normalized so that each row sums to 1

  int<lower=1> P; // number of stocks
  matrix[M, P] Y; // price changes
  
  // stuff for priors on alpha, beta, and sigma
}
transformed data {
  real<lower=0> alpha_rate = inv(alpha_mean);
}
parameters {
  // LDA part
  positive_ordered[K] alpha;     // topic prior
  simplex[K] theta[M];   // topic dist for doc m
  simplex[V] phi[K];     // word dist for topic k

  // stock market part
  real alpha_stock;          // intercept
  vector[V] beta_stock_raw;  // standardized influence of each word in the vocabulary
  real<lower=0> tau;         // standard deviation in coefficient across words
  real<lower=0> sigma;       // error standard deviations
}
transformed parameters {
  vector[V] beta_stock = beta_stock_raw * tau; // unstandardized
  vector<lower=negative_infinity(),upper=positive_infinity()>[M] mu;
  {
    matrix[M, V] res;
    for (m in 1:M) { // create topic-weighted residuals in word-proportions
      row_vector[V] res_m = rep_row_vector(0, V);
      vector[K] theta_m = theta[m];
      for (k in 1:K) res_m += (DTM[m,] - phi[k]') * theta_m[k];
      res[m,] = res_m;
    }
    mu = alpha_stock + res * beta_stock;
  }
}
model {
  // LDA part
  alpha ~ exponential(alpha_rate);
  for (m in 1:M)  
    theta[m] ~ dirichlet(alpha);  // prior
  for (k in 1:K)  
    phi[k] ~ dirichlet(beta);     // prior
  for (n in 1:N) {
    vector[K] gamma = log(theta[doc[n]]);
    int w_n = w[n];
    for (k in 1:K) 
      gamma[k] += log(phi[k,w_n]);
    target += log_sum_exp(gamma);  // likelihood
  }

  // stock markets part
  
  // priors on alpha_stock, beta_stock, and sigma
  target += normal_lpdf(alpha_stock | 0, 1.5);
  target += normal_lpdf(beta_stock_raw | 0, 1);
  target += exponential_lpdf(tau | 1);
  target += exponential_lpdf(sigma | 1);
  
  for (p in 1:P) { // likelihood of alpha_stock, beta_stock, and sigma at observed Y
    target += normal_lpdf(Y[,p] | mu, sigma);   
  }
}
generated quantities {
  matrix[M, P] Y_tilde;
  for (p in 1:P) {
    for (m in 1:M) Y_tilde[m,p] = normal_rng(mu[m], sigma);
  }
}

