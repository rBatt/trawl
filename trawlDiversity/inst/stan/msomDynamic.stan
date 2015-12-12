
// ========
// = Data =
// ========
data {
    int<lower=1> nT; // number of years
    int<lower=1> Kmax; // max samples in a site-year
    int<lower=1> Jmax; // number of sites
    int<lower=0, upper=Jmax> nJ[nT]; // number of sites in each year
    int<lower=0, upper=Kmax> nK[nT,Jmax]; // number of samples (replicates/ hauls)
    int nU; // number of Covariates for psi (presence)
    int nV; // number of Covariates for theta (detection)
    
    int N; // total number of observed species (anywhere, ever)
    int<lower=1> nS; // size of super population, includes unknown species
    vector[nS] isUnobs[nT,Jmax]; // was a species unobserved (across all K) each site-year?
    
    int<lower=0, upper=nU> nU_c; // number of presence covariates that are constants
    int<lower=0, upper=nV> nV_c; // number of detection covariates that are contants
    int<lower=0, upper=nU> nU_rv; // number of presence covariates that are random variables (params)
    int<lower=0, upper=nV> nV_rv; // number of detection covariates that are random variables (params)
    matrix[Jmax, nU_c] U_c[nT]; // psi (presence) covariates that are consants
    matrix[Jmax, nV_c] V_c[nT]; // theta (detection) covariates that are consants
    matrix[Jmax,nU_rv] U_mu[nT]; // sample mean for psi covariates (U)
    matrix[Jmax,nU_rv] U_sd[nT]; // sample sd for U
    matrix[Jmax,nV_rv] V_mu[nT]; // sample mean for theta covariates (V)
    matrix[Jmax,nV_rv] V_sd[nT]; // sample sd for V
    
    int X[nT,Jmax,nS]; // species abundances
}


// ====================
// = Transformed Data =
// ====================
transformed data {
	int<lower=0, upper=(Jmax*nT)> nJ_sum;
	int<lower=0> n0;
	
	n0 <- nS - N;
	nJ_sum <- sum(nJ);
}


// ==============
// = Parameters =
// ==============
parameters { 
  real<lower=0, upper=1> Omega;
  
  vector[nU] alpha_mu; // hyperparameter mean
  vector<lower=0>[nU] alpha_sd; // hyperparameter sd
  matrix[nU,nS] alpha_raw; // non-centered presence coefficient
  
  vector[nV] beta_mu; // hyperparameter mean
  vector<lower=0>[nV] beta_sd; // hyperparameter sd
  matrix[nV,nS] beta_raw; // detection coefficient
  
  matrix[Jmax,nU_rv] U_raw[nT]; // predictors for psi, not including intercept
  matrix[Jmax,nV_rv] V_raw[nT]; // predictors for theta, not including intercept
  
	real phi_mu;
	real phi_sd;
	vector[nS] phi_raw; // peristence 
	
	real gamma_mu;
	real gamma_sd;
	vector[nS] gamma_raw; // colonization
	
	matrix[Jmax, nS] logit_psi_t1_raw; // starting probability of occupancy
}


// ==========================
// = Transformed Parameters =
// ==========================
transformed parameters {

  // ---- declare ----
  matrix[nU,nS] alpha; // presence coefficient
  matrix[nV,nS] beta; // detection coefficient

  matrix[Jmax, nU] U[nT]; // presence covariates
  matrix[Jmax, nV] V[nT]; // detection covariates
	
	matrix[Jmax, nS] logit_psi[nT]; // logit presence probability
	matrix[Jmax, nS] logit_theta[nT]; // logit detection probability  
	
	vector[nS] phi; // peristence 
	vector[nS] gamma; // colonization 
	
	matrix[Jmax, nS] logit_psi_t1;
	
  
  // ---- define ---- 
  // coefficients
  for (u in 1:nU) {
    alpha[u] <- alpha_mu[u] + alpha_sd[u]*alpha_raw[u];
  }
  for (v in 1:nV) {
    beta[v] <- beta_mu[v] + beta_sd[v]*beta_raw[v];
  }
	
	for (j in 1:Jmax) {
		for (s in 1:nS) {
			logit_psi_t1[j,s] <- 2 * logit_psi_t1_raw[j,s];
		}
	}
  
  // covariates
  for (t in 1:nT) { 
    matrix[Jmax, nU_rv] tU; // annual covariates, aside from intercept
    matrix[Jmax, nV_rv] tV;
    
    tU <- U_mu[t] + U_raw[t] .* U_sd[t]; // center
    tV <- V_mu[t] + V_raw[t] .* V_sd[t];
    
    U[t] <- append_col(U_c[t], tU); // add covaraites that are constants
    V[t] <- append_col(V_c[t], tV); // add constant detection covs
  }	
	
	// phi and gamma
	for (s in 1:nS) {
		phi[s] <- inv_logit(phi_mu + phi_raw[s] * phi_sd);
		gamma[s] <- inv_logit(gamma_mu + gamma_raw[s] * gamma_sd);
	}
	
	// psi and theta
	logit_psi[1] <- logit_psi_t1;
	logit_theta[1] <- V[1] * beta;
	for (t in 2:nT) {
		matrix[Jmax, nS] t_U_alpha;
				
		t_U_alpha <- U[t] * alpha; // used to be logit_psi[t]
		logit_theta[t] <- V[t] * beta;
		
		for (j in 1:Jmax) {
			for (s in 1:nS) {
				real tm1_psi;
				tm1_psi <- inv_logit(logit_psi[t-1][j,s]);
				logit_psi[t][j,s] <- logit(phi[s] * tm1_psi + gamma[s] * (1 - tm1_psi)) + t_U_alpha[j,s];
			}			
		}
		
		
	}
	 
}


// =========
// = Model =
// =========
model {
  
  // ---- Priors for Hyperparameters ----
	alpha_mu ~ cauchy(0, 1);
	alpha_sd ~ cauchy(0, 2);
  beta_mu ~ cauchy(0, 1);
  beta_sd ~ cauchy(0, 2);
	phi_mu ~ normal(0, 1);
	phi_sd ~ cauchy(0, 1);
	gamma_mu ~ normal(0, 1);
	gamma_sd ~ cauchy(0, 1);
	
	  
  // ---- Priors for Parameters ----
  Omega ~ beta(2,2);
  
  for (u in 1:nU) {
    alpha_raw[u] ~ normal(0, 1); // implies alpha ~ normal(alpha_mu, alpha_sd)
  }
  for (v in 1:nV) {
    beta_raw[v] ~ normal(0, 1); // implies beta ~ normal(beta_mu, beta_sd)
  }
  
  for (t in 1:nT) {
    for (j in 1:Jmax) { 
      U_raw[t][j] ~ normal(0, 1); // implies U ~ normal(U_mu, U_sd)
      V_raw[t][j] ~ normal(0, 1); // implies V ~ normal(V_mu, V_sd)
    }
  }
	
	for (j in 1:Jmax) {
		for (s in 1:nS) {
			logit_psi_t1_raw[j,s] ~ normal(0, 1); // implies logit_psi_t1[j,s] ~ normal(0, 2)
		}
	}
	
	for (s in 1:nS) {
		phi_raw[s] ~ normal(0, 1); // implies phi[s] ~ normal(phi_mu, phi_sd)
		gamma_raw[s] ~ normal(0, 1); // implies gamma[s] ~ normal(gamma_mu, gamma_sd)
	}
  
  
  // ---- Increment Omega for Known Species (Vectorized) ----
	increment_log_prob(bernoulli_log(1, Omega) * N);
	
	// ---- Species that are Known to Exist: Iterative Form ----
	for (n in 1:N) {
		// 1 ~ bernoulli(Omega); // iterative way to increment Omega for known species
		for (t in 1:nT) {
			for (j in 1:Jmax) {
				if (nK[t,j] > 0) {
					if (X[t,j,n] > 0) {
						increment_log_prob(log_inv_logit(logit_psi[t][j,n]) + binomial_logit_log(X[t,j,n], nK[t,j], logit_theta[t][j,n]));
						// increment_log_prob(bernoulli_logit_log(1, logit_psi[t][j,n]) + binomial_logit_log(X[t,j,n], nK[t,j], logit_theta[t][j,n])); // alternative to above
					} else {
						increment_log_prob(log_sum_exp(bernoulli_logit_log(1, logit_psi[t][j,n]) + binomial_logit_log(0, nK[t,j], logit_theta[t][j,n]), bernoulli_logit_log(0, logit_psi[t][j,n])));
					}
				}
			}
		}
	}
	
	// ---- Species that are Known to Exist: Vectorized Form ?? ----
  // for (t in 1:nT) { // loop through years
  //
  //   if(nJ[t]){ // statement only necessary if using failed approach for vectorization
  //
  //     for (j in 1:Jmax) { // sites
  //
  //       if(nK[t,j]){ // if samples in site
  //
  //         row_vector[N] t_logit_psi; // presence
  //         row_vector[N] t_logit_theta; // detection
  //         vector[N] lil_lp; // log_inv_logit(logit_psi)
  //         vector[N] l1mil_lp; // log1m_inv_logit(logit_psi)
  //
  //         t_logit_psi <- sub_row(logit_psi[t], j, 1, N);
  //         t_logit_theta <- sub_row(logit_theta[t], j, 1, N);
  //
  //         for (n in 1:N){
  //           l1mil_lp[n] <- log1m_inv_logit(t_logit_psi[n]);
  //           lil_lp[n] <- log_inv_logit(t_logit_psi[n]);
  //         }
  //
  //         increment_log_prob(lp_exists(
  //           segment(X[t,j], 1, N), // x
  //           nK[t,j], // K
  //           lil_lp, // vector lil_lp
  //           t_logit_theta, // row_vector logit_theta
  //           segment(isUnobs[t,j], 1, N), // vector isUnobs
  //           l1mil_lp // vector l1mil_lp
  //         ));
  //       } // if nK
  //     } // end site loop
  //   } // if nJ
  // } // end year loop


	// ---- Never-Observed Species: Iterative Form ----
	for (s in (N+1):nS) {
		real lp_unavailable; // unavail part of never obs prob
		real lp_available; // available part of never obs prob
		vector[nJ_sum] lp_available_pt1; // stores 'present but undetected' part of 'never obs but avail' term
		int pos;

		pos <- 1;

		for (t in 1:nT) {
			for (j in 1:Jmax) {
				if (nK[t,j] > 0) {
					lp_available_pt1[pos] <- log_sum_exp(log_inv_logit(logit_psi[t][j,s]) + binomial_logit_log(0, nK[t,j], logit_theta[t][j,s]), log1m_inv_logit(logit_psi[t][j,s]));
						// lp_available_pt1[pos] <- log_sum_exp(bernoulli_logit_log(1, logit_psi[t][j,s]) + binomial_logit_log(0, nK[t,j], logit_theta[t][j,s]), bernoulli_logit_log(0, logit_psi[t][j,s])); // alternative to above
					pos <- pos + 1;
				}
			}
		}
		lp_unavailable <- bernoulli_log(0, Omega);
		lp_available <- bernoulli_log(1, Omega) + sum(lp_available_pt1);
		increment_log_prob(log_sum_exp(lp_unavailable, lp_available));
	}
	

	// ---- Never Observed Species: Vectorized Form?? ----
 // for (t in 1:nT) { // loop through years
 //
 //    if(nJ[t]){ // statement only necessary if using failed approach for vectorization
 //
 //      for (j in 1:Jmax) { // sites
 //
 //        if(nK[t,j]){ // if samples in site
 //
 //          row_vector[n0] t_logit_psi; // presence
 //          row_vector[n0] t_logit_theta; // detection
 //          vector[n0] lil_lp; // log_inv_logit(logit_psi)
 //          vector[n0] l1mil_lp; // log1m_inv_logit(logit_psi)
 //
 //          t_logit_psi <- sub_row(logit_psi[t], j, (N+1), n0);
 //          t_logit_theta <- sub_row(logit_theta[t], j, (N+1), n0);
 //
 //          for (s in 1:n0){
 //            l1mil_lp[s] <- log1m_inv_logit(t_logit_psi[s]);
 //            lil_lp[s] <- log_inv_logit(t_logit_psi[s]);
 //          }
 //
 //          increment_log_prob(lp_never_obs(
 //            nK[t,j], // int[] K
 //            lil_lp, // vector lil_lp
 //            t_logit_theta, // row_vector logit_theta
 //            Omega, // real Omega
 //            l1mil_lp // vector l1mil_lp
 //          ));
 //        } // if nK
 //      } // end site loop
 //    } // if nJ
 //  } // end year loop
 //
} // end model


// ========================
// = Generated Quantities =
// ========================
// generated quantities {
// }

