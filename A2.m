% Exercise 3 (b)
%import atlantic file
atlantic = importdata('atlantic.txt');
%number of rows in the data
n = size(atlantic, 1);

%Maximum Liklehood estimates using est_gumbel function
out = est_gumbel(atlantic);
beta = out(1);
mu   = out(2);

%simulate a sample of Gumbel data of size atlantic data
u = rand(n,1); % 582 uniforms in (0,1)
x = mu - beta*log(-log(u)); % Gumbel draws

%qqplot to compare the distribution of the atlantic data and the simulated data
qqplot(atlantic,x);

% Exercise 3 (c)
% the number of bootstrap simulations
B = 10000; 
mu_est = zeros(B,1);  % initialize a vector of zeroes where we store mu's
beta_est = zeros(B,1);  % initialize a vector of zeroes where we store beta's

% set a seed, for reproducibility
rng(123);
for ii=1:B
    % simulate bootstrapped data
    u = rand(n,1); % 582 uniforms in (0,1)  
    x = mu - beta*log(-log(u)); % Gumbel draws
    out = est_gumbel(x);
    mu_est(ii) = out(2);
    beta_est(ii)= out(1);
end

% use the percentile method to obtain 95% confidence bounds
mu_CI = prctile(mu_est,[2.5,97.5]);
beta_CI = prctile(beta_est,[2.5,97.5]);



% Exercise 3 (d)
u = 1 - (1/(3*14*100));
xx = mu_est - beta_est*log(-log(u));
highest_wave  = prctile(xx,[2.5,97.5]);


% Exercise 3 (f)
% the number of bootstrap simulations
B = 10000; 
np_mu_est = zeros(B,1);  % initialize a vector of zeroes where we store mu's
np_beta_est = zeros(B,1);  % initialize a vector of zeroes where we store beta's

% set a seed, for reproducibility
rng(123);
for ii=1:B
    % obtain(nonparametric) bootstrap sample with replacement
    replace = 1;
    sample = randsample(atlantic,n,replace);
    out = est_gumbel(sample);
    np_mu_est(ii) = out(2);
    np_beta_est(ii)= out(1);
end

% use the percentile method to obtain 95% confidence bounds
np_mu_CI = prctile(np_mu_est,[2.5,97.5]);
np_beta_CI = prctile(np_beta_est,[2.5,97.5]);



