---
date: "2022-26-02"
diagram: true
highlight: true
title: Assessing Baseline Equivalence Using What Works Clearinghouse Standards
tags: ["RCT", "WWC", "What Works Clearinghouse", "random assignment", "baseline equivalence", "causal inference", "covariate balance"]
---

Research studies using random assignment are considered the "gold standard" because they yield balance on both observed and unobserved covariates between treatment groups. A small conceptual detail that is often left behind, however is that this balance is achieved _in expectation_. The reality is that imbalance among covariates between groups, sometimes called unhappy randomization, occurs more frequently than we'd like to admit. In fact, it occurs frequently enough that methodologists have developed methods for _re_-randomizing to achieve covariate balance (see Morgan & Rubin, 2012; 2015).

The <a href="https://ies.ed.gov/">Institute for Education Sciences'</a><a href="https://ies.ed.gov/ncee/wwc/">What Works Clearinghouse (WWC)</a> reviews education research studies for the purpose of determining whether rigorous methods have been used and summarizing findings for education practitioners. The agency is effectively trying to determine "what works in education?" The <a href="https://www.dol.gov/">U.S. Department of Labor</a> maintains the <a href="https://clear.dol.gov/">Clearinghouse for Labor Evaluation and Research (CLEAR)</a> for similar purposes. The WWC continuously develops and maintains 
<a href="https://ies.ed.gov/ncee/wwc/Docs/referenceresources/WWC-Standards-Handbook-v4-1-508.pdf">standards</a> for a number of research designs that can be used to assess research rigor and quality. One of the key research study aspects the standards covers is the assessment of baseline equivalence. 

## Baseline Equivalence

The WWC establishes review protocols for various content areas (e.g., Early Childhood, Reading, etc.). These protocols outline how reviewers should examine research studies related to the content area. With regard to baseline equivalence, the protocol will list characteristics (covariates) that should be assessed for equivalence between treatment and comparison groups at _baseline_ (prior to the introduction of the intervention). For example, if a research study is examining the impact of a middle school math intervention on math achievement (say, scores on a standardized math assessment), then the review protocol will likely specify that groups should ideally be balanced on the same, or similar, standardized math assessment. Baseline equivalence of the pre-intervention measure must be assessed for the _analytic sample_: the set of subjects from the intervention and comparison groups used to estimate outcomes.

### Code
We begin by simulating some data to work with, representing the analytic sample for a fictious RCT study:

```r
#set up packages
Packages <- c("mvtnorm","tidyverse","aod")
invisible(lapply(Packages, library, character.only = TRUE))

#simulate some data for running the be_eq function
#n = number of records/units
#tx_eff = treatment effect (in sd units; e.g. .5 = 1/2 SD)
#tx_eff_sd = treatment effect standard deviation
be_sim = function(n, tx_eff, tx_eff_sd){
  #set number of units to sim
  n_units <- n
  
  #set seed
  set.seed(1234)
  
  ##create correlation matrix
  r_mat <- matrix(0, nrow=3, ncol=3) 
  colnames(r_mat) <- c("y","x","c1")
  
  ##specify desired correlations in lower left triangle
  r_mat[2,1] <- .25; r_mat[3,1] <- .15; 
  r_mat[3,2] <- .10;
  
  #use transpose and add to upper triangle
  r_mat <- r_mat+t(r_mat)
  diag(r_mat) <-1
  
  ##generate raw correlated data
  ##set 1 row 6 column matrix of means equal to 0
  mu <- rep(0,3)
  raw_dat_mat = rmvnorm(n_units,mean=mu,sigma=r_mat)
  colnames(raw_dat_mat) <- c("y","x","c1")
  
  ##create data frame
  raw_dat_frm <- data.frame(raw_dat_mat)
  
  ##rank records based on the known need, where higher value is more need:
  raw_dat_frm$xrank <- rank(raw_dat_frm$x, ties.method= "first")  # first occurrence wins
  raw_dat_frm$tx <- as.numeric(1)
  raw_dat_frm$tx[raw_dat_frm$xrank > n_units/2] <- as.numeric(0)
  
  #here we add a positive effect to high-need and a negative effect to low need
  raw_dat_frm <- raw_dat_frm %>%
    rowwise() %>%
    mutate(c2 = ifelse(tx==1, rbinom(n=1, size=1, prob=0.51), rbinom(n=1, size=1, prob=0.49) )) %>%
    ungroup() %>% 
    mutate(tx_eff_adj = case_when(tx==1 ~ rnorm(1, mean=tx_eff, sd=tx_eff_sd),
                                  (tx==0) ~ rnorm(1, mean=((tx_eff*-1)), sd=tx_eff_sd),
                                  TRUE ~ rnorm(1, mean=(tx_eff*-1), sd=tx_eff_sd) ),
           c1 = c1 + tx_eff_adj) 
}
be_dat = replicate(1, be_sim(n = 100, tx_eff=.05, tx_eff_sd=0), simplify = FALSE)
be_dat <- data.frame(be_dat)  
be_dat <- be_dat %>% mutate(tx_a = case_when(tx == 0 ~ "CT", tx == 1 ~ "TX")) %>% 
  dplyr::select(y,x,c1,c2,tx,tx_a)
```

This gives us a small version of a data file like we might have when analyzing data from a research study: an outcome variable _y_, an independent variable _x_, two covariates of interest _c1_ and _c2_ (the latter a binary), and a numeric and alphanumeric representation of treatment group _tx_ and _tx_a_, respectively. For our purposes, we are interested in assessing the baseline equivalence of the two covariates _c1_ and _c2_. 

Now we'll step through chunks of the baseline equivalence function. First, we see the function takes four input arguments: the data file to be analyzed (in_dat), an argument representing the variable we want to assess for equivalence (dv), the variable denoting the treatment groups (iv), and the type of model based on the dv (either "t" for continuous variables or "l" for binary variables). Note that the function, as I've specified it, assumes a character representation of the intervention groups where control/comparison/BAU is coded as "CT" and treatment/intervention is coded as "TX". The initial step is to split the file into separate intervention groups. The function also assumes that the data frame specified as the in_dat argument represents the analytic sample (i.e., the sample that would be used to estimate the primary outcome of interest, _y_). Effectively, this data frame should already have accounted/removed records with missing data, etc.

```r
##baseline equivalence function for continuous and binary covariates
be_eq = function(in_dat, dv, iv, model){
  #split data by tx-ct indicator
  ct_dat <- in_dat %>% filter(in_dat[[iv]] == "CT")
  tx_dat <- in_dat %>% filter(in_dat[[iv]] == "TX")
```
The next chunk of code compiles information for when the model "t" is specified for an assessment of equivalence for a continuous variable. The first step creates separate vectors based on the inputs to the function to conduct a t-test to compare the two groups. We then summarize data contained in each of the separate data frames we created in the previous step: the number of records in the file, as well as the mean and standard deviation of the variable of interest. We also calculate Hedges' _g_ (1981) with a bias correction for small sample sizes $\omega = [(1-3/(4*(tx_n+ct_n)-9))] (Borenstein & Hedges, 2019). We then apply the WWC's ranges for baseline equivalence effect sizes (ie., Satisfies, Stat Adjust or Unsatisfied; see Table II.2 in the <a href="https://ies.ed.gov/ncee/wwc/Docs/referenceresources/WWC-Standards-Handbook-v4-1-508.pdf">WWC Standards Handbook V4.1</a>).

```r
if (model == 't') {
    #set vectors for t-test
    y <- in_dat[[dv]]
    x <- in_dat[[iv]]
    t <- t.test(y~x)
    
    #summarize data, calculate hedges and labels
    hedge <- bind_cols(ct_dat %>% 
                         mutate(covar = dv,
                          ct_n = n(), ct_mn = round(mean(ct_dat[[dv]],na.rm=T),3), ct_sd = round(sd(ct_dat[[dv]],na.rm=T)),3) %>%
                         distinct(across(c(covar, ct_n, ct_mn, ct_sd))),
                       tx_dat %>% 
                         mutate(tx_n = n(), tx_mn = round(mean(tx_dat[[dv]],na.rm=T),3), tx_sd = round(sd(tx_dat[[dv]],na.rm=T)),3) %>%
                         distinct(across(c(tx_n, tx_mn, tx_sd))) ) %>% 
      mutate(hedge_g = round((tx_mn - ct_mn)*(1-3/(4*(tx_n+ct_n)-9)) / 
                               sqrt((((ct_n-1)*(ct_sd**2)) + ((tx_n-1)*(tx_sd**2)))/(ct_n + tx_n - 2)), 3),
             base_eq = case_when(abs(hedge_g) >= 0 & abs(hedge_g) <= .05 ~ "Satisfied",
                                 abs(hedge_g) > 0.05 & abs(hedge_g) <= .25 ~ "Stat Adjust",                       
                                 abs(hedge_g) > 0.25 ~ "Unsatisfied"),
             t_val = round(as.numeric(t$statistic),3),
             t_df = round(as.numeric(t$parameter),3),
             t_pval = round(t$p.value,3)  ) 
  }
```

The last piece of code summarizes data when the variable of interest is binary. These steps are generally the same, though a logistic regression is conducted for obtaining an inferential assessment of the difference, the formula for the standard deviation of a binary variable is used, and the formula for the effect size (the Cox index) makes use of the logistic regression coefficient (Sanchez-Meca et al., 2003).

```r
  if (model == 'l') {
    #aod package for conducting wald test
    require(aod)
    #run logistic regression using binary baseline variable as outcome
    log_mod <- glm(in_dat[[dv]] ~ in_dat[[iv]], data = in_dat, family = "binomial")
    lor <- round(as.numeric(coef(log_mod)[2]),5)
    wald <- wald.test(b = coef(log_mod), Sigma = vcov(log_mod), Terms = 2)
    hedge <- bind_cols(ct_dat %>% 
                         mutate(covar = dv, ct_n = n(), ct_mn = round(mean(ct_dat[[dv]],na.rm=T)),3) %>%
                         mutate(ct_sd = round(sqrt(ct_mn*(1-ct_mn))),3) %>%
                         distinct(across(c(covar, ct_n, ct_mn, ct_sd))),
                       tx_dat %>% 
                         mutate(tx_n = n(), tx_mn = round(mean(tx_dat[[dv]],na.rm=T)),3) %>% 
                         mutate(tx_sd = round(sqrt(tx_mn*(1-tx_mn))),3) %>% 
                         distinct(across(c(tx_n, tx_mn, tx_sd))) ) %>% 
      mutate(cox_d = round((1-3/(4*(tx_n+ct_n)-9))*(lor/1.65), 3),
             base_eq = case_when(abs(cox_d) >= 0 & abs(cox_d) <= .05 ~ "Satisfied",
                                 abs(cox_d) > 0.05 & abs(cox_d) <= .25 ~ "Stat Adjust",                       
                                 abs(cox_d) > 0.25 ~ "Unsatisfied"),
             wchi_val = round(as.numeric(wald$result[[1]][1]),3),
             wchi_df = round(as.numeric(wald$result[[1]][2]),3),
             wchi_pval = round(as.numeric(wald$result[[1]][3]),3)  )   
  }
  return(hedge)
}
```
If we call the function using our simulated data to assess the baseline equivalence of _c1_, we get the output shown below. The absolute value of the effect size is greater than 0.25, resulting in a baseline equivalence assessment of 'Unsatisfied'. The accompanying t-test output shows the difference between the comparison and treatment groups to be statistically significant, with the mean outcome larger for the comparison group. 

```r
be_eq(in_dat=be_dat, dv="c1", iv="tx_a", model="t")

  covar ct_n ct_mn ct_sd tx_n  tx_mn tx_sd hedge_g     base_eq t_val   t_df t_pval
1    c1   50 0.304     1   50 -0.334     1  -0.633 Unsatisfied 3.058 97.734  0.003
```

We can also assess the binary covariate _c2_ using the same function call by specifying model = "l". The absolute value of the effect size for c2 was just under 0.25, resulting in a label of 'Statistical Adjustment'. This variable would have to be included in the final outcome estimate models to align with WWC standards. 

```r
be_eq(in_dat=be_dat, dv="c2", iv="tx_a", model="l")

  covar ct_n ct_mn ct_sd tx_n tx_mn tx_sd cox_d     base_eq wchi_val wchi_df wchi_pval
1    c2   50     0     0   50     1     0 0.241 Stat Adjust    0.997       1     0.318
```
### Math

Academic supports a Markdown extension for $\LaTeX$ math. You can enable this feature by toggling the `math` option in your `config/_default/params.toml` file.

To render *inline* or *block* math, wrap your LaTeX math with `$...$` or `$$...$$`, respectively.

Example **math block**:

```latex
$$\gamma_{n} = \frac{ 
\left | \left (\mathbf x_{n} - \mathbf x_{n-1} \right )^T 
\left [\nabla F (\mathbf x_{n}) - \nabla F (\mathbf x_{n-1}) \right ] \right |}
{\left \|\nabla F(\mathbf{x}_{n}) - \nabla F(\mathbf{x}_{n-1}) \right \|^2}$$
```

renders as

$$\gamma_{n} = \frac{ \left | \left (\mathbf x_{n} - \mathbf x_{n-1} \right )^T \left [\nabla F (\mathbf x_{n}) - \nabla F (\mathbf x_{n-1}) \right ] \right |}{\left \|\nabla F(\mathbf{x}_{n}) - \nabla F(\mathbf{x}_{n-1}) \right \|^2}$$

Example **inline math** `$\nabla F(\mathbf{x}_{n})$` renders as $\nabla F(\mathbf{x}_{n})$.

Example **multi-line math** using the `\\\\` math linebreak:

```latex
$$f(k;p_{0}^{*}) = \begin{cases}p_{0}^{*} & \text{if }k=1, \\\\
1-p_{0}^{*} & \text{if }k=0.\end{cases}$$
```

renders as

$$f(k;p_{0}^{*}) = \begin{cases}p_{0}^{*} & \text{if }k=1, \\\\
1-p_{0}^{*} & \text{if }k=0.\end{cases}$$

## References
Borenstein, M. & Hedges, L. V. (2019). Effect sizes for meta-analysis. In H. Cooper, L. V. Hedges, & J. C. Valentine (Eds.), _The handbook of research synthesis and meta-analysis_ (3rd ed., pp. 207–244). New York, NY: Russell Sage Foundation.

Hedges, L. V. (1981). Distribution theory for Glass’s estimator of effect size and related estimators. _Journal of Educational and Behavioral Statistics, 6(2),_ 107–128. DOI: <a href="https://doi.org/10.3102/10769986006002107">10.3102/10769986006002107</a>

Morgan, K.L. & Rubin, D.B. (2015). Rerandomization to Balance Tiers of Covariates. _Journal of the American Statistical Association (JASA), 110(512),_ 1412-1421. DOI: <a href="https://doi.org/10.1080/01621459.2015.1079528">10.1080/01621459.2015.1079528</a> 

Morgan, K.L. & Rubin, D.B. (2012). Rerandomization to Improve Covariate Balance in Experiments. _Annals of Statistics, 40(2),_ 1262-1282. DOI: <a href="https://doi.org/10.1214/12-AOS1008">10.1214/12-AOS1008</a>

Sanchez-Meca, J., Marin-Martinez, F., & Chacon-Moscoso, S. (2003). Effect-size indices for dichotomous outcomes in meta-analysis. _Psychological Methods, 8(4),_ 448–467. DOI: <a href="https://doi.org/10.1037/1082-989X.8.4.448">10.1037/1082-989X.8.4.448</a>

