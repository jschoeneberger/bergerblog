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

This gives us a smaller version of a data file like we might have when analyzing data from a research study: an outcome variable _y_, an independent variable _x_, two covariates of interest _c1_ and _c2_ (the latter a binary), and a numeric and alphanumeric representation of treatment group _tx_ and _tx_a_, respectively. For our purposes, we are interested in assessing the baseline equivalence of the two covariates _c1_ and _c2_. 

```r
ggplot(rd_dat, aes(x = X_c, y = Y_adj, color = tx_a)) + 
  geom_point(size=2,alpha = 0.25) + 
  geom_smooth(aes(fill = tx_a), method = "loess") + 
  geom_vline(xintercept = 0, linetype = "dashed", color="black", size=1) +
  theme_classic() +
  labs(x="Centered Assignment", y = "Y", color = "Treatment", fill="Treatment") + 
  scale_x_continuous(limits = c(-round(max(abs(rd_dat$X_c))), round(max(abs(rd_dat$X_c))))) +
  scale_y_continuous(limits = c(-ceiling(max(abs(rd_dat$Y_adj))), ceiling(max(abs(rd_dat$Y_adj))))) +
  scale_color_brewer(palette="Set1", direction=-1) + scale_fill_brewer(palette="Set1", direction=-1)
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
Morgan, K.L. & Rubin, D.B. (2015). Rerandomization to Balance Tiers of Covariates. _Journal of the American Statistical Association (JASA), 110(512),_ 1412-1421. DOI: <a href="https://doi.org/10.1080/01621459.2015.1079528">10.1080/01621459.2015.1079528</a> 

Morgan, K.L. & Rubin, D.B. (2012). Rerandomization to Improve Covariate Balance in Experiments. _Annals of Statistics, 40(2),_ 1262-1282. DOI: <a href="https://doi.org/10.1214/12-AOS1008">10.1214/12-AOS1008</a>

