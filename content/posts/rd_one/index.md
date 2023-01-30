---
title: "Regression Discontinuity in R (Part 1): Introduction and Working With Multi-Site, Different Scale Assignment Variables"
diagram: yes
date: '2023-01-02'
math: yes
highlight: yes
tags:
- Regression Discontinuity
- WWC
- R
- What Works Clearinghouse
- multi-site
- rdrobust
---

Random assignment research designs are considered the "gold standard" by methodologists because they yield balance on both observed and unobserved covariates between treatment groups, _in expectation_. However, RCTs tend to be more costly, take more time to manage, and are often not politically palatable to educational leaders and policymakers. Educational leaders do not wish to deny students, teachers, or schools a potentially beneficial intervention, even if only for a short time as in delayed-intervention designs (Shadish, Cook & Campbell, 2002). Local or State Education Agencies (LEAs; SEAs) must make use of resources to support schools and students most in need, particularly when there is a substantial amount of variability in ‘need’ among schools (Hitt, Robinson, & Player, 2018; The Center on School Turnaround, 2017). Decisions for resource allocation must often be made quickly, leaving little time for considering research design and evaluation concepts. 

Regression Discontinuity (RD or RDDs; Cattaneo, Idrobo & Titiunik, 2018a; 2018b; Cook, Shadish and Wong, 2008; Hahn, Todd & van der Klaauw, 2001; Imbens & Lemieux, 2008a; Jacob & Lefgren, 2004a; Ludwig & Miller, 2007; Shadish, Cook & Campbell, 2002) is an increasingly popular design with internal validity approaching that of an RCT. In an RD design, units are identified for intervention exposure based on whether their value on a measure falls above (or below) some threshold cut-point. Units on one side of the cut-point receive the intervention, those on the other side do not and serve as the comparison group. Inclusion of the assignment variable in a regression model accounts for unmeasured differences that might exist between the two groups (Angrist and Lavy, 1999; Hahn, Todd, and van der Klaauw, 2001; Imbens & Kalyanaraman, 2009; Thistlethwaite & Campbell, 1960). Determining intervention receipt using a score is appealing to educational leaders because they can target those (students, teachers, schools) most in need while still allowing for a rigorous estimate of the intervention impact.

This series of posts was motivated by my work on the <a href="https://new.every1graduates.org/projects/alfalab/#:~:text=WHAT%20IS%20ALFA%20LAB%3F,significantly%20below%20grade%20level%20standards">
Accelerating Literacy for Adolescents (ALFA) Lab </a> evaluation funded by the U.S Department of Education's <a href="https://ies.ed.gov/">Institute for Education Sciences' </a> (R305A180154). In collaboration with researchers from <a href="https://education.jhu.edu/">Johns Hopkins School of Education </a>, I developed a number of R functions and scripts to analyze RD-based outcomes. My intent is to share what I learned with others seeking to use an RD design in their work.

This first post will showcase how we handled a multi-site RD design, where each site had a unique assignment variable (as presented at the <a href="https://www.eval.org/">American Evaluation Association</a> 2022 conference). The post will also provide sample code for exploratory RD plotting, impact estimation, and post-estimation RD plots. Additional posts will provide code for generating evidence aligned with href="https://ies.ed.gov/ncee/wwc/">What Works Clearinghouse (WWC)</a> href="https://ies.ed.gov/ncee/wwc/Docs/referenceresources/WWC-HandbookVer5.0AppIES-508.pdf">Standards 5.0</a> and will also explore mediation and moderation analyses within an RD design context. 

## Preparing Multi-Site, Multi-Assignment Variable Data

Let's start by generating some basic descriptives of our data, by site. Below are simple summaries for our outcome variable _Y_, the assignment/running variable _X_, the treatment/intervention indicator _tx_, and two covariates _C1_ and _C2_. For now, we ignore the _Med_ and _Mod_ variables.

```r
#set up packages
pckgs <- c("tidyverse","rdrobust","rddensity","rdlocrand","ggplot2")
invisible(lapply(pckgs, library, character.only = TRUE))

#generate descriptives of data 
rd_desc <- rd_dat %>% dplyr::select(Y, X, tx, C1, C2, site) %>% group_by(site)
descr(rd_desc, stats=c("mean", "sd", "min", "med","max"), order="p", transpose=T)

Descriptive Statistics  
rd_desc  
Group: site = 1  
N: 200  

            Mean   Std.Dev     Min   Median    Max
-------- ------- --------- ------- -------- ------
       Y    0.09      1.02   -2.37     0.04   2.84
       X   -0.04      1.11   -2.61    -0.05   2.93
      tx    0.52      0.50    0.00     1.00   1.00
      C1   -0.02      1.00   -2.70    -0.03   3.10
      C2   -0.01      1.02   -2.52     0.00   2.98

Group: site = 2  
N: 200  

             Mean   Std.Dev     Min   Median      Max
-------- -------- --------- ------- -------- --------
       Y     0.08      0.98   -2.62     0.13     2.61
       X   100.76      9.60   74.59   101.18   135.46
      tx     0.48      0.50    0.00     0.00     1.00
      C1     0.03      0.90   -2.84    -0.01     2.44
      C2     0.05      0.99   -2.13     0.02     2.88

Group: site = 3  
N: 200  

            Mean   Std.Dev     Min   Median     Max
-------- ------- --------- ------- -------- -------
       Y    0.00      1.00   -2.40     0.07    2.52
       X   20.11      3.98   10.68    20.14   30.93
      tx    0.50      0.50    0.00     1.00    1.00
      C1   -0.06      1.02   -2.64    -0.05    3.49
      C2   -0.05      0.99   -2.67     0.01    3.11
```

The most important thing to take note of in this summary are the means of _X_, by site. Clearly, each site has a distinct assignment variable determining which units receive the intervention, and which do not. The WWC Standards 5.0 outlines two options for handling site-based RD estimates: pooling or aggregation. Pooling combines the raw data for analysis yielding a single estimate. Aggregation involves the generation of separate estimates and then computing a weighted average of the estimates. For our project, we chose to pool the raw data. To accomplish this, we standardized and centered the assignment variable. Centering can be done using the individual cut-scores identified prior to assignment. Here we automatically center using the largest value associated with treatment units (tx = 1):  

```r
#standardize assignment score (z-score) by site
  #identify max z-version value by site for tx group
  #add small amt to max for tx group to ensure they are less than zero
  #center the z-version around the maximum value
rd_dat <- rd_dat %>% group_by(site) %>% 
  mutate(run_z = round((X - mean(X, na.rm=T))/sd(X, na.rm=T),5)) %>% group_by(site, tx) %>%
  mutate(x_max_tmp = max(run_z, na.rm=TRUE),
         x_max_tmp = max(case_when(tx==1 ~ x_max_tmp + 0.00001)) ) %>% group_by(site) %>%
  mutate(x_max = max(x_max_tmp, na.rm=TRUE), 
         run_z_c = (run_z - x_max)) %>% ungroup() %>% dplyr::select(-c(x_max_tmp,x_max) )
```
Below is a snapshot of the rd_dat data file displaying data for site 1, sorted in ascending order of the running variable _X_. The _run_z_ variable is the z-scored version of _X_, and _run_z_c_ is the centered version of _run_z_. Note the third case in the snapshot, with a value of -0.00001. This is because its _run_z_ value of 0.05357 is the largest (i.e., maximum) value for a site 1 treatment unit. Remember we added a small amount to ensure that treatment units were less than zero. This is because the RDROBUST package is parameterized such that assignment scores greater than _or equal to_ zero are intervention units. 

![](/images/rd_run_z_c.png)

## Raw Data Scatterplot

With our appropriately scaled and centered assignment variable, we can plot the raw data to see the basic relationship between the assignment variable and our outcome _Y_.

```r
#generate plot
ggplot(rd_dat, aes(x = run_z_c, y = Y, color = tx_a)) + 
  geom_point(size=2,alpha = 0.25) + 
  geom_smooth(aes(fill = tx_a), method = "loess") + 
  geom_vline(xintercept = 0, linetype = "dashed", color="black", linewidth=1) +
  theme_classic() +
  labs(x="Centered Assignment", y = "Y", color = "Treatment", fill="Treatment") + 
  scale_x_continuous(limits = c(-ceiling(max(abs(rd_dat$run_z_c))),  ceiling(max(abs(rd_dat$run_z_c))))) +
  scale_y_continuous(limits = c(-ceiling(max(abs(rd_dat$Y))), ceiling(max(abs(rd_dat$Y))))) +
  scale_color_brewer(palette="Set1", direction=-1) + scale_fill_brewer(palette="Set1", direction=-1)
```

![](/images/rd_raw_plot.png)

Note the code above automatically sets the limits for the X and Y axes, colors the data according to the value of the _tx_a_ variable, and includes a smooth loess curve for reference. Based on the raw data, it appears there is a discontinuity at the cut-point of zero, as the red (treatment) line intersects the dashed cut-point line at a point higher on the Y-axis than does the blue (control) line. 

## Smoothing To Detect Discontinuities

Though the scatterplot shown above seems to show a pretty clear discontinuity, this may not always be the case. Effectively, the raw data are binned into intervals along the assignment variable continuum. These bins can take various forms, including quantile-spaced (QS) and evenly-spaced (ES) bins. QS bins contain the same number of observations in each bin. ES bins are equal in length along the assignment score continuum. There are also two methods for determining the number of bins: integrated mean-squared error (IMSE) and mimicking variance (MV). The former is an asymptotic approximation of the MSE resulting from approximating individual data points with a constant, the latter chooses binned means yielding variability similar to that seen in the raw data. 

#### Baseline Equivalence Function

We also calculate Hedges' _g_ (1981) with a bias correction for small sample sizes $\omega$ = [(1-3/(4*(tx_n+ct_n)-9))] (Borenstein & Hedges, 2019). 



### Other Software
Here is a link to the simulated data file:
[be_eq_data](/be_eq_data.xlsx)



## References
Borenstein, M. & Hedges, L. V. (2019). Effect sizes for meta-analysis. In H. Cooper, L. V. Hedges, & J. C. Valentine (Eds.), _The handbook of research synthesis and meta-analysis_ (3rd ed., pp. 207–244). New York, NY: Russell Sage Foundation.

Hedges, L. V. (1981). Distribution theory for Glass’s estimator of effect size and related estimators. _Journal of Educational and Behavioral Statistics, 6(2),_ 107–128. DOI: <a href="https://doi.org/10.3102/10769986006002107">10.3102/10769986006002107</a>

Morgan, K.L. & Rubin, D.B. (2015). Rerandomization to Balance Tiers of Covariates. _Journal of the American Statistical Association (JASA), 110(512),_ 1412-1421. DOI: <a href="https://doi.org/10.1080/01621459.2015.1079528">10.1080/01621459.2015.1079528</a> 

Morgan, K.L. & Rubin, D.B. (2012). Rerandomization to Improve Covariate Balance in Experiments. _Annals of Statistics, 40(2),_ 1262-1282. DOI: <a href="https://doi.org/10.1214/12-AOS1008">10.1214/12-AOS1008</a>

Sanchez-Meca, J., Marin-Martinez, F., & Chacon-Moscoso, S. (2003). Effect-size indices for dichotomous outcomes in meta-analysis. _Psychological Methods, 8(4),_ 448–467. DOI: <a href="https://doi.org/10.1037/1082-989X.8.4.448">10.1037/1082-989X.8.4.448</a>

