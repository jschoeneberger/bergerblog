---
title: "Regression Discontinuity in R (Part 1): Introduction and Working With Multi-Site, Different Scale Assignment Variables"
diagram: yes
date: '2023-02-13'
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
Accelerating Literacy for Adolescents (ALFA) Lab </a> evaluation funded by the U.S Department of Education's <a href="https://ies.ed.gov/">Institute for Education Sciences' </a> (R305A180154). In collaboration with researchers from <a href="https://education.jhu.edu/">Johns Hopkins School of Education </a>, I developed a number of R scripts to analyze RD-based outcomes. My intent is to share what I learned with others seeking to use an RD design in their work.

This first post will showcase how we handled a multi-site RD design, where each site (and cohort in the actual study) had a unique assignment variable (as presented at the <a href="https://www.eval.org/">American Evaluation Association</a> 2022 conference). The post will also provide sample code for exploratory RD plotting, impact estimation, and post-estimation RD plots. This post is not intended to serve as an exploration into all things RD-estimation, but rather to share some developed code to facilitate the analysis and presentation of RD design data using <a href="https://rdpackages.github.io/rdrobust/">RDROBUST</a> and its affiliated packages. Additional posts will provide code for generating evidence aligned with <a href="https://ies.ed.gov/ncee/wwc/">What Works Clearinghouse (WWC)</a> <a href="https://ies.ed.gov/ncee/wwc/Docs/referenceresources/WWC-HandbookVer5.0AppIES-508.pdf">Standards 5.0</a> and will also explore mediation and moderation analyses within an RD design context. 

For those looking to learn more about RD estimation and explore more of the features associated with the various RD packages, consult the references listed as the end of this post. The authors of <a href="https://rdpackages.github.io/rdrobust/">RDROBUST</a> were very responsive and accessible as I worked through applying their packages to my research situation. See their <a href=" https://rdpackages.github.io/references/Cattaneo-Titiunik_2022_ARE.pdf">2022</a> review of RD designs for a nice introduction.



## Preparing Multi-Site, Multi-Assignment Variable Data

Let's start by generating some basic descriptives of our data, by site. 

Here is a link to the simulated data file: [rd_dat](/rd_dat.rds)

Below are simple summaries for our outcome variable _Y_, the assignment/running variable _X_, the treatment/intervention indicator _tx_, and two covariates _C1_ and _C2_. For now, we ignore the _Med_ and _Mod_ variables.

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

The most important thing to take note of in this summary are the means of _X_, by site. Clearly, each site has a distinct assignment variable determining which units receive the intervention, and which do not. The WWC Standards 5.0 outlines two options for handling site-based RD estimates: pooling or aggregation. Pooling combines the raw data for analysis yielding a single estimate. Aggregation involves the generation of separate estimates and then computing a weighted average of the estimates. For our project, we chose to pool the raw data. To accomplish this, we standardized and centered the assignment variable. Centering can be done using the individual cut-scores identified prior to assignment (Cattaneo, Titiunik, Vazquez-Bare, & Keele, 2016). Here we automatically center using the largest value associated with treatment units (tx = 1):  

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

We also go ahead and create dummy variables to represent the individual sites (so we can include these in our covariate object):
```
#create dummy variables representing site
rd_dat <- rd_dat %>% mutate(s1_dum = case_when (site == 1 ~ 1, site != 1 ~ 0),
                            s2_dum = case_when (site == 2 ~ 1, site != 2 ~ 0),
                            s3_dum = case_when (site == 3 ~ 1, site != 3 ~ 0))
```
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

Though the scatterplot shown above seems to show a pretty clear discontinuity, this may not always be the case. To enhance our ability to detect the overall pattern in the data (i.e., the likely regression form and discontinuities), the data can be 'smoothed'. Effectively, the raw data are binned into intervals along the assignment variable continuum. These bins can take various forms, including quantile-spaced (QS) and evenly-spaced (ES) bins. QS bins contain the same number of observations in each bin. ES bins are equal in length along the assignment score continuum. There are also two methods for determining the number of bins: integrated mean-squared error (IMSE) and mimicking variance (MV). The former is an asymptotic approximation of the MSE resulting from approximating individual data points with a constant, the latter chooses binned means yielding variability similar to that seen in the raw data. 

I've written a function to compile information about various bins for inspection. We begin by setting an object equal to available covariates that can eventually be used in the RD estimation process. We also set up lists of features that we wish to explore using RDPLOT, including the selection method, kernel (weighting) type, and polynomials. 

```
#set up z = covariates 
z = cbind(rd_dat$C1, rd_dat$C2, rd_dat$s1_dum, rd_dat$s2_dum, rd_dat$s3_dum)

#create lists of features we wish to explore using RDPLOT
#here we specify the selection method, kernel and polynomials
sel_lst <- c("esmv","qsmv","es","qs")
krn_lst <- c("uniform","triangular")
p_lst <- c(1,4)
all_lst <- expand.grid(sel_lst, krn_lst, p_lst)
```

Now we define the function that makes use of <a href="https://search.r-project.org/CRAN/refmans/rdrobust/html/rdplot.html/">RDPLOT</a>, a function for plotting data-driven RD plots. When executed, a separate plot will be generated for each combination of method, kernel and p (polynomial).

```
#bins_id is a function to compile identified bin numbers based on various specifications
if(exists("bins_id")) rm("bins_id", envir = globalenv())
bins_id <- do.call(rbind.data.frame,
            with(all_lst, Map(function(bin_sel, krn, p, dat, x, y, cut, n_bin) {
              floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
              ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
              x_lo <- floor_dec(min(dat[[x]]), 1)
              x_hi <- ceiling_dec(max(dat[[x]]), 1)
              x_lim <- max(abs(x_lo), abs(x_hi))
              y_lo <- floor_dec(min(dat[[y]]), 1)
              y_hi <- ceiling_dec(max(dat[[y]]), 1)
              y_lim <- max(abs(y_lo), abs(y_hi))
            
              #if(length(bin_sel) == 0) { print(bin_sel) } else { print(krn) }
              if(length(n_bin) > 0){ #this rdplot call runs when the number of bins is not specified
                  out_put <- rdplot(x = dat[[x]], y = dat[[y]], c = cut, binselect = bin_sel, kernel=krn, p=p, 
                                    x.lim = c(-x_lim,x_lim), y.lim = c(-y_lim,y_lim),
                                    title=paste0("Method=",bin_sel, " / Kernel=",krn, " / P=",p), 
                                    x.label=x, y.label=y, masspoints="adjust", covs=z )
              }
              else{ #this rdplot call runs when the number of bins is specified
                  out_put <- rdplot(x = dat[[x]], y = dat[[y]], c = cut, binselect = bin_sel, kernel=krn, nbins = c(n_bin),
                                        x.lim = c(-x_lim,x_lim), y.lim = c(-y_lim,y_lim),
                                        title=paste0("Method=",bin_sel, " / Kernel=",krn, " / P=",p), 
                                        x.label=x,y.label=y, masspoints="adjust", covs=z )
              }
              #compile results
              out_sum <- as.data.frame(cbind(method=paste0(bin_sel), kernel=paste0(krn), p=paste0(p),
                                                   imse_l=out_put$J_IMSE[1], imse_r=out_put$J_IMSE[2], mv_l=out_put$J_MV[1], mv_r=out_put$J_MV[2],
                                                   mn_bin_len_l=round(out_put$bin_avg[1],3), mn_bin_len_r=round(out_put$bin_avg[2],3),
                                                   md_bin_len_l=round(out_put$bin_med[1],3), md_bin_len_r=round(out_put$bin_med[2],3)))
              return(out_sum)
}, Var1, Var2, Var3, dat=list(rd_dat), x=list("run_z_c"), y=list("Y"), cut=list(0), n_bin=list(0))) )
```
Once the resulting data frame is generated, I subset to focus on those where p=4 (i.e., the global polynomial used to approximate the mean functions on either side of the cut-off).
```
#create file to print: remember, p is about the graph polynomial
bin_prt <- bins_id %>% filter(p==4) %>% 
  mutate(left_n = case_when(method %in% c("esmv","qsmv") ~ mv_l, TRUE ~ imse_l), 
         rght_n = case_when(method %in% c("esmv","qsmv") ~ mv_r, TRUE ~ imse_r)) %>%
  dplyr::select(method,kernel,p,left_n,rght_n,mn_bin_len_l,mn_bin_len_r,md_bin_len_l,md_bin_len_r)
bin_prt
```
As we can see below, we get a summary table displaying the number of bins, the mean and the median bin lengths on each side of the cut-off. Note how ES-based bins have the same mean and median lengths on either side, while QS-based bins are different. 

![](/images/rd_bin_id.png)

Assume we wish to proceed looking only at plots where the polynomial is 4 (i.e, p=4), but we'd like to show nicely formatted plots for inclusion in a report. I've compiled another function to make a plot with a bit more formatting. The function takes an output object from RDPLOT, as well as arguments represeting the raw data, the X and Y variables, and their corresponding labels and break-points for plotting on the axes.  
```
##pretty bin plot function - this should be presented first as a display of raw data
rd_col_plot = function(in_data, cut, rawdat, xvar, xlab, yvar, ylab, xbrk, ybrk){
  c = cut
  x_plot = in_data$vars_poly[,"rdplot_x"]
  y_hat  = in_data$vars_poly[,"rdplot_y"]
  x_plot_r=x_plot[x_plot>=c]
  x_plot_l=x_plot[x_plot<c]
  num_bin_l <- in_data$vars_bins %>% filter(rdplot_mean_bin < 0) %>% summarise(n=n())
  num_bin_r <- in_data$vars_bins %>% filter(rdplot_mean_bin >= 0) %>% summarise(n=n())
  rd_tx = c(rep("TX",nrow(data.frame(x_plot_l))), rep("CT",nrow(data.frame(x_plot_r))))
  rd_tx = data.frame(rd_tx)
  rd_line_data <- data.frame(x_plot,y_hat,rd_tx)
  rd_line_data$id <- seq.int(nrow(rd_line_data))
  rdplot_mean_bin = in_data$vars_bins[,"rdplot_mean_bin"]
  rdplot_mean_bin <- data.frame(rdplot_mean_bin)
  rdplot_mean_bin$id <- seq.int(nrow(rdplot_mean_bin))
  rdplot_mean_y   = in_data$vars_bins[,"rdplot_mean_y"]
  #rdplot_mean_y[is.nan(rdplot_mean_y)] <- 0
  rdplot_mean_y <- data.frame(rdplot_mean_y)
  rdplot_mean_y$id <- seq.int(nrow(rdplot_mean_y))
  rd_dot_color  = data.frame(c(rep("TX",num_bin_l),rep("CT",num_bin_r)))
  rd_dot_color$id <- seq.int(nrow(rd_dot_color))
  colnames(rd_dot_color) <- c("rd_dot_color", "id")
  rd_line_data <-  rd_line_data %>% left_join(rdplot_mean_bin, by = "id") %>% 
    left_join(rdplot_mean_y, by = "id") %>% 
    left_join(rd_dot_color, by = "id")
  
  floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
  ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
  x_lo <- floor(floor_dec(min(rawdat[[xvar]], na.rm=T), 1)*2)/2
  x_hi <- ceiling(ceiling_dec(max(rawdat[[xvar]], na.rm=T), 1)*2)/2
  x_lim <- max(abs(x_lo), abs(x_hi))
  y_lo <- floor_dec(min(rawdat[[xvar]]), 1)
  y_hi <- ceiling_dec(max(rawdat[[xvar]]), 1)
  y_lim <- max(abs(y_lo), abs(y_hi))
  
  temp_plot <- ggplot(rd_line_data, aes(color=rd_tx)) + 
    geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y, color=rd_dot_color, shape=rd_dot_color), size=2, show.legend = FALSE, na.rm = TRUE) +
    geom_line(aes(x = x_plot, y = y_hat), size=1, na.rm = TRUE) + 
    labs(x = xlab, y = ylab, col="Treatment", shape="Treatment") + 
    geom_vline(xintercept = c, linetype = "solid", color="black", linewidth=1) +
    theme_classic() + 
    scale_color_brewer(palette="Set1", direction=-1, na.translate = F) +
    scale_x_continuous(limits=c(-x_lim, x_lim), breaks=xbrk) +
    scale_y_continuous(limits=c(-y_lim, y_lim), breaks=ybrk) 
  #print(temp_plot)
  return(rd_line_data)	
}
```
Making use of the selection method list defined above, we can use a custom function to generate the RDPLOT output object. Note I have included 'hide=T' to suppress printing the plot itself.    
```
#use the selection method list defined above to generate the require output objects
plt_func <- function(bin_sel, dat, x, y, krn, p, cut) {
  out_put <- rdplot(x = dat[[x]], y = dat[[y]], c = cut, binselect = bin_sel, kernel=krn, p=p, 
                    x.lim = c(-x_lim,x_lim), y.lim = c(-y_lim,y_lim),
                    title=paste0("Method=",bin_sel, " / Kernel=",krn, " / P=",p), 
                    x.label=x, y.label=y, masspoints="adjust", covs=z, hide=T )
  return(out_put)
}
```
The call to the function refers to the selection list and specified a triangular kernel and polynomial of 4. This yields a resulting object list (rd_plt_out) containing the results for each of the four selection methods.
```
rd_plt_out <- lapply(sel_lst, plt_func, dat=rd_dat, x="run_z_c", y="Y", krn="triangular", p=4, cut=0)
```
Using this object as input to the fancier plot function I crafted, we can generate another output object for the 'nice' plots (rd_ncplt_out).
```
rd_ncplt_out <- lapply(rd_plt_out, rd_col_plot, cut=0, rawdat=rd_dat, xvar='run_z_c', xlab='Centered Z-Score', yvar='Y', ylab='Y',  
                  xbrk=c(-4,-3, -2, -1, 0, 1, 2, 3, 4), ybrk=c(-4,-3, -2, -1, 0, 1, 2, 3, 4) )

```
Now we edit the resulting list output object to create labels of each selection method and spacing to be used as as a facet in out facet plot. Note the list is in the order of the specified methods list specified above.
```
#edit the nice object to label the output for each selection method
fac_dat <- lapply(rd_ncplt_out, as.data.frame) %>% bind_rows(.id = "id") %>% 
  mutate(bin_type = case_when(id == 1 ~ "MV", id == 2 ~ "MV", id == 3 ~ "IMSE", id == 4 ~ "IMSE"),
         spacing = case_when(id == 1 ~ "ES", id == 2 ~ "QS", id == 3 ~ "ES", id == 4 ~ "QS"))
```
We then submit this data to a GGPLOT call with bin selection type as rows and spacing as columns.
```
#facet plot of graphs
fac_plot <- ggplot(fac_dat, aes(color=rd_tx)) + 
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y, color=rd_dot_color, shape=rd_dot_color), size=2, show.legend = FALSE, na.rm = TRUE) +
  geom_line(aes(x = x_plot, y = y_hat), size=1, na.rm = TRUE) + 
  labs(x = 'Centered Z-Score', y = 'Y', col="Treatment", shape="Treatment") + 
  geom_vline(xintercept = 0, linetype = "solid", color="black", linewidth=1) +
  theme_classic() + 
  scale_color_brewer(palette="Set1", direction=-1, na.translate = F) +
  scale_x_continuous(limits=c(-x_lim, x_lim), breaks=c(-4,-3, -2, -1, 0, 1, 2, 3, 4)) +
  scale_y_continuous(limits=c(-y_lim, y_lim), breaks=c(-4,-3, -2, -1, 0, 1, 2, 3, 4)) + 
  facet_grid(rows = vars(bin_type), cols = vars(spacing)) + 
  theme(panel.background = element_rect(fill = NA, color = "black"), legend.position="bottom",
        axis.text=element_text(size=13), axis.title=element_text(size=13),
        legend.text=element_text(size=13), legend.title=element_text(size=13),
        strip.text.x = element_text(size=13), strip.text.y = element_text(size=13) )
print(fac_plot)
```
As we can see in the plot below, we get four quadrants representing the bin plots by bin selection method and spacing method. Note how the ES-based plots appear to have a bit more variability across the bins, while the QS are more clustered toward the cut-score. Regardless of methods, each plot seems to suggest an advantage for the treatment group (i.e., an intersection at the cut-point for the treatment group that is higher than the intersection for the control group).

![](/images/rd_bin_fac_plot.png) 

## Generating RD Estimates
Having examined the raw data and the smoothed data via the faceted bin plot, we should have a pretty good idea of (a) whether a discontinuity is likely to be present and (b) some ideas about the actual functional form of the local regressions on either side of the cut-off. One other piece of information we should have in hand before turning to <a href="https://rdpackages.github.io/rdrobust/">RDROBUST</a> for estimation is whether we truly have a running/assignment score continuum of unique data points. RDROBUST contains an adjustment function (masspoints) for when a completely unique continuum is not available. Here is the code i used to check for masspoints.
```
#determine whether masspoints existing in running variable
#should be done on the analytic sample: no missing data on running variable, covariates or outcome
rv_u <- rd_dat %>% ungroup() %>% dplyr::select(site, tx_a, run_z_c) %>% group_by(site, tx_a, run_z_c) %>% 
  mutate(masspoints=n()) %>% distinct() %>% filter(masspoints > 1) %>% ungroup() %>% 
  dplyr::select(site, tx_a) %>% group_by(site, tx_a) %>% mutate(masspoint_n = n())
print(rv_u)
```
With the current data, there are no masspoints found. The code is set up to generate a summary of the number of running score values for both groups on either side of the cut-off that are not unique. Here is some code that duplicates one score in our data and prints a result:
```
  #fictitious sample where data is edited
  fake <- rd_dat %>% mutate(run_z_c = case_when(row_id==3 ~ round(0.33868,5), TRUE ~ round(run_z_c,5)))
  rv_u <- fake %>% filter(row_id <= 3) %>% ungroup() %>% dplyr::select(site, tx_a, run_z_c) %>% 
    group_by(site, tx_a, run_z_c) %>% 
    mutate(masspoints=n()) %>% distinct() %>% filter(masspoints > 1) %>% ungroup() %>% 
    dplyr::select(site, tx_a) %>% group_by(site, tx_a) %>% mutate(masspoint_n = n())
  print(rv_u)
```
![](/images/rd_fake_masspt.png) 

Given we do not have any masspoints, we can forego using that command option within RDROBUST during estimation. The following code will execute an analysis of our RD data. We specify _run_z_c_ as our assignment variable, _Y_ as our dependent variable, a cut-score of zero, the data-driven bandwidth selection procedure MSETWO (so we allow for separate bandwidths on either side of the cut-score), a triangular kernel weight, a linear specification (p=1) using the covariates we assigned to object _z_. The "all=TRUE" option tells RDROBUST to report all the types of estimates and standard errors (see the RDROBUST documentation). See Cattaneo, Idrobo & Titiunik (2018a) and Cattaneo & Titiunik (2022) for recommendations on when to use separate bandwidths, kernel functions, and regression function specifications (i.e., p=1).

```
rd_est = rdrobust(x=rd_dat$run_z_c, y=rd_dat$Y, c=0, bwselect="msetwo", kernel="triangular", p=1, covs=z,     masspoints="off", all=TRUE)
summary(rd_est)
```
First, we notice that our data-driven bandwidth selection procedure yielded different boundaries on either side (0.937 to the left of the cut-score, 0.770 to the right), resulting in 196 and 160 units falling within our bandwidth, respectively (and wider for the bias-corrected bandwidths). We also see in the summary estimate table at the bottom of the output that all estimates are negative and significant. Why are we obtaining negative estimates despite the graphical evidence presented previously showing a positive effect for treatment group? It is important to know that RDROBUST is parameterized to assume that the intervention group of interest falls _ABOVE_ the cut-score. Thus, these results are showing that the comparison group has a significantly negative effect relative to the treatment group.

![](/images/rd_est.png)

We can take these results and create a nice plot that shows the estimated lines on each side of the cut-score over the raw data points falling inside the bandwidth. The following custom function will create this plot, utilizing the rd_est object created above and the raw data as input. Note that aspects of this function can be customized for particular needs, including labels of the intervention groups, colors, axis labels, etc. 

We can see the positive effect associated with the intervention group, as well as the raw data points for each group on either side of the cut-off. I have also included the code to annotate the plot with the estimate, p-value and confidence interval. Be sure to adapt these features if you want to change the location of the annotation or report the robust, bias-corrected versions of the estimate, p-value or confidence intervals. 

![](/images/rd_est_plot.png)

The code to create this plot is:
```
#this function creates nice, final estimated plot with raw dots but estimated lines
rd_est_plot = function(in_raw, in_est, x_raw, y_raw, dot_col, cut, krn, p, xlab, ylab, ymin, ymax, xbrk, ybrk){
  #filter raw data by bandwidth from final model; calculate weights
  raw_sub <- in_raw %>% mutate(rd_dot_color_raw={{dot_col}}) %>%  
    filter({{x_raw}} >= (cut - round(in_est$bws[1],4)) & {{x_raw}} <= (cut + round(in_est$bws[3],4)) ) %>% arrange({{x_raw}}) %>%
    mutate(id=row_number(), u_l = ({{x_raw}} - cut)/in_est$bws[1], u_r = ({{x_raw}} - cut)/in_est$bws[3]) 
  raw_sub <- raw_sub %>% mutate(krntyp=krn) %>%
    mutate(kernel_wght = case_when (krntyp=="uniform" & {{x_raw}} < 0 ~ (0.5*(abs(u_l)<=1))/in_est$bws[1], 
                                    krntyp=="uniform" & {{x_raw}} >= 0 ~ (0.5*(abs(u_r)<=1))/in_est$bws[3],
                                    krntyp=="triangular" & {{x_raw}} < cut ~ ((1-abs(u_l))*(abs(u_l)<=1))/in_est$bws[1],
                                    krntyp=="triangular" & {{x_raw}} >= cut ~ ((1-abs(u_r))*(abs(u_r)<=1))/in_est$bws[3], TRUE ~ NA_real_) )
  
  #create vector of covariates on subset of data
  z_plot = cbind(raw_sub$C1, raw_sub$C2, raw_sub$s1_dum, raw_sub$s2_dum, raw_sub$s3_dum)
  
  #create functions for floor and ceiling for decimals
  floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
  ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
  x_lo <- floor(floor_dec(min(raw_sub[[deparse(substitute(x_raw))]], na.rm=T), 1)*2)/2
  x_hi <- ceiling(ceiling_dec(max(raw_sub[[deparse(substitute(x_raw))]], na.rm=T), 1)*2)/2
  x_lim <- max(abs(x_lo), abs(x_hi))
  y_lo <- floor(floor_dec(min(raw_sub[[deparse(substitute(y_raw))]], na.rm=T), 1)*2)/2
  y_hi <- ceiling(ceiling_dec(max(raw_sub[[deparse(substitute(y_raw))]], na.rm=T), 1)*2)/2
  y_lim <- max(abs(y_lo), abs(y_hi))
  
  #create plot output file
  est_plot <- rdplot(x = raw_sub[[deparse(substitute(x_raw))]], y = raw_sub[[deparse(substitute(y_raw))]], 
                     c = cut, kernel=krn, p=p, binselect="esmv", weights=raw_sub$kernel_wght, covs=z_plot, 
                     x.lim = c(-x_lim, x_lim), y.lim = c(-y_lim, y_lim), hide=TRUE, masspoints="off" )
  
  #limit raw data to necessarily fields
  raw_sub <- raw_sub %>% dplyr::select(id, {{x_raw}}, {{y_raw}}, rd_dot_color_raw)
  
  #now use rdplot output to create lines for plotting
  x_plot = est_plot$vars_poly[,"rdplot_x"]
  y_hat  = est_plot$vars_poly[,"rdplot_y"]
  x_plot_r=x_plot[x_plot >= cut]
  x_plot_l=x_plot[x_plot < cut]
  num_bin_l = est_plot$J[1]
  num_bin_r = est_plot$J[2]
  rd_tx = c(rep("TX",nrow(data.frame(x_plot_l))), rep("CT",nrow(data.frame(x_plot_r))))
  rd_line_data <- data.frame(cbind(as.numeric(x_plot),as.numeric(y_hat),rd_tx))
  rd_line_data$id <- seq.int(nrow(rd_line_data))
  rdplot_mean_bin = est_plot$vars_bins[,"rdplot_mean_bin"]
  rdplot_mean_bin <- data.frame(rdplot_mean_bin)
  rdplot_mean_bin$id <- seq.int(nrow(rdplot_mean_bin))
  rdplot_mean_y = est_plot$vars_bins[,"rdplot_mean_y"]
  rdplot_mean_y <- data.frame(rdplot_mean_y)
  rdplot_mean_y$id <- seq.int(nrow(rdplot_mean_y))
  rd_dot_color  = data.frame(c(rep("TX",num_bin_l),rep("CT",num_bin_r)))
  rd_dot_color$id <- seq.int(nrow(rd_dot_color))
  colnames(rd_dot_color) <- c("rd_dot_color", "id")
  rd_line_data <-  rd_line_data %>% 
    left_join(rdplot_mean_bin, by = "id") %>% 
    left_join(rdplot_mean_y, by = "id") %>% 
    left_join(rd_dot_color, by = "id") %>% 
    left_join(raw_sub, by = "id") 
  
  #filter out the section at the cut point so line doesn't appear on graph
  rd_line_data <- rd_line_data %>% filter(V1 != 0)
  
  #create plot
  temp_plot <- ggplot(rd_line_data, aes(color=rd_tx)) + 
    geom_point(aes(x = {{x_raw}}, y = {{y_raw}}, color=rd_dot_color_raw, shape=rd_dot_color_raw), size=2, alpha = 0.5, show.legend = FALSE, na.rm = TRUE) +
    geom_line(aes(x = as.numeric(V1), y = as.numeric(V2)), size=2, na.rm = TRUE) + 
    labs(x = xlab, y = ylab, col="Treatment", shape="Treatment") + 
    geom_vline(xintercept = cut, linetype = "solid", color="black", size=1) +
    theme_classic() + 
    scale_color_brewer(palette="Set1", direction=-1, na.translate = F) + scale_fill_brewer(palette="Set1", direction=-1) + #na.trans removes NA from legend
    scale_x_continuous(limits=c(-x_lim, x_lim), breaks=xbrk) +
    scale_y_continuous(limits=c(-y_lim, y_lim), breaks=ybrk) + 
    annotate("text", x=-1, y=3.0, label = paste("Estimate = ", format(round(in_est$coef[1],3),nsmall=3), sep=""), hjust=0, size=3) + 
    annotate("text", x=-1, y=2.8, label = paste("p-value = ", format(round(in_est$pv[1],3),nsmall=3), sep=""), hjust=0, size=3) + 
    annotate("text", x=-1, y=2.6, label = paste("C.I. = [", format(round(in_est$ci[1],3),nsmall=3)," , ", format(round(in_est$ci[4],3),nsmall=3), "]", sep=""), hjust=0, size=3)
  print(temp_plot)
  return(rd_line_data)
}
rd_est_plot(in_raw=rd_dat, in_est=rd_est, x_raw=run_z_c, y_raw=Y, dot_col=tx_a,
                      krn="triangular", cut= 0, p=1,
                      xlab='Centered Z-Score', ylab='Y',
                      xbrk=seq(-2, 2, by=0.5), 
                      ybrk=seq(-4, 4, by=1) )
```
The code above for generating the nice plot is handy once you have settled on your final model specification (i.e., the kernel weight to use, the polynomial specification, etc.). As a form of sensitivity analysis, it can be helpful to examine estimates for an array of specifications. The following function again takes list of parameters for the bandwidth selection, kernel type, and polynomial specification, estimates the models using each combination, and creates a summary table of relevant output information. Included in the summary information is a standardized effect size (and corresponding confidence interval) based on the raw outcome standard deviation on each side of the cut-off. In addition, you'll note I set up the code to flip the estimates and confidence intervals (by multiplying by -1) so that the estimates refer to the effect for the group _below_ the cut-point. If you have a Fuzzy design, the first-stage estimates required during a WWC review are also captured (just adjust the selections made before printing).

```
#set up lists of parameters to estimate across    
sel_lst <- c("mserd","msetwo","cerrd","certwo")
krn_lst <- c("uniform","triangular")
p_lst <- c(1,2)
all_lst <- expand.grid(sel_lst, krn_lst, p_lst)


bw_id <- with(all_lst, Map(function(bw_sel, krn, p, dat, x, y, cut, covars, fuz) {
  ci_vec <- c(0.05, seq(from = -1000, to = 1000, by = .1)) #establish confidence interval of interest
  #conduct bandwidth selection
  if (fuz == "N") {
    out_put <- rdrobust(x = dat[[x]], y = dat[[y]], c = cut, kernel = krn, p = p, bwselect = bw_sel, covs=covars, masspoints = "adjust", all=T)
  }
  else {
    out_put <- rdrobust(x = dat[[x]], y = dat[[y]], c = cut, kernel = krn, p = p, bwselect = bw_sel, covs=covars, masspoints = "adjust", all=T, fuzzy=dat$tx_fuz)
  }
  #summary(out_put)
  
  #use output to isolate records inside robust bandwidth to calculate weights
  es_sub <- dat %>% filter(dat[[x]] >= (cut - round(out_put$bws[2],4)) & dat[[x]] <= (cut + round(out_put$bws[4],4)) )
  es_sub <- es_sub %>% mutate(u_l = (es_sub[[x]] - cut)/out_put$bws[2], u_r = (es_sub[[x]] - cut)/out_put$bws[4]) 
  es_sub <- es_sub %>% mutate(krntyp=krn) %>%
    mutate(kernel_wght = case_when (krntyp=="uniform" & es_sub[[x]] < 0 ~ (0.5*(abs(u_l)<=1))/out_put$bws[2], 
                                    krntyp=="uniform" & es_sub[[x]] >= 0 ~ (0.5*(abs(u_r)<=1))/out_put$bws[4],
                                    krntyp=="triangular" & es_sub[[x]] < cut ~ ((1-abs(u_l))*(abs(u_l)<=1))/out_put$bws[2],
                                    krntyp=="triangular" & es_sub[[x]] >= cut ~ ((1-abs(u_r))*(abs(u_r)<=1))/out_put$bws[4],
                                    TRUE ~ NA_real_) )
  
  #calculate effect size
  if (fuz == "N") {
    es_l <- dat %>% filter(dat[[x]] < cut)
    es_r <- dat %>% filter(dat[[x]] >= cut)
  }
  if (fuz == "Y") {
    es_l <- dat %>% filter(tx_fuz == 1)
    es_r <- dat %>% filter(tx_fuz == 0)
  }
  l_n <- es_l %>% summarise(n())
  r_n <- es_r %>% summarise(n()) 
  l_sd <- es_l %>% summarise(sd(es_l[[y]]))
  r_sd <- es_r %>% summarise(sd(es_r[[y]]))
  omega <- 1 - (3/(4*(l_n + r_n - 2) - 1))
  sd_p <- sqrt( ((l_n-1)*(l_sd^2) + (r_n-1)*(r_sd^2))/(l_n + r_n - 2) )
  g <- as.numeric(round( omega*(round(unlist(out_put$coef)[1]*-1,4)) / sd_p, 5))
  g_se <- as.numeric(omega*sqrt( (out_put$se[1]/sd_p)^2 + g^2/(2*(l_n + r_n)) ))
  g_lcl <- round(g - g_se*(qnorm((1-.95)/2)*-1), 5)
  g_ucl <- round(g + g_se*(qnorm((1-.95)/2)*-1), 5)
  
  #compile results - THESE ARE FLIPPED TO ACCOUNT FOR ABOVE-CUT AS TREATMENT (hence *-1 and flipping confidence intervals)
  if (fuz == "N") {
    out_sum <- as.data.frame(cbind(bw_type=bw_sel, kernel=krn, p=p,
                                   n_l=out_put$N_h[2], n_r=out_put$N_h[1],
                                   bw_l=round(out_put$bws[3],3), bw_r=round(out_put$bws[1],3), 
                                   bw_b_l=round(out_put$bws[4],3), bw_b_r=round(out_put$bws[2],3),
                                   conv_est=round(out_put$coef[1]*-1,3), conv_se=round(out_put$se[1],3), conv_p=format(round(out_put$pv[1],3),nsmall=3), conv_l=round(out_put$ci[4]*-1,3), conv_r=round(out_put$ci[1]*-1,3),
                                   bc_est=round(out_put$coef[2]*-1,3), bc_se=round(out_put$se[2],3), bc_p=format(round(out_put$pv[2],3),nsmall=3), bc_l=round(out_put$ci[5]*-1,3),  bc_r=round(out_put$ci[2]*-1,3),
                                   rob_n_l=out_put$N_b[2], rob_n_r=out_put$N_b[1], 
                                   rob_est=round(out_put$coef[3]*-1,3), rob_se=round(out_put$se[3],3), rob_p=format(round(out_put$pv[3],3),nsmall=3), rob_l=round(out_put$ci[6]*-1,3), rob_r=round(out_put$ci[3]*-1,3),
                                   es=round(g,3), es_ci_l=round(g_lcl,3), es_ci_r=round(g_ucl,3)))
  }
  else{
    out_sum <- as.data.frame(cbind(bw_type=bw_sel, kernel=krn, p=p,
                                   n_l=out_put$N_h[2], n_r=out_put$N_h[1],
                                   bw_l=round(out_put$bws[3],3), bw_r=round(out_put$bws[1],3), 
                                   bw_b_l=round(out_put$bws[4],3), bw_b_r=round(out_put$bws[2],3),
                                   conv_est=round(out_put$coef[1]*-1,3), conv_se=round(out_put$se[1],3), conv_p=format(round(out_put$pv[1],3),nsmall=3), conv_l=round(out_put$ci[4]*-1,3), conv_r=round(out_put$ci[1]*-1,3),
                                   bc_est=round(out_put$coef[2]*-1,3), bc_se=round(out_put$se[2],3), bc_p=format(round(out_put$pv[2],3),nsmall=4), bc_l=round(out_put$ci[5]*-1,3),  bc_r=round(out_put$ci[2]*-1,3),
                                   rob_n_l=out_put$N_b[2], rob_n_r=out_put$N_b[1], 
                                   rob_est=round(out_put$coef[3]*-1,3), rob_se=round(out_put$se[3],3), rob_p=format(round(out_put$pv[3],3),nsmall=3), rob_l=round(out_put$ci[6]*-1,3), rob_r=round(out_put$ci[3]*-1,3),
                                   es=round(g,3), es_ci_l=round(g_lcl,3), es_ci_r=round(g_ucl,3),
                                   fs_conv_est=round(out_put$tau_T[1]*-1,3), fs_conv_se=round(out_put$se_T[1],3), 
                                   fs_conv_t=round(out_put$t_T[1]*-1,3), fs_conv_p=format(round(out_put$pv_T[1],3),nsmall=3), 
                                   fs_conv_l=round(out_put$ci_T[4]*-1,3), fs_conv_r=round(out_put$ci_T[1]*-1,3),
                                   fs_bc_est=round(out_put$tau_T[2]*-1,3), fs_bc_se=round(out_put$se_T[2],3), 
                                   fs_bc_t=round(out_put$t_T[2]*-1,3), fs_bc_p=format(round(out_put$pv_T[2],3),nsmall=3), 
                                   fs_bc_l=round(out_put$ci_T[5]*-1,3), fs_bc_r=round(out_put$ci_T[2]*-1,3),
                                   fs_rob_est=round(out_put$tau_T[3]*-1,3), fs_rob_se=round(out_put$se_T[3],3),
                                   fs_rob_t=round(out_put$t_T[3]*-1,3), fs_rob_p=format(round(out_put$pv_T[3],3),nsmall=3), 
                                   fs_rob_l=round(out_put$ci_T[6]*-1,3), fs_rob_r=round(out_put$ci_T[3]*-1,3) ))
  }
  return(out_sum)
}, Var1, Var2, Var3, dat=list(rd_dat), x=list("run_z_c"), y=list("Y"), cut=list(0), covars=list(z), fuz=list("N")))

rd_bw_id <- lapply(bw_id, as.data.frame) %>% bind_rows(.id = "id") %>% 
  mutate(bw_type = case_when(bw_type == 1 ~ "mserd", bw_type == 2 ~ "msetwo", bw_type == 3 ~ "cerrd", bw_type == 4 ~ "certwo"),
         kernel = case_when(kernel == 1 ~ "uniform", kernel == 2 ~ "triangular"),
         p = case_when(p == 1 ~ 1, p == 2 ~ 2))
rd_bw_id_prt <- rd_bw_id %>% dplyr::select(kernel, bw_type, p, conv_est, conv_se, n_l, n_r, rob_p, conv_l, conv_r, bw_b_l, bw_b_r, es, es_ci_l, es_ci_r, 
                                           rob_est, rob_se, rob_l, rob_r)
rd_bw_id_prt
```
Note in the summary output selected, we present the conventional estimate accompanied by the robust p-value (as recommended by Cattaneo, Idrobo & Titiunik; 2018a). The bandwidths, effect sizes and robust estimates are also presented. The summary allows for a quick examination of models using either a linear or quadractice specification, different weighting kernels and different bandwidth selection types.

![](/images/rd_bw_id.png) 

Please feel free to reach out with questions about any of the code or content presented above. The next post will present the code I used to report the evidence necessary for a WWC Standards 5.0 review of our RD design evidence. 

## References
Angrist, J., & Lavy, V. (1999). Using Maimonides’ rule to estimate the effect of class size on student achievement. _Quarterly Journal of Economics 114, May_, 535-575. DOI: <a href="https://doi.org/10.1162/003355399556061">10.1162/003355399556061</a>

Cattaneo, M. D., Idrobo, N., and Titiunik, R. (2018a). _A Practical Introduction to Regression Discontinuity Designs: Volume I, Cambridge Elements: Quantitative and Computational Methods for Social Science_. Cambridge University Press.

Cattaneo, M. D., Idrobo, N., and Titiunik, R. (2018b). _A Practical Introduction to Regression Discontinuity Designs: Volume I, Cambridge Elements: Quantitative and Computational Methods for Social Science_. Cambridge University Press.

Cattaneo, M. D., Titiunik, R., Vazquez-Bare, G., & Keele, L. (2016). Interpreting regression discontinuity designs with multiple cutoffs. _The Journal of Politics, 78(4)_, 1229–1248. DOI: <a href="https://www.journals.uchicago.edu/doi/abs/10.1086/686802">10.1086/686802</a>

Cattaneo, M. D. & Titiunik, R. (2022). Regression discontinuity designs. _The Annual Review of Economics, 14_, 821-51. DOI: <a href="https://doi.org/10.1146/annurev-economics-051520-021409">10.1146/annurev-economics-051520-021409</a>

Cook, T. D., W. R. Shadish, and V. C. Wong. 2008. Three Conditions Under Which Experiments and Observational Studies Produce Comparable Causal Estimates: New Findings from Within-Study Comparisons. _Journal of Policy Analysis and Management 27, 4_, 724-750. DOI: <a href="https://doi.org/10.1002/pam.20375">10.1002/pam.20375</a>

Hahn, J., P. Todd, and W. van de Klaauw. (1999). Evaluating the Effect of an Antidiscrimination Law Using a Regression-Discontinuity Design (_NBER Working Paper 7131_). Cambridge, MA: National Bureau of Economic Research. DOI: <a href="https://doi.org/10.3386/w7131">10.3386/w7131</a>

Hitt, D. H., Robinson, W., & Player, D. (2018). _District readiness to support school turnaround: A guide for state education agencies and districts, 2nd edition_. [The Center on School Turnaround at WestEd]. San Francisco, CA: WestEd.

Imbens, G. W. & Kalyanaraman, K. (2009). Optimal Bandwidth Choice for the Regression Discontinuity Estimator. (Unpublished working paper).

Imbens, G. W., & Lemieux, T. (Eds.). (2008a). The regression discontinuity design: Theory and applications [Special issue]. _Journal of Econometrics, 142(2)_. DOI: <a href="https://doi.org/10.1016/j.jeconom.2007.05.008">10.1016/j.jeconom.2007.05.008</a>

Jacob, B., & Lefgren, L. (2004a). Remedial education and student achievement: A regression-discontinuity analysis. _Review of Economics and Statistics, LXXXVI_, 226–244. DOI: <a href="https://doi.org/10.3386/w8918">10.3386/w8918</a>

Ludwig, J., & Miller, D. L. (2007). Does Head Start improve children’s life chances? Evidence from a regression discontinuity design. _The Quarterly Journal of Economics, 122(1)_, 159–208. DOI: <a href="https://doi.org/10.1162/qjec.122.1.159">10.1162/qjec.122.1.159</a>

Shadish, W. R., T. D. Cook, and D. T. Campbell. (2002). _Experimental and Non-Experimental Designs for Generalized Causal Inference_. Boston: Houghton Mifflin.

The Center on School Turnaround. (2017). _Four domains for rapid school improvement: A systems framework_ [The Center for School Turnaround at WestEd]. San Francisco, CA: WestEd.

Thistlethwaite, D. & D. Campbell. (1960). Regression-Discontinuity Analysis: An Alternative to the Ex Post Facto Experiment. _Journal of Educational Psychology, 51_, 309-317. DOI: <a href="https://doi.org/10.1037/h0044319">10.1037/h0044319</a>




