**import data simulated in r;
proc import out=be_dat datafile="PATH\be_eq.xlsx" dbms=xlsx replace; run;

**baseline equivalence macro;
%macro base_eq(data,var,class,label,num,length,model);

%if "&model" = "t" %then %do;
	ods exclude all;
	proc ttest data=&data plots=none;
		class &class;
		var &var;
		ods output statistics=stats_&num ttests=t_&num equality=eq_&num;
		run;
	ods exclude none;

	**create macro arg denoting equal variances or not;
	proc sql noprint;
	select case when probf < .05 then "Unequal" else "Equal" end into : eqvar
		from eq_&num;
		quit;

	proc sql noprint;
	create table h1_&num as
	select max(variable) as variable length=&length, &label as label length=&length, "t-test" as test length=8,
		mean(case when class="0" then mean end) as ct_mn,
		mean(case when class="1" then mean end) as tx_mn,
		round(mean(case when class="0" then stddev end),.001) as ct_sd,
		round(mean(case when class="1" then stddev end),.001) as tx_sd,
		mean(case when class="0" then n end) as ct_n,
		mean(case when class="1" then n end) as tx_n,
		sum(n) as nobsused
		from stats_&num;
	create table hedge_g_&num as
	select a.*, round(tvalue,.001) as stat_val, df as df, "&eqvar" as eq_var length=7, round(b.probt, .001) as p_val label=' ',
		round((a.tx_mn-a.ct_mn)*(1-3/(4*a.nobsused - 9)) /
		sqrt((((a.ct_n-1)*(a.ct_sd**2)) + ((a.tx_n-1)*(a.tx_sd**2)))/(a.ct_n + a.tx_n - 2)), .001) as hedge_g_adj,
		abs(calculated hedge_g_adj) as abs_hedge_g_adj,
		case when abs(calculated hedge_g_adj) ge 0 and abs(calculated hedge_g_adj) le .05 then "Satisfied"
			 when abs(calculated hedge_g_adj) gt .05 and abs(calculated hedge_g_adj) le .25 then "Stat Adjust"
			 when abs(calculated hedge_g_adj) gt .25 then "Unsatisfied" end as base_equiv,
		&num as comp_num
		from h1_&num as a
		left join t_&num as b on a.variable=b.variable and b.variances="&eqvar";
		quit;
%end;

%if "&model" = "log" %then %do;
	**run logsitic for log-odds ratio for dichotomous predictors;
	ods exclude all;
	proc logistic data=&data;
			model &var (event='1')=&class/clodds=wald rsq lackfit;
		ods output parameterestimates=est_&num nobs=nobs_&num;
		output out=log_out_&num prob=p;
		run;
	ods exclude none;

	**compile information for odds ratio effect size per Cox in WWC;
	proc sql noprint;
	create table h2_&num as
	select "&var" as variable length=&length, &label as label length=&length, "logistic" as test length=8,
		round(mean(case when variable="&class" then waldchisq end),.001) as stat_val,
		mean(case when variable="&class" then df end) as df,
		"N/A" as eq_var length=7,
		mean(case when variable="&class" then estimate end) as lor,
		round(mean(case when variable="&class" then probchisq end),.001) as p_val
		from est_&num;
	create table p_out_&num as
	select distinct "&var" as variable length=&length,
		max(case when &class=1 then p end) as tx_mn,
		max(case when &class=0 then p end) as ct_mn,
		round(sqrt(calculated tx_mn*(1-calculated tx_mn)),.001) as tx_sd,
		round(sqrt(calculated ct_mn*(1-calculated ct_mn)),.001) as ct_sd
		from log_out_&num;
	create table h4_&num as
	select "&var" as variable length=&length, 
		count(case when &class=0 then &class end) as ct_n,
		count(case when &class=1 then &class end) as tx_n
		from &data
		where &var ne . and &class ne .;
	create table h3_&num as
	select "&var" as variable length=&length,
		mean(nobsused) as nobsused
		from nobs_&num;
	create table hedge_g_&num as
	select a.*, b.nobsused, c.tx_n, c.ct_n, d.tx_mn, d.ct_mn, d.tx_sd, d.ct_sd,
		round((1-3/(4*b.nobsused - 9))*(a.lor/1.65),.001) as hedge_g_adj,
		abs(calculated hedge_g_adj) as abs_hedge_g_adj,
		case when abs(calculated hedge_g_adj) ge 0 and abs(calculated hedge_g_adj) le .05 then "Satisfied"
				 when abs(calculated hedge_g_adj) gt .05 and abs(calculated hedge_g_adj) le .25 then "Stat Adjust"
				 when abs(calculated hedge_g_adj) gt .25 then "Unsatisfied" end as base_equiv,
		&num as comp_num
		from h2_&num as a
		left join h3_&num as b on a.variable=b.variable
		left join h4_&num as c on a.variable=c.variable
		left join p_out_&num as d on a.variable=d.variable;
		quit;
%end;

**combine all the files into single table;
%if &num = 1 %then %do;
	data hedge_g;
		set hedge_g_&num;
		run;
%end;
%else %do;
	data hedge_g;
		set hedge_g hedge_g_&num;
		run;
%end;

%mend base_eq;
	
%base_eq(data=be_dat, var=y, class=tx, block=, label='y', num=1, length=40, model=t);
%base_eq(data=be_dat, var=y_b, class=tx, block=, label='y_b', num=2, length=40, model=log);
