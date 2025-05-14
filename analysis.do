* --- Contents --- *
* 1. * PLOTS AND DESCRIPTIVE STATS * 
* 2. * PORTFOLIO CREATION *
* 3. * ROLLING BETAS (SR, MR, LR) *
* 4. * REGRESSION ANALYSIS *
* 5. * ADDITIONAL EVIDENCE - VECTOR AUTOREGRESSIVE (VAR) MODEL *

* IMPORT DATASET INTO STATA *
// Windows
// import delimited "C:\Users\Ivan\Desktop\alternative\raw_data\dataset.csv", varnames(1) clear

//MacOS
import delimited "/Users/ivan/Desktop/alternative/raw_data/dataset.csv", varnames(1) clear
set scheme s1color

rename price_* p_*

gen datenum = tm(1990m1) + _n-1
format datenum %tm
tsset datenum

drop date 
rename datenum date 


* PLOTS AND DESCRIPTIVE STATS * 
* Plotting raw prices
tsline p_*, legend(size(vsmall) pos(6) rows(2) ///
       label(1 "SPX") label(2 "VIX") label(3 "GSCI") ///
       label(4 "Soy") label(5 "Wheat") label(6 "Corn") ///
       label(7 "Sil") label(8 "Plat") label(9 "Gold") ///
       label(10 "PNG") label(11 "JNJ") label(12 "Exxon")) ///
       xscale(range(360,790)) ///
       ytitle("Price", size(smallmedium) margin(medlarge)) ///
       graphregion(margin(l=4 r=5 b=5 t=5)) ///  
       plotregion(margin(l=8 r=3))    
       
graph export "/Users/ivan/Desktop/alternative/timeseries_plots/prices.png", replace


* Generate log and normalised returns
local price_vars p_corn p_exxon p_gold p_gsci p_jnj p_png p_plat p_sil p_soy p_spx p_wheat p_vix

foreach var of local price_vars {

    * 1. Generate log returns 
    gen log_`var' = ln(`var'/L.`var') if `var' > 0 & L.`var' > 0
    label variable log_`var' "Log return of `var'"

    * 2. Get the first non-missing value
    summ `var' if !missing(`var'), meanonly
    local base_value = r(min)

    * 3. Generate normalised returns using the base value
    gen norm_`var' = (`var'/`base_value') * 100 if !missing(`var')
    label variable norm_`var' "Normalized price (100 = first non-missing `var')"
}

* Re-label variables
foreach var of varlist log_p_* norm_p_* {
    * Extract the ticker by removing the prefix
    local ticker = substr("`var'", strpos("`var'", "_p_") + 3, .)
    * Label the variable with just the ticker
    label variable `var' "`=upper("`ticker'")'"
}

* Create time-series plots for both
* Logarithmic Returns Time Series
tsline log_*, legend(size(vsmall) pos(6) rows(2)) xscale(range(360,790)) ytitle(Return, size(smallmedium))
graph export "/Users/ivan/Desktop/alternative/timeseries_plots/logreturns.png", replace


* Normalised Returns Time Series
tsline norm_*, legend(size(vsmall) pos(6) rows(2)) xscale(range(360,790)) ytitle(Return, size(smallmedium))
graph export "/Users/ivan/Desktop/alternative/timeseries_plots/normreturns.png", replace


* Rename log returns for efficiency
foreach var in log_p_corn log_p_exxon log_p_gold log_p_gsci log_p_jnj log_p_png log_p_plat log_p_sil log_p_soy log_p_spx log_p_wheat log_p_vix {
    local newname = subinstr("`var'", "log_p_", "", 1)  // Remove "log_p_" prefix
    rename `var' `newname'
}

* Creating heatmap of returns
* Install the following packages to create the heatmap 
// ssc install heatplot
// ssc install palettes
// ssc install colrspace

* Find correlation matrix before creating heatmap
corr gold sil plat corn wheat soy exxon jnj png spx gsci vix

* Save the correlation matrix to corrmatrix 
matrix Correlation = r(C)

* Create heatmap 
heatplot Correlation, ///
    xlabel(, angle(90) labsize(small)) ///
    ylabel(, labsize(small)) ///
    color(plasma, reverse intensity(0.7)) ///
    values(format(%4.2f) size(vsmall)) ///
    aspectratio(1) ///
    graphregion(color(white)) plotregion(color(white)) 

graph export "/Users/ivan/Desktop/alternative/charts/heatmap.png", replace

* PORTFOLIO CREATION *
/*We will create three equally weighted portfolios*/

egen portmetal = rowmean(gold sil plat)
egen portagri = rowmean(corn wheat soy)
egen portstock = rowmean(exxon jnj png)
egen porthybrid = rowmean(gold sil corn soy jnj png)

tsline portmetal portagri portstock porthybrid, legend(rows(1)) xscale(range(360,790))

* Descriptive Statistics 
sum portmetal portagri portstock porthybrid
* Mean, Std dev, Min, Max 

* Cumulative Reuturns
* Cmulative Return = e^(sum of all log returns)-1 *
* Sum all returns over time
foreach var in portmetal portagri portstock porthybrid {
    egen cumul_log_`var' = total(`var'), by(date)  // Sums log returns
}

* Convert to simple cumulative returns
foreach var in portmetal portagri portstock porthybrid {
    gen cumul_return_`var' = exp(cumul_log_`var') - 1  
}

foreach var in portmetal portagri portstock porthybrid {
    list cumul_return_`var' in -1  // Displays the last observation
}

* Volatility 
* Define the list of variables
local vars portmetal portagri portstock porthybrid

* Loop through each variable
foreach var of local vars {
    
    * Calculate Mean Return
    quietly sum `var'
    scalar mean_`var' = r(mean)
    
    * Generate Squared Deviations
    gen sd_`var' = (`var' - mean_`var')^2
    
    * Compute Variance (sample)
    quietly sum sd_`var'
    scalar var_`var' = r(sum) / (r(N) - 1)
    
    * Daily Volatility (Standard Deviation)
    scalar dailyvol_`var' = sqrt(var_`var')
    di "Daily Volatility of `var': " dailyvol_`var'
}

* Sharpe Ratio  
* Generate Excess Return variables 
local vars portmetal portagri portstock porthybrid

foreach var of local vars { 
	gen excess_ret_`var' = `var' - (rf/100)
	sum excess_ret_`var'
	scalar `var'_sharpe_r = r(mean)/r(sd)
	di "Sharpe Ratio: " `var'_sharpe_r 
}

* Sortino Ratio 
local vars portmetal portagri portstock porthybrid

foreach var of local vars { 
	gen downside_`var' = excess_ret_`var' if excess_ret_`var' < 0 
	
	sum downside_`var'
	scalar sigma_down_`var' = r(sd)
	
	sum excess_ret_`var'
	scalar mean_excess_`var' = r(mean)
	scalar mean_excess_simp = exp(mean_excess_`var') - 1
	
	scalar sortino_`var' = mean_excess_`var' / sigma_down_`var'
	di "Raw Sortino Ratio: " sortino_`var'
	
}

* Jensen's alpha (and beta)
* Generate market premium (r_m - rf)
gen spx_prem = spx - (rf/100)
local vars portmetal portagri portstock porthybrid

foreach var of local vars { 
	gen `var'_prem = `var' - (rf/100)
	reg `var'_prem spx_prem
	est store jen_alpha_`var'
}

esttab jen_alpha_portmetal jen_alpha_portagri jen_alpha_portstock jen_alpha_porthybrid using "/Users/ivan/Desktop/alternative/regression_outputs/jensensalpha.csv", replace csv label b(6) se(3) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 r2_a, fmt(0 3 3))

* Maximum drawdown
* Define portfolio variables
local vars portmetal portagri portstock porthybrid

foreach var of local vars {
    * Generate cumulative log return path 
    gen cum_log_`var' = sum(`var')
    
    * Convert log returns to price-like index (100 = starting value)
    gen cum_`var' = 100 * exp(cum_log_`var')
    
    * Initialize rolling peak
    gen peak_`var' = cum_`var'[1]
    
    * Updates rolling peak (highest value so far)
    forvalues i = 2/`=_N' {
        replace peak_`var' = max(peak_`var'[_n-1], cum_`var'[`i']) in `i'
    }
    
    * Calculate drawdowns (current value vs. peak)
    gen drawdown_`var' = (cum_`var' - peak_`var') / peak_`var'
    
    * Find maximum drawdown (most negative value)
    sum drawdown_`var'
    scalar max_dd_`var' = abs(r(min)) * 100  // Convert to positive %
    
}

* Displaying maximum drawdown values
local vars portmetal portagri portstock porthybrid

foreach var of local vars {
    * Display 
    di "Max Drawdown for `var': " max_dd_`var' "%"
}

* Maximum drawdown values for the spx (to compare against our portfolios)
gen cum_spx = sum(spx)
gen peak_spx = cum_spx[1]

forvalues i = 2/`=_N' {
        replace peak_spx = max(peak_spx[_n-1], cum_spx[`i']) in `i'
}

gen drawdown_spx = (cum_spx - peak_spx)/peak_spx

sum drawdown_spx 
scalar max_dd_spx = abs(r(min)) * 100

* Maximum drawdown plots
local vars portmetal portagri portstock porthybrid
foreach var of local vars {
    twoway (line drawdown_`var' date) ///
    (line drawdown_spx date, lcolor(gray)), ///
    xscale(range(360,790)) ///
    name(`var'_graph, replace) ///
    legend(label(1 "`var'") label(2 "SPX"))
    graph export "/Users/ivan/Desktop/alternative/maxdrawdown/`var'_drawdown.png", replace
}

twoway (line drawdown_portmetal drawdown_portagri drawdown_portstock drawdown_porthybrid date)

* ROLLING BETAS (SR, MR, LR) *
// ssc install rangestat

* Estimating portfolio betas using trading windows
* Short run (3 years ~ 36 months)
* Define portfolio variables
local vars portmetal portagri portstock porthybrid

foreach var of local vars {
    * Calculate rolling betas 
    rangestat (reg) `var' spx, interval(date -35 0)
    
    * Rename regression outputs
    rename b_spx `var'_beta
    rename reg_nobs `var'_nobs
    rename b_cons `var'_alpha
    rename se_spx `var'_beta_se
    
    * Label variables
    label variable `var'_beta "Rolling β (36M)"
    label variable date "Date"
    
    * Drop unnecessary stats 
    drop reg_r2 reg_adj_r2 se_cons
    
    * Plot rolling betas 
    twoway (line `var'_beta date) if `var'_nobs == 36, ///
        ytitle("Beta") xtitle("Date") ///
        name("`var'_beta_plot", replace) /// 
    
    * Save graph
    capture mkdir "~/Desktop/alternative/rollingbeta_plots"
    graph export "~/Desktop/alternative/rollingbeta_plots/`var'_beta.png", replace
}

* Medium run (5 years ~ 60 months)
* Define portfolio variables
local vars portmetal portagri portstock porthybrid

foreach var of local vars {
    * Calculate rolling betas 
    rangestat (reg) `var' spx, interval(date -59 0)
    
    * Rename regression outputs - NOTE: rangestat creates reg_b_spx not b_spx
    rename b_spx `var'_beta_mr
    rename reg_nobs `var'_nobs_mr
    rename b_cons `var'_alpha_mr
    rename se_spx `var'_beta_se_mr
    
    * Label variables
    label variable `var'_beta_mr "Rolling β (60M)"
    label variable date "Date"
    
    * Drop unnecessary stats 
    drop reg_r2 reg_adj_r2 se_cons
    
    * Plot rolling betas 
    twoway (line `var'_beta_mr date) if `var'_nobs_mr == 60, ///
        ytitle("Beta") xtitle("Date") ///
        name("`var'_beta_plot", replace) ///
    
    * Save graph
    capture mkdir "~/Desktop/alternative/rollingbeta_plots"
    graph export "~/Desktop/alternative/rollingbeta_plots/`var'_beta_mr.png", replace
}

* Long run (10 years ~ 120 months)
* Define portfolio variables
local vars portmetal portagri portstock porthybrid

foreach var of local vars {
    * Calculate rolling betas 
    rangestat (reg) `var' spx, interval(date -119 0)
    
    * Rename regression outputs 
    rename b_spx `var'_beta_lr
    rename reg_nobs `var'_nobs_lr
    rename b_cons `var'_alpha_lr
    rename se_spx `var'_beta_se_lr
    
    * Label variables
    label variable `var'_beta_lr "Rolling β (120M)"
    label variable date "Date"
    
    * Drop unnecessary stats 
    drop reg_r2 reg_adj_r2 se_cons
    
    * Plot rolling betas 
    twoway (line `var'_beta_lr date) if `var'_nobs_lr == 120, ///
        ytitle("Beta") xtitle("Date") ///
        name("`var'_beta_plot", replace) ///

    * Save graph
    capture mkdir "~/Desktop/alternative/rollingbeta_plots"
    graph export "~/Desktop/alternative/rollingbeta_plots/`var'_beta_lr.png", replace
}

* Plotting an overlay version of the rolling betas for each portfolios (with crises highlighted)

gen dotcom_low = -0.5
gen dotcom_high = 1.5
gen gfc_low = -0.5
gen gfc_high = 1.5
gen covid_low = -0.5
gen covid_high = 1.5

* Short Run
twoway (rarea dotcom_low dotcom_high date if date >= ym(1995,1) & date <= ym(2002,8), color(gs12) lwidth(none) lpattern(solid)) ///
       (rarea gfc_low gfc_high date if date >= ym(2007,6) & date <= ym(2009,1), color(gs12) lwidth(none) lpattern(solid)) ///
       (rarea covid_low covid_high date if date >= ym(2020,2) & date <= ym(2023,5), color(gs12) lwidth(none) lpattern(solid)) ///
       (line portmetal_beta date if portmetal_nobs == 36, lcolor(navy) lwidth(medthick)) ///
       (line portagri_beta date if portagri_nobs == 36, lcolor(maroon) lwidth(medthick)) ///
       (line portstock_beta date if portstock_nobs == 36, lcolor(forest_green) lwidth(medthick)) ///
       (line porthybrid_beta date if porthybrid_nobs == 36, lcolor(dkorange) lwidth(medthick)), ///
       ytitle("Beta Coefficient", size(small)) ///
       xtitle("Date", size(small)) ///
       legend(order(4 "Metals" 5 "Agriculture" 6 "Stocks" 7 "Hybrid") rows(1)) ///
       xlabel(, labsize(small)) ylabel(, labsize(small))
graph export "~/Desktop/alternative/rollingbeta_plots/srrolling.png", replace

* Medium Run
twoway (rarea dotcom_low dotcom_high date if date >= ym(1995,1) & date <= ym(2002,8), color(gs12) lwidth(none) lpattern(solid)) ///
       (rarea gfc_low gfc_high date if date >= ym(2007,6) & date <= ym(2009,1), color(gs12) lwidth(none) lpattern(solid)) ///
       (rarea covid_low covid_high date if date >= ym(2020,2) & date <= ym(2023,5), color(gs12) lwidth(none) lpattern(solid)) ///
       (line portmetal_beta_mr date if portmetal_nobs_mr == 60, lcolor(navy) lwidth(medthick)) ///
       (line portagri_beta_mr date if portagri_nobs_mr == 60, lcolor(maroon) lwidth(medthick)) ///
       (line portstock_beta_mr date if portstock_nobs_mr == 60, lcolor(green) lwidth(medthick)) ///
       (line porthybrid_beta_mr date if porthybrid_nobs_mr == 60, lcolor(orange) lwidth(medthick)), ///
       ytitle("Beta Coefficient", size(small)) ///
       xtitle("Date", size(small)) ///
       legend(order(4 "Metals" 5 "Agriculture" 6 "Stocks" 7 "Hybrid") rows(1))  ///
       xlabel(, labsize(small)) ylabel(, labsize(small))
graph export "~/Desktop/alternative/rollingbeta_plots/mrrolling.png", replace

* Long Run
twoway (rarea dotcom_low dotcom_high date if date >= ym(1995,1) & date <= ym(2002,8), color(gs12) lwidth(none) lpattern(solid)) ///
       (rarea gfc_low gfc_high date if date >= ym(2007,6) & date <= ym(2009,1), color(gs12) lwidth(none) lpattern(solid)) ///
       (rarea covid_low covid_high date if date >= ym(2020,2) & date <= ym(2023,5), color(gs12) lwidth(none) lpattern(solid)) ///
       (line portmetal_beta_lr date if portmetal_nobs_lr == 120, lcolor(navy) lwidth(medthick)) ///
       (line portagri_beta_lr date if portagri_nobs_lr == 120, lcolor(maroon) lwidth(medthick)) ///
       (line portstock_beta_lr date if portstock_nobs_lr == 120, lcolor(green) lwidth(medthick)) ///
       (line porthybrid_beta_lr date if porthybrid_nobs_lr == 120, lcolor(orange) lwidth(medthick)), ///
       ytitle("Beta Coefficient", size(small)) ///
       xtitle("Date", size(small)) ///
       legend(order(4 "Metals" 5 "Agriculture" 6 "Stocks" 7 "Hybrid") rows(1)) ///
       xlabel(, labsize(small)) ylabel(, labsize(small))
graph export "~/Desktop/alternative/rollingbeta_plots/lrrolling.png", replace

	   
* REGRESSION ANALYSIS *
* Drop variables for efficiency
drop cumul_log_portmetal cumul_log_portagri cumul_log_portstock cumul_log_porthybrid cumul_return_portmetal cumul_return_portagri cumul_return_portstock cumul_return_porthybrid sd_portmetal sd_portagri sd_portstock sd_porthybrid excess_ret_portmetal excess_ret_portagri excess_ret_portstock excess_ret_porthybrid downside_portmetal downside_portagri downside_portstock downside_porthybrid cum_log_portmetal cum_portmetal peak_portmetal drawdown_portmetal cum_log_portagri cum_portagri peak_portagri drawdown_portagri cum_log_portstock cum_portstock peak_portstock drawdown_portstock cum_log_porthybrid cum_porthybrid peak_porthybrid drawdown_porthybrid cum_spx peak_spx drawdown_spx portmetal_nobs portmetal_beta portmetal_alpha portmetal_beta_se portagri_nobs portagri_beta portagri_alpha portagri_beta_se portstock_nobs portstock_beta portstock_alpha portstock_beta_se porthybrid_nobs porthybrid_beta porthybrid_alpha porthybrid_beta_se portmetal_nobs_mr portmetal_beta_mr portmetal_alpha_mr portmetal_beta_se_mr portagri_nobs_mr portagri_beta_mr portagri_alpha_mr portagri_beta_se_mr portstock_nobs_mr portstock_beta_mr portstock_alpha_mr portstock_beta_se_mr porthybrid_nobs_mr porthybrid_beta_mr porthybrid_alpha_mr porthybrid_beta_se_mr portmetal_nobs_lr portmetal_beta_lr portmetal_alpha_lr portmetal_beta_se_lr portagri_nobs_lr portagri_beta_lr portagri_alpha_lr portagri_beta_se_lr portstock_nobs_lr portstock_beta_lr portstock_alpha_lr portstock_beta_se_lr porthybrid_nobs_lr porthybrid_beta_lr porthybrid_alpha_lr porthybrid_beta_se_lr

* Install estout to save regressions
// ssc install estout

* Looking at the VIX
tsline p_vix

* Simple regressions   
local vars portmetal_prem portagri_prem portstock_prem porthybrid_prem

foreach var of local vars { 
	reg `var' spx_prem cpi 
}

* Crisis dummy regressions
* General crisis conditions (creating indicators which could indicate when a crisis is coming)
gen crisis = 0
replace crisis = 1 if spx <= -0.05 & p_vix > 30

local vars portmetal_prem portagri_prem portstock_prem 

foreach var of local vars {
	reg `var' cpi c.spx_prem##crisis
	est store simple_model_`var'
	
	* Run VIF test to see if multicollinearity is present
	estat vif
	
	* Run Bruesh-Pagan test to see if heteroskedasticity exists 
	hettest 
}   

reg porthybrid_prem cpi c.spx_prem##crisis
est store simple_porthybrid_prem

esttab simple_model_portmetal_prem simple_model_portagri_prem simple_model_portstock_prem simple_porthybrid_prem using "/Users/ivan/Desktop/alternative/regression_outputs/simple_models.csv", replace csv label b(6) se(3) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 r2_a, fmt(0 3 3)) mtitle("Precious Metals" "Argiculture" "Traditional Equities" "Hybrid")

* Dotcom bubble: Jan 1995 - October 2002
gen dot_crisis = 0
replace dot_crisis = 1 if date >= tm(1995m1) & date <= tm(2002m10)

local vars portmetal_prem portagri_prem portstock_prem porthybrid_prem 

foreach var of local vars {
	reg `var' cpi c.spx_prem##dot_crisis
	est store dot_model_`var'
}   

esttab dot_model_portmetal_prem dot_model_portagri_prem dot_model_portstock_prem dot_model_porthybrid_prem using "/Users/ivan/Desktop/alternative/regression_outputs/dotcom_models.csv", replace csv label b(6) se(3) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 r2_a, fmt(0 3 3)) mtitle("Precious Metals" "Argiculture" "Traditional Equities" "Hybrid")


* GFC period: June 2007 - Jan 2009
gen gfc_crisis = 0
replace gfc_crisis = 1 if date >= tm(2007m6) & date <= tm(2009m1)

local vars portmetal_prem portagri_prem portstock_prem porthybrid_prem 

foreach var of local vars {
	reg `var' cpi c.spx_prem##gfc_crisis
	est store gfc_model_`var'
}   

esttab gfc_model_portmetal_prem gfc_model_portagri_prem gfc_model_portstock_prem gfc_model_porthybrid_prem using "/Users/ivan/Desktop/alternative/regression_outputs/gfc_models.csv", replace csv label b(6) se(3) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 r2_a, fmt(0 3 3)) mtitle("Precious Metals" "Argiculture" "Traditional Equities" "Hybrid")

* COVID period: Feb 2020 - June 2020 
gen covid_crisis = 0 
replace covid_crisis = 1 if date >= tm(2020m2) & date <= tm(2023m5)
	   
local vars portmetal_prem portagri_prem portstock_prem porthybrid_prem 

foreach var of local vars {
	reg `var' cpi c.spx_prem##covid_crisis
	est store covid_model_`var'
}      

esttab covid_model_portmetal_prem covid_model_portagri_prem covid_model_portstock_prem covid_model_porthybrid_prem using "/Users/ivan/Desktop/alternative/regression_outputs/covid_models.csv", replace csv label b(6) se(3) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 r2_a, fmt(0 3 3)) mtitle("Precious Metals" "Argiculture" "Traditional Equities" "Hybrid")

* ADDITIONAL EVIDENCE - VECTOR AUTOREGRESSIVE (VAR) MODEL *
* Endogeneous variables: spx with (portmetal, portagri, portstock, porthybrid) [justify why we don't include the premium here... for clarity! Previously, it was intended to follow a more traditional CAPM style; whereas here we want to clarify our insight, so we aim for simplicity]
* Exogeneous variables (for completeness): cpi, rf/100 (rfdec), crisis dummies

* Generate a decimal value for interest rates
gen rfdec = rf/100

* Testing for stationarity (using the Augmented Dickey Fuller (ADF)-test) 
* Looking for stationarity visually 
local vars spx portmetal portagri portstock porthybrid 
foreach var of local vars {
    tsline `var', name(`var', replace) 
}

* Identifying stationarity statistically
local spx portmetal portagri portstock porthybrid 
foreach var of local vars {
	dfuller `var', lags(0)
	
* To no surprise, we find that our porfolios are stationary in levels form, so we can proceed 
	
* Testing stationarity of endogeneosu variables
dfuller rfdec
dfuller cpi

dfuller d.rfdec
dfuller d.cpi

* We find that in order to include our macro indicators into the model, we should difference them

* Find optimal lag lengths 
local vars portmetal portagri portstock porthybrid
foreach var of local vars {
	varsoc `var' spx d.rfdec d.cpi, exog(crisis) 
}

/*The following shows the optimal lags found from each of our criterion
AIC = 4, HQIC = BIC = 2
Prefer AIC to minimise errors at the sacrifice of increasing computational cost
*/

* General Crisis Dummy 
local vars portmetal portagri portstock porthybrid
foreach var of local vars {
	var `var' spx d.rfdec d.cpi, lags(1/4) exog(crisis)
	est store var_model_`var'
	* Plot an eigenvalue mapping to check for dynamic stationarity 
	varstable, graph
	graph export "~/Desktop/alternative/varstable/`var'general.png", replace
}

esttab var_model_portmetal var_model_portagri var_model_portstock var_model_porthybrid using "/Users/ivan/Desktop/alternative/regression_outputs/var_simple_models.csv", replace csv label b(6) se(3) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 r2_a, fmt(0 3 3)) mtitle("Precious Metals" "Argiculture" "Traditional Equities" "Hybrid")


* Dotcom Bubble: Jan 1995 - October 2002
local vars portmetal portagri portstock porthybrid
foreach var of local vars {
	var `var' spx d.rfdec d.cpi, lags(1/4) exog(dot_crisis)
	est store var_dot_model_`var'
	* Plot an eigenvalue mapping to check for dynamic stationarity 
	varstable, graph
	graph export "~/Desktop/alternative/varstable/`var'dotcom.png", replace
}

esttab var_dot_model_portmetal var_dot_model_portagri var_dot_model_portstock var_dot_model_porthybrid using "/Users/ivan/Desktop/alternative/regression_outputs/var_dot_models.csv", replace csv label b(6) se(3) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 r2_a, fmt(0 3 3)) mtitle("Precious Metals" "Argiculture" "Traditional Equities" "Hybrid")


* GFC Period: June 2007 - Jan 2009
local vars portmetal portagri portstock porthybrid
foreach var of local vars {
	var `var' spx d.rfdec d.cpi, lags(1/4) exog(gfc_crisis)
	est store var_gfc_model_`var'
	* Plot an eigenvalue mapping to check for dynamic stationarity 
	varstable, graph
	graph export "~/Desktop/alternative/varstable/`var'gfc.png", replace
}

esttab var_gfc_model_portmetal var_gfc_model_portagri var_gfc_model_portstock var_gfc_model_porthybrid using "/Users/ivan/Desktop/alternative/regression_outputs/var_gfc_models.csv", replace csv label b(6) se(3) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 r2_a, fmt(0 3 3)) mtitle("Precious Metals" "Argiculture" "Traditional Equities" "Hybrid")


* COVID Period: Feb 2020 - June 2020 
local vars portmetal portagri portstock porthybrid
foreach var of local vars {
	var `var' spx d.rfdec d.cpi, lags(1/4) exog(covid_crisis)
	est store var_covid_model_`var'
	* Plot an eigenvalue mapping to check for dynamic stationarity 
	varstable, graph
	graph export "~/Desktop/alternative/varstable/`var'covid.png", replace
}

esttab var_covid_model_portmetal var_covid_model_portagri var_covid_model_portstock var_covid_model_porthybrid using "/Users/ivan/Desktop/alternative/regression_outputs/var_covid_models.csv", replace csv label b(6) se(3) star(* 0.10 ** 0.05 *** 0.01) stats(N r2 r2_a, fmt(0 3 3)) mtitle("Precious Metals" "Argiculture" "Traditional Equities" "Hybrid")




