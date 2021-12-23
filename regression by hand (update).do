// regression by hand
cd "/Users/gjmb/Desktop/soc360code"
use gss2018, clear

// Let's use a classic example. First, we get our regression line. 

// First, we drop data missing on either var so summary stats are correct
drop if missing(educ) | missing(speduc) 

// Now we want to store some key statistics as scalars
sum educ
scalar educmean = r(mean) // We store the mean of r's educ
scalar educsd = r(sd) // We store the SD of r's educ
scalar n = r(N) // Storing our sample size will later come in handy
scalar df = n-2 // ... as will storing our degrees of freedom
sum speduc // Now we'll do all the same stuff for the spouse's educ
scalar speducmean = r(mean)
scalar speducsd = r(sd)
gen educdev = educ - educmean // This produces a centered version of educ
gen speducdev = speduc - speducmean // Same for speduc

// Now let's get the correlation. First, we take the dot product
gen dotproduct = educdev*speducdev
quietly sum dotproduct
scalar covariance = r(sum)/(n-1) // Now we get the covariance 
scalar samplecorr = covariance/(educsd*speducsd) // Now we find the correlation
corr educ speduc // we can compare our results to Stata
di samplecorr // Checks out!

//Now we get the slope and the intercept
scalar betahat1 = samplecorr*(educsd/speducsd) // get the slope 
// Ybar = beta-hat-0 + beta-hat-1(xbar) must be a point on the line
scalar betahat0 = educmean - (betahat1*speducmean) 


// Now let's find the residuals

// First, we obtain the estimate for each observation
gen yhat = betahat0 + (speduc)*betahat1
// Now we get the residual
gen residual = (educ - yhat)
gen SR = residual^2 // square them 
quietly sum SR // we quietly sum to get the sum of squared residuals
scalar SSR =  r(sum) // store SSR 
scalar conditionalSD = sqrt(SSR/(df)) // calculate conditional SD (AKA RMSE AKA "standard error of regression")

// Now let's calculate the standard error of the slope (which is NOT the SE of regression, a misleading term)

gen deviationsX2 = speducdev^2 // We need the squared deviations of X around its mean first
quietly sum deviationsX2 
scalar TSSX = r(sum) // Now we need their sum 
scalar sqrtTSSX = sqrt(TSSX) // and the root of the sum 
scalar SEb1 = conditionalSD/sqrtTSSX // finally we can take the SE of b1
scalar tstat = betahat1/SEb1 // and calculate a t-statistic 
scalar pvalue2T = ttail(n-2, tstat)*2 // and finally a p-value 

// Let's get the other sums of squares

gen modeldev = (yhat-educmean)^2 // We first put together the model deviations
quietly sum modeldev 
scalar MSS =r(mean) // Now we find the model sum of squares
gen deviationsY2 = (educdev)^2 // Now we get the sum of squares of Y. We already had the centered variable
quietly sum deviationsY2 
scalar TSS =r(mean) // We take the sum 
scalar rsquared = MSS/TSS // And now we're able to get r-squared
di MSS/TSS
di samplecorr^2 // and MSS/TSS agrees with r-squared

// The big reveal: comparing our results to what Stata gives
matrix hand_calc_results = betahat1, SEb1, tstat, pvalue2T, betahat0, rsquared // Put results in matrix
matrix colnames hand_calc_results = betahat1 SEb1 tstat pvalue2T betahat0 rsquared // Title columns
reg educ speduc // Automated regression 
matrix list hand_calc_results // display our results matrix


