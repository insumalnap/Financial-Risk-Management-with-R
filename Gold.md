# Financial Risk Management

A hedge fund invested 1 billion of gold in the London Bullion Market.

We would like to know:
- How much money could they possibly lose? 
- Can they afford to lose this amount of money?

## Data Collection and Preparation
 
Load the required packages.

``` r
library(quantmod)
library(moments)
library(MASS)
library(metRology)
library(rugarch)
library(ggplot2)
```
  
Retrieve the price of gold using the quantmod library.

``` r
gold <- getSymbols('GOLDPMGBD228NLBM', src = 'FRED', auto.assign = FALSE)
```
  
Remove the missing data

``` r
gold <- na.omit(gold)
```

Include only the data from 1980 to 2017.

``` r
gold <- gold["1980-01-01/2017-12-31"]
```
 
Calculate the daily log returns and remove the first observation which
is missing.

``` r
logret <- diff(log(gold))[-1]
head(logret)
```

    ##            GOLDPMGBD228NLBM
    ## 1980-01-03      0.125005427
    ## 1980-01-04     -0.075322007
    ## 1980-01-07      0.074533052
    ## 1980-01-08     -0.037801043
    ## 1980-01-09     -0.004600731
    ## 1980-01-10     -0.007189817

Plot the daily log returns.

``` r
ggplot(logret) + geom_line(aes(x=index(logret), y=logret)) + 
  labs(title = 'Daily Log Returns of Gold', x='', y='Log Returns')
```

![](Gold_files/unnamed-chunk-6-1.png)<!-- -->
 
Calculate the daily discrete/arithmetic returns from the daily log
returns.

``` r
ret <- exp(logret) - 1 
```

Log returns is preferred as it allows for easier time-aggregation
i.e. it is easy to go from one-day log returns to longer period log
returns. For example, weekly log return is just the sum of all daily log
returns in the week.
 
Calculate the weekly, monthly, quarterly, and yearly log returns and
discrete returns.

``` r
logret.w <- apply.weekly(logret,sum) # sums the daily log returns from Monday to Friday
logret.m <- apply.monthly(logret,sum)
logret.q <- apply.quarterly(logret,sum)
logret.y <- apply.yearly(logret,sum)
```

The longer period arithmetic returns can be calculated from the
corresponding log returns using the same formula.
 
Calculate longer period arithmetic returns from the log returns.

``` r
ret.w <- exp(logret.w)-1
ret.m <- exp(logret.m)-1
ret.q <- exp(logret.q)-1
ret.y <- exp(logret.y)-1
```

## 1-day Value-at-Risk at 95% Confidence Level and Expected Shortfall

-   Value-at-Risk (VaR) is the amount that a portfolio might lose over a
    specific period at a given confidence level.
-   Expected Shortfall (ES) is the expected loss given that the loss is
    larger than the VaR.
  
### Method 1. Estimate the VaR and ES of the normal distribution

Estimate the parameters of the normal distribution (mean and standard
deviation)

``` r
mu <- mean(logret)
sig <- sd(logret)
cbind(mu, sig)
```

    ##                mu        sig
    ## [1,] 8.776413e-05 0.01206868
 
Estimate the VaR and ES of the normal distribution with the estimated
parameters *mu* and *sig*

``` r
alpha <- 0.05
var <- qnorm(alpha,mu,sig)
es <- mu-sig*dnorm(qnorm(alpha,0,1),0,1)/alpha 
cbind(var, es)
```

    ##              var          es
    ## [1,] -0.01976345 -0.02480646

Estimate the VaR and ES of the portfolio

``` r
usdvar <- 1000 * (exp(var)-1) # in million dollars
usdes <- 1000 * (exp(es)-1) # in million dollars
cbind(usdvar, usdes)
```

    ##         usdvar     usdes
    ## [1,] -19.56943 -24.50131

At 95% confidence level, the hedge fund is not likely to lose more than
19.61 million over a day. If losses fell by more than the VaR, the hedge
fund is expected to lose 24.56 million in a day.
 
### Method 2. Estimate the VaR and ES by simulating from the normal distribution

Simulate 100,000 1-day log returns from the normal distribution with the
estimated parameters *mu* and *sig*. Calculate the VaR and ES for these
100,000 outcomes.

``` r
alpha <- 0.05
set.seed(123789)
rvec.norm <- rnorm(100000,mu,sig)
var.norm <- quantile(rvec.norm,alpha)
es.norm <- mean(rvec.norm[rvec.norm<var.norm])
cbind(var.norm,es.norm)
```

    ##       var.norm     es.norm
    ## 5% -0.01986583 -0.02504584
 
Estimate the VaR and ES of the portfolio

``` r
usdvar.norm <- 1000 * (exp(var.norm)-1)
usdes.norm <- 1000 * (exp(es.norm)-1)
cbind(usdvar.norm,usdes.norm)
```

    ##     usdvar.norm usdes.norm
    ## 5%    -19.66981  -24.73479

### Method 3. Estimate the VaR and ES by simulating from the observed distribution of the data

Unlike the previous two methods, this method does not assume that the
log returns follow a normal distribution.
 
Draw 100,000 1-day log returns from the empirical distribution.
Calculate the VaR and ES for these 100,000 outcomes.

``` r
alpha <- 0.05
set.seed(123789)
rvec.obs <- sample(as.vector(logret),100000,replace=TRUE)
var.obs <- quantile(rvec.obs,alpha) 
es.obs <- mean(rvec.obs[rvec.obs<var.obs])
cbind(var.obs,es.obs)
```

    ##        var.obs      es.obs
    ## 5% -0.01782277 -0.02901677

Estimate the VaR and ES of the portfolio

``` r
usdvar.obs <- 1000 * (exp(var.obs)-1)
usdes.obs <- 1000 * (exp(es.obs)-1)
cbind(usdvar.obs,usdes.obs)
```

    ##   usdvar.obs usdes.obs
    ## 5% -17.66488 -28.59983
 
Compare the results of the three methods

``` r
# VaR
rbind(var, var.norm, var.obs)
```

    ##                   5%
    ## var      -0.01976345
    ## var.norm -0.01986583
    ## var.obs  -0.01782277

``` r
# VaR in Million USD
rbind(usdvar, usdvar.norm, usdvar.obs)
```

    ##                    5%
    ## usdvar      -19.56943
    ## usdvar.norm -19.66981
    ## usdvar.obs  -17.66488

``` r
# ES
rbind(es, es.norm, es.obs)
```

    ##                [,1]
    ## es      -0.02480646
    ## es.norm -0.02504584
    ## es.obs  -0.02901677

``` r
# ES in Million USD
rbind(usdes, usdes.norm, usdes.obs)
```

    ##                 [,1]
    ## usdes      -24.50131
    ## usdes.norm -24.73479
    ## usdes.obs  -28.59983

The results of the third method are different from the results of the
other two methods. The difference is due to the normality assumption
made in the first two methods.

---
  
**Check if daily log returns are normally distributed.**
 
Check the density plot and qq plot.

``` r
ggplot(logret) + geom_density(aes(x=logret)) + labs(title = 'Distribution of Returns')
```

![](Gold_files/unnamed-chunk-18-1.png)<!-- -->

``` r
qqnorm(logret)
qqline(logret)
```

![](Gold_files/unnamed-chunk-18-2.png)<!-- -->

Based on the plots, the daily log returns of gold does not seem to
follow the normal distribution.
  
Calculate the skewness and kurtosis to further check.

``` r
rvec <- as.vector(logret)
sk <- skewness(rvec)
kt <- kurtosis(rvec)
cbind(sk, kt)
```

    ##               sk       kt
    ## [1,] -0.08928404 15.42853

  
The skewness and kurtosis of the normal distribution are 0 and 3,
respectively. Meanwhile, the skewness and kurtosis of the distribution
of log returns are 0.42 and 6.87. This suggests that the data is not
normally distributed.
  
Perform the Jarque-Bera Test for Normality to confirm.

``` r
# Null Hypothesis: The daily log returns are normally distributed.
jarque.test(rvec) 
```

    ## 
    ##  Jarque-Bera Normality Test
    ## 
    ## data:  rvec
    ## JB = 61330, p-value < 2.2e-16
    ## alternative hypothesis: greater

*Reject the null hypothesis. There is enough evidence to say that the
daily log returns is not normally distributed. In this case, it is
therefore incorrect to calculate the VaR and ES under the assumption of
normality.*

---

### Method 4. Estimate the VaR and ES by simulating from the student-t distribution
  
The distribution of daily log returns is heavy/fat-tailed as indicated by the
high kurtosis, which means extreme outcomes happened more than expected.
A student-t distribution may be a better fit to the data. In fact, student-t
distribution is one of the most commonly used fat-tailed distributions in the
literature for modeling asset returns.

Estimate the parameters of the student-t distribution (mean, standard
deviation, degrees of freedom)

``` r
t.fit <- fitdistr(rvec, "t")
t.fit$estimate
```

Simulate 100,000 1-day log returns from the scaled student-t
distribution with the estimated parameters. Calculate the VaR and ES for
these 100,000 outcomes.

``` r
alpha <- 0.05
set.seed(123789)
rvec.t <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
var.t <- quantile(rvec.t,alpha)
es.t <- mean(rvec.t[rvec.t<var.t])
cbind(var.t,es.t)
```

    ##          var.t        es.t
    ## 5% -0.01609432 -0.02337146

Estimate the VaR and ES of the portfolio

``` r
usdvar.t <- 1000 * (exp(var.t)-1)
usdes.t <- 1000 * (exp(es.t)-1)
cbind(usdvar.t,usdes.t)
```

    ##    usdvar.t   usdes.t
    ## 5% -15.9655 -23.10047

## Changing the Time Horizon: 10-day Value-at-Risk at 95% Confidence Level and Expected Shortfall

Aside from one day, we can also compute for VaR and ES over longer time
horizon.

### Method 1: Calculate the VaR and ES by simulating from the student-t distribution with the estimated parameters

Simulate ten 1-day log returns from the scaled student t distribution
with the estimated parameters then add them up. Repeat this 100,000
times then calculate the VaR and ES for these 100,000 outcomes.

``` r
alpha <- 0.05
set.seed(123789)
rvec.t10 <- rep(0,100000) 
for (i in 1:10) {
  rvec.t10 <- rvec.t10+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
var.t10 <- quantile(rvec.t10,alpha)
es.t10 <- mean(rvec.t10[rvec.t10<var.t10])
cbind(var.t10,es.t10)
```

    ##        var.t10      es.t10
    ## 5% -0.05239492 -0.06861644

Estimate the 10-day VaR and ES of the portfolio

``` r
usdvar.t10 <- 1000 * (exp(var.t10)-1)
usdes.t10 <- 1000 * (exp(es.t10)-1)
cbind(usdvar.t10,usdes.t10)
```

    ##    usdvar.t10 usdes.t10
    ## 5% -51.04597  -66.31526

### Method 2: Calculate the VaR and ES by simulating from the empirical distribution with “IID” draws
  
Draw ten 1-day log returns from the empirical distribution then add them
up. Repeat this 100,000 times then calculate the VaR and ES for these
100,000 outcomes.

``` r
alpha <- 0.05
set.seed(123789)
rvec.obs10 <- rep(0,100000)
for (i in 1:10) {
  rvec.obs10 <- rvec.obs10+sample(as.vector(logret),100000,replace=TRUE)
}
var.obs10 <- quantile(rvec.obs10,alpha)
es.obs10 <- mean(rvec.obs10[rvec.obs10<var.obs10])
cbind(var.obs10,es.obs10)
```

    ##      var.obs10    es.obs10
    ## 5% -0.06141883 -0.08291482

Estimate the 10-day VaR and ES of the portfolio

``` r
usdvar.obs10 <- 1000 * (exp(var.obs10)-1)
usdes.obs10 <- 1000 * (exp(es.obs10)-1)
cbind(usdvar.obs10,usdes.obs10)
```

    ##    usdvar.obs10 usdes.obs10
    ## 5%   -59.57072    -79.57045
  
### Method 3: Calculate the VaR and ES by simulating from the empirical distribution with “block” draws
  
Draw ten consecutive 1-day log returns from the empirical distribution
then add them up. Repeat this 100,000 times then calculate the VaR and
ES for these 100,000 outcomes.

``` r
alpha <- 0.05
set.seed(123789)
rvec.block10 <- rep(0,100000)
posn <- seq(from=1,to=length(rvec)-9,by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec.block10 <- rvec.block10+ rvec[rpos]
  rpos <- rpos+1
}
var.block10 <- quantile(rvec.block10,alpha)
es.block10 <- mean(rvec.block10[rvec.block10<var.block10])
cbind(var.block10,es.block10)
```

    ##    var.block10  es.block10
    ## 5% -0.05442822 -0.08232616

Estimate the 10-day VaR and ES of the portfolio

``` r
usdvar.block10 <- 1000 * (exp(var.block10)-1)
usdes.block10 <- 1000 * (exp(es.block10)-1)
cbind(usdvar.block10,usdes.block10)
```

    ##    usdvar.block10 usdes.block10
    ## 5%     -52.97351      -79.02847

Method 2 draws randomly across days, and with that, time dependence (if
any) in the actual data is destroyed. Method C, on the other hand,
preserves time dependence within any block of consecutive data. If there
is no time dependence in data, then Method 2 and 3 should yield similar
results.

*Until now, VaR and ES were estimated without taking into account how the data was ordered. To check if the order of data is important,
the presence of serial correlation and volatility clustering is
examined.*

## Serial Correlation and Volatility Clustering

-   Serial correlation or autocorrelation is the correlation between a
    variable and its lagged values.

-   Volatility clustering happens when periods of high (low) volatility
    are followed by periods of high volatility (low).
  
**Check for evidence of serial correlation and volatility clustering**
  
Plot the autocorrelation function (ACF) of log returns to check for
serial correlation.

``` r
acf(logret)
```

![](Gold_files/fig-1.png)<!-- -->

The plot shows that the log returns are not serially correlated as there
are not too many bars outside the 95% confidence bands.
  
Plot the autocorrelation function (ACF) of the absolute value of log
returns to check for volatility clustering.

``` r
acf(abs(logret))
```

![](Gold_files/unnamed-chunk-30-1.png)<!-- -->

Volatility clustering is present in the log returns as there are many
bars outside the 95% confidence bands.

Re-arrange / change the order of log returns then check the ACF of the
absolute value. Compare the results when log returns are arranged
chronologically.

``` r
set.seed(123789)
df <- data.frame(logret, row.names=NULL)
logret.shuffled <- df[sample(nrow(logret)),]
plot(logret.shuffled, type='l')
```

![](Gold_files/unnamed-chunk-31-1.png)<!-- -->

``` r
acf(abs(logret.shuffled))
```

![](Gold_files/unnamed-chunk-31-2.png)<!-- -->

After the data was randomly rearranged, the volatility clustering seems
to have disappeared. This means that the ‘order’ of data matters
indeed.

## Generalized Autoregressive Conditional Heteroskedasticity (GARCH) Model

GARCH models are shown to successfully model time-varying volatility in
financial time-series data.
- Mean Equation: ![Equation](https://latex.codecogs.com/gif.latex?%5Cinline%20r_%7Bt%7D%20%3D%20a_%7B0%7D%20&plus;%20%5Csqrt%7Bh_%7Bt%7D%7D%5Cepsilon_%7Bt%7D)
- Variance Equation: ![Equation](https://latex.codecogs.com/gif.latex?%5Cinline%20h_%7Bt%7D%20%3D%20%5Calpha_%7B0%7D%20&plus;%20%5Cbeta_%7Bt%7Dh_%7Bt-1%7D%20&plus;%20%5Calpha_%7B1%7D%5Cepsilon%5E%7B2%7D_%7Bt-1%7D)

Estimate a GARCH(1,1) model with student-t distribution

``` r
uspec <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0,0), include.mean = TRUE), # constant only
                    distribution.model = "std") # specifications of the garch model
garch.fit <- ugarchfit(spec = uspec, data = logret[,1])
garch.fit
```

    ## 
    ## *---------------------------------*
    ## *          GARCH Model Fit        *
    ## *---------------------------------*
    ## 
    ## Conditional Variance Dynamics    
    ## -----------------------------------
    ## GARCH Model  : sGARCH(1,1)
    ## Mean Model   : ARFIMA(0,0,0)
    ## Distribution : std 
    ## 
    ## Optimal Parameters
    ## ------------------------------------
    ##         Estimate  Std. Error   t value Pr(>|t|)
    ## mu      0.000020    0.000071   0.28202 0.777929
    ## omega   0.000000    0.000000   2.25715 0.023999
    ## alpha1  0.058964    0.002958  19.93080 0.000000
    ## beta1   0.940034    0.002729 344.42763 0.000000
    ## shape   4.705505    0.209036  22.51054 0.000000
    ## 
    ## Robust Standard Errors:
    ##         Estimate  Std. Error  t value Pr(>|t|)
    ## mu      0.000020    0.000065  0.30652 0.759207
    ## omega   0.000000    0.000001  0.67011 0.502787
    ## alpha1  0.058964    0.021674  2.72054 0.006518
    ## beta1   0.940034    0.019661 47.81121 0.000000
    ## shape   4.705505    0.283641 16.58967 0.000000
    ## 
    ## LogLikelihood : 31116.12 
    ## 
    ## Information Criteria
    ## ------------------------------------
    ##                     
    ## Akaike       -6.5311
    ## Bayes        -6.5274
    ## Shibata      -6.5311
    ## Hannan-Quinn -6.5299
    ## 
    ## Weighted Ljung-Box Test on Standardized Residuals
    ## ------------------------------------
    ##                         statistic p-value
    ## Lag[1]                     0.3801  0.5375
    ## Lag[2*(p+q)+(p+q)-1][2]    2.3240  0.2142
    ## Lag[4*(p+q)+(p+q)-1][5]    4.9806  0.1546
    ## d.o.f=0
    ## H0 : No serial correlation
    ## 
    ## Weighted Ljung-Box Test on Standardized Squared Residuals
    ## ------------------------------------
    ##                         statistic   p-value
    ## Lag[1]                      12.84 0.0003401
    ## Lag[2*(p+q)+(p+q)-1][5]     13.76 0.0009531
    ## Lag[4*(p+q)+(p+q)-1][9]     14.95 0.0036868
    ## d.o.f=2
    ## 
    ## Weighted ARCH LM Tests
    ## ------------------------------------
    ##             Statistic Shape Scale P-Value
    ## ARCH Lag[3]     0.311 0.500 2.000  0.5771
    ## ARCH Lag[5]     1.622 1.440 1.667  0.5609
    ## ARCH Lag[7]     2.133 2.315 1.543  0.6891
    ## 
    ## Nyblom stability test
    ## ------------------------------------
    ## Joint Statistic:  1084.52
    ## Individual Statistics:               
    ## mu       0.5609
    ## omega  295.9701
    ## alpha1   0.6067
    ## beta1    0.5471
    ## shape    0.8520
    ## 
    ## Asymptotic Critical Values (10% 5% 1%)
    ## Joint Statistic:          1.28 1.47 1.88
    ## Individual Statistic:     0.35 0.47 0.75
    ## 
    ## Sign Bias Test
    ## ------------------------------------
    ##                    t-value   prob sig
    ## Sign Bias           1.1842 0.2364    
    ## Negative Sign Bias  0.2647 0.7913    
    ## Positive Sign Bias  0.9225 0.3563    
    ## Joint Effect        2.2437 0.5234    
    ## 
    ## 
    ## Adjusted Pearson Goodness-of-Fit Test:
    ## ------------------------------------
    ##   group statistic p-value(g-1)
    ## 1    20     38.23    5.542e-03
    ## 2    30     58.97    8.293e-04
    ## 3    40     80.69    9.885e-05
    ## 4    50     91.35    2.300e-04
    ## 
    ## 
    ## Elapsed time : 1.040894

Save the estimated parameters and fitted values.

``` r
parm <- garch.fit@fit$coef
fitval <- cbind(logret[,1], garch.fit@fit$sigma, garch.fit@fit$z)
names(fitval) <- c('logret', 'sigma', 'err' )
```

-   *parm* contains the estimated parameters of the GARCH(1,1) model
    with student-t distribution
-   *logret* is the daily log returns
-   *sigma* is the fitted values of ![Equation](https://latex.codecogs.com/gif.latex?%5Cinline%20%5Csqrt%7Bh_%7Bt%7D%7D)
-   *err* is the fitted values of ![Equation](https://latex.codecogs.com/gif.latex?%5Cinline%20%5Cepsilon_%7Bt%7D)

Check the mean, standard deviation, skewness, and kurtosis of the fitted
values of ![Equation](https://latex.codecogs.com/gif.latex?%5Cinline%20%5Cepsilon_%7Bt%7D)

``` r
mean(fitval$err) # close to 0
```

    ## [1] 0.008788697

``` r
sd(fitval$err) # close to 1
```

    ## [1] 1.020248

``` r
skewness(fitval$err) # close to 0
```

    ##         err 
    ## 0.003963088

``` r
kurtosis(fitval$err) # larger than 3
```

    ##      err 
    ## 9.448774

Examine the ACF of z and the absolute value of z to check if the GARCH
model has captured volatility clustering in the data

``` r
acf(fitval$err)
```

![](Gold_files/unnamed-chunk-35-1.png)<!-- -->

``` r
acf(abs(fitval$err))
```

![](Gold_files/unnamed-chunk-35-2.png)<!-- -->
 
The plot shows that volatility clustering is no longer present. The
model was able to explain volatility clustering in the data.

Get the annualized volatilty by multiplying the fitted values of
![Equation](https://latex.codecogs.com/gif.latex?%5Cinline%20%5Csqrt%7Bh_%7Bt%7D%7D) by ![Equation](https://latex.codecogs.com/gif.latex?%5Cinline%20%5Csqrt%7B252%7D).

``` r
annual.vol <- fitval$sigma * sqrt(252)

ggplot(fitval$sigma) + geom_line(aes(x=index(annual.vol), y=annual.vol)) + 
  labs(title = 'Annualized Fitted Volatility of Gold', x='', y='Annual Volatility')
```

![](Gold_files/unnamed-chunk-36-1.png)<!-- -->
  
The plot shows that the volatility of the log returns varies over time.
Consequently, the VaR and ES that will be computed using this model are
also time-varying.

## 1-day Value-at-Risk at 95% Confidence Level and Expected Shortfall from the GARCH(1,1) Model
  
### Estimate the 1-day ahead VaR and ES from the GARCH model by bootstrapping from the fitted ![Equation](https://latex.codecogs.com/gif.latex?%5Cinline%20%5Cepsilon_%7Bt%7D) (standardized residuals)
  
Draw 100,000 outcomes from the standardized residuals then calculate the
VaR and ES for the next day using these outcomes.

``` r
alpha <- 0.05
set.seed(123789) 
garch.boot <- ugarchboot(garch.fit, method='Partial', sampling='raw', n.ahead=1, n.bootpred=100000, solver='solnp')
rvec.boot <- garch.boot@fseries # simulated outcomes
var.boot <- quantile(rvec.boot,alpha)
es.boot <- mean(rvec.boot[rvec.boot<var.boot])
cbind(var.boot, es.boot)
```

    ##        var.boot     es.boot
    ## 5% -0.009162998 -0.01337916

Estimate the VaR and ES of the portfolio

``` r
usdvar.boot <- 1000 * (exp(var.boot)-1)
usdes.boot <- 1000 * (exp(es.boot)-1)
cbind(usdvar.boot,usdes.boot)
```

    ##    usdvar.boot usdes.boot
    ## 5%  -9.121146   -13.29006

The 1-day ahead VaR and ES is much smaller than the ones estimated
earlier. The VaR and ES calculated previously did not take into the
‘ordering’ of data (and volatility clustering), thus they apply to the
‘typical’ outcome.
  
Meanwhile, the VaR and ES calculated using the GARCH model varies over
time, and therefore apply only to a specific day. In this case, the
1-day ahead VaR and ES computed using the data is specific to 02 Jan
2018 (i.e. since the data is only until 31 Dec 2017).
  
### Estimate the 1-day ahead VaR and ES from the GARCH model for a specific day by bootstrapping from standardized residuals

On 19 Oct 1987, US equities fell 19% – the largest single day decline in
the sample period 1980-2017. Calculate the VaR and ES for the next day.

Estimate the GARCH model using the data until 19 Oct 1987 only, then
compute for the VaR and ES for 20 Oct 1987.

``` r
garch.fit1 <- ugarchfit(spec = uspec, data = logret["1980-01-01/1987-10-19"])

parm1 <- garch.fit1@fit$coef
fitval1 <- cbind(logret["1980-01-01/1987-10-19"], garch.fit1@fit$sigma, garch.fit1@fit$z)
names(fitval1) <- c('logret', 'sigma', 'err' )

alpha <- 0.05
set.seed(123789) 
garch.boot1 <- ugarchboot(garch.fit1, method='Partial', sampling='raw', n.ahead=1, n.bootpred=100000, solver='solnp')
rvec.boot1 <- garch.boot1@fseries
var.boot1 <- quantile(rvec.boot1,alpha)
es.boot1 <- mean(rvec.boot1[rvec.boot1<var.boot1])
cbind(var.boot1, es.boot1)
```

    ##      var.boot1    es.boot1
    ## 5% -0.01911114 -0.02866925
  
### Estimate the ‘rolling’ 1-day ahead VaR from the GARCH model

Since the VaR calculated using the GARCH model is time-varying and
therefore only apply to a specific day, we can calculate the VaR for
each day of 2017 by re-estimating the GARCH model (like we did earlier)
for each day. The procedure is as follows:

1.  Estimate the GARCH model from the start of the sample until the end
    of 2016.
2.  At the end of 2016, calculate the VaR for the first day of 2017.
3.  Re-estimate the GARCH model from the start of our sample again but
    this time ending with the first day of 2017.
4.  Calculate the VaR for the second day of 2017, and again re-estimate
    the GARCH model from the start of our sample until the second day
    of 2017.
5.  Keep on going like this, adding one day at a time.
  
Implement the process using the ‘ugarchroll’ function.

``` r
alpha <- 0.05
n2016 <- length(logret["1980-01-01/2016-12-31"])
garch.roll <- ugarchroll(spec=uspec, 
                         data=logret, 
                         n.ahead=1, 
                         forecast.length=1, 
                         n.start= n2016, 
                         refit.every=1, 
                         refit.window="recursive", 
                         calculate.VaR=TRUE, 
                         VaR.alpha=alpha, 
                         keep.coef=TRUE)
```

Plot the log returns and 1-day ahead VaR.

``` r
logret.roll = logret["2017-01-01/2017-12-31"]
var.roll <- garch.roll@forecast$VaR$`alpha(5%)`

ggplot(logret.roll) + labs(title = 'Log Returns of Gold in 2017', x='', y='Log Returns') +
  geom_line(aes(x=index(logret.roll), y=logret.roll, color='Log Returns')) + 
  geom_line(aes(x=index(logret.roll), y=var.roll, color='1-day VaR')) +
  scale_color_manual(name=NULL, values = c('Log Returns'='black', '1-day VaR' = 'red'))
```

![](Gold_files/unnamed-chunk-41-1.png)<!-- -->

If the model is correct, then there should be **less than 5%** of the
daily log returns below VaR.

``` r
sum(logret.roll<var.roll)/length(logret.roll)
```

    ## [1] 0.03212851

Only **3.21%** of actual log returns are worse than VaR, which means the
GARCH(1,1) model with student-t distribution is a reasonably good model
for the data.