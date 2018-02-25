NBA Bootstrap Resampling
================
Hubert Luo
February 17, 2018

Boostrap Resampling: Points
---------------------------

``` r
dat <- read.csv('nba2017-players.csv', stringsAsFactors = FALSE)
dat_points <- na.omit(dat$points)
n = 100 #Sample size

#Take simple random sample of size n
srs = sample(dat_points,n)

#Take boostrap sample by repeating each entry number of times needed to approximate population
bootstrapSamples = numeric()
numRepeats = round(length(dat_points)/n)
for (i in 1:n){
  bootstrapSamples = c(bootstrapSamples, rep(srs[i],numRepeats))
}
```

Compare Accuracies
------------------

``` r
mean(dat_points) #Population 
```

    ## [1] 546.6054

``` r
mean(srs) #Simple Random Sample
```

    ## [1] 541.86

``` r
mean(bootstrapSamples) #Bootstrap Resampling
```

    ## [1] 541.86

``` r
sd(dat_points)
```

    ## [1] 489.0156

``` r
sd(srs)
```

    ## [1] 453.6185

``` r
sd(bootstrapSamples)
```

    ## [1] 451.91

Simple Random Sampling: All Parameters
--------------------------------------

We will take a sample of 100 players and try to estimate population parameters (for all 441 NBA players) from their sample statistics.

``` r
numeric_dat <- dat[, -c(1,2,3,8)] #Keep the numeric variables to calculate population parameters
pop_params <- numeric() #Create empty vector to store population parameters
for (sample_var in 1:(ncol(numeric_dat))){
  pop_params = c(pop_params,mean(numeric_dat[[sample_var]])) #Append population parameters of each variable to previously created vector
}
names(pop_params) <- colnames(numeric_dat) #Label what each parameter is
pop_params
```

    ##       height       weight          age   experience       salary 
    ## 7.915420e+01 2.201655e+02 2.629252e+01 4.675737e+00 6.187014e+06 
    ##        games      minutes       points      points3      points2 
    ## 5.370748e+01 1.244043e+03 5.466054e+02 4.970748e+01 1.525079e+02 
    ##      points1 
    ## 9.246712e+01

``` r
sample_dat <- dat[sample(nrow(dat),100),] #Take sample of 100 players without replacement

numeric_sample_dat <- sample_dat[, -c(1,2,3,8)] #Keep the numeric variables to calculate sample statistics
x_bars <- numeric() #Create empty vector to store sample statistics
for (sample_var in 1:(ncol(numeric_sample_dat))){
  x_bars = c(x_bars,mean(numeric_sample_dat[[sample_var]])) #Append sample statistic of each variable to previously created vector
}
names(x_bars) <- colnames(numeric_sample_dat) #Label what each statistic is 
x_bars
```

    ##     height     weight        age experience     salary      games 
    ##      78.72     219.09      26.72       5.11 7301737.89      56.35 
    ##    minutes     points    points3    points2    points1 
    ##    1377.16     625.67      56.28     174.03     108.77

``` r
#Calculate standard errors

N = nrow(dat) #Population size
n = nrow(sample_dat) #Sample size

s_xbars = numeric() #create empty vector to store SE's
for (sample_var in 1:(ncol(numeric_sample_dat))){
  s_xbars = c(s_xbars,sqrt(var(numeric_sample_dat[[sample_var]])/(n-1)*((N-n)/N))) #Append SE of each variable to previously created vector
}
names(s_xbars) <- colnames(numeric_sample_dat) #Label what each statistic is 
s_xbars
```

    ##       height       weight          age   experience       salary 
    ## 3.283020e-01 2.277775e+00 3.694586e-01 3.822245e-01 6.420335e+05 
    ##        games      minutes       points      points3      points2 
    ## 2.147130e+00 7.304323e+01 4.382141e+01 5.202722e+00 1.322501e+01 
    ##      points1 
    ## 1.027915e+01

``` r
#Calculate bounds for 95% Confidence Intervals
uBounds = s_xbars + 1.96*s_xbars
uBounds
```

    ##       height       weight          age   experience       salary 
    ## 9.717738e-01 6.742214e+00 1.093598e+00 1.131385e+00 1.900419e+06 
    ##        games      minutes       points      points3      points2 
    ## 6.355504e+00 2.162079e+02 1.297114e+02 1.540006e+01 3.914603e+01 
    ##      points1 
    ## 3.042627e+01

``` r
lBounds = s_xbars - 1.96*s_xbars
lBounds
```

    ##        height        weight           age    experience        salary 
    ## -3.151699e-01 -2.186664e+00 -3.546803e-01 -3.669356e-01 -6.163522e+05 
    ##         games       minutes        points       points3       points2 
    ## -2.061245e+00 -7.012150e+01 -4.206855e+01 -4.994613e+00 -1.269601e+01 
    ##       points1 
    ## -9.867980e+00

Bootstrap
---------

Now, we will take our sample from earlier and apply the boostrap.

``` r
sample_factor = round(nrow(dat)/100) #Calculate how many times we need to repeat each observation
bootstrap_dat <- sample_dat[rep(seq_len(nrow(sample_dat)),each = sample_factor),] #Repeats each row sample_factor number of times
numeric_bootstrap <- bootstrap_dat[, -c(1,2,3,8)] #Keep the numeric variables to calculate sample statistics
bootstrap_x_bars <- numeric() #Create empty vector to store sample statistics
for (sample_var in 1:(ncol(numeric_bootstrap))){
  bootstrap_x_bars = c(bootstrap_x_bars,mean(numeric_bootstrap[[sample_var]])) #Append sample statistic of each variable to previously created vector
}
names(bootstrap_x_bars) <- colnames(numeric_bootstrap) #Label what each statistic is 
bootstrap_x_bars
```

    ##     height     weight        age experience     salary      games 
    ##      78.72     219.09      26.72       5.11 7301737.89      56.35 
    ##    minutes     points    points3    points2    points1 
    ##    1377.16     625.67      56.28     174.03     108.77

``` r
#Now, calculate standard errors from bootstrap

n = nrow(bootstrap_dat) #Sample size has changed!

bootstrap_s_xbars = numeric() #create empty vector to store SE's
for (sample_var in 1:(ncol(numeric_bootstrap))){
  bootstrap_s_xbars = c(bootstrap_s_xbars,sqrt(var(numeric_bootstrap[[sample_var]])/(n-1)*((N-n)/N))) #Append SE of each variable to previously created vector
}
names(bootstrap_s_xbars) <- colnames(numeric_bootstrap) #Label what each statistic is 
bootstrap_s_xbars
```

    ##       height       weight          age   experience       salary 
    ## 5.649116e-02 3.919384e-01 6.357302e-02 6.576966e-02 1.104752e+05 
    ##        games      minutes       points      points3      points2 
    ## 3.694582e-01 1.256860e+01 7.540382e+00 8.952363e-01 2.275637e+00 
    ##      points1 
    ## 1.768740e+00

``` r
#Calculate bounds for 95% Confidence Intervals
bootstrap_uBounds = bootstrap_s_xbars + 1.96*bootstrap_s_xbars
bootstrap_uBounds
```

    ##       height       weight          age   experience       salary 
    ## 1.672138e-01 1.160138e+00 1.881761e-01 1.946782e-01 3.270065e+05 
    ##        games      minutes       points      points3      points2 
    ## 1.093596e+00 3.720306e+01 2.231953e+01 2.649899e+00 6.735887e+00 
    ##      points1 
    ## 5.235471e+00

``` r
bootstrap_lBounds = bootstrap_s_xbars - 1.96*bootstrap_s_xbars
bootstrap_lBounds
```

    ##        height        weight           age    experience        salary 
    ## -5.423152e-02 -3.762609e-01 -6.103010e-02 -6.313887e-02 -1.060562e+05 
    ##         games       minutes        points       points3       points2 
    ## -3.546799e-01 -1.206586e+01 -7.238767e+00 -8.594268e-01 -2.184612e+00 
    ##       points1 
    ## -1.697991e+00
