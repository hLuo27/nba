NBA Jackknife Resampling
================
Hubert Luo
February 18, 2018

``` r
library(bootstrap)
```

Jackknife Resampling: Points
----------------------------

``` r
dat <- read.csv('nba2017-players.csv', stringsAsFactors = FALSE)
dat_points <- na.omit(dat$points)
n = 100 #Sample size

#Take simple random sample of size n
srs = sample(dat_points,n)

#Remove 1 observation with jackknife
jackknifeSamples = numeric()
for (i in 1:n) {
  jackknifeSamples = c(jackknifeSamples, mean(srs[-i]))
}

#Alternatively, we can use the jackknife function in the bootstrap package
jackknife(srs, theta = mean) #Returns a list consisting of resampling standard error, bias, and values
```

    ## $jack.se
    ## [1] 50.94869
    ## 
    ## $jack.bias
    ## [1] 0
    ## 
    ## $jack.values
    ##   [1] 501.7374 514.9495 508.0606 508.8081 513.0909 515.6465 510.9798
    ##   [8] 518.3939 509.9596 507.9192 516.3838 514.4848 492.6566 514.0101
    ##  [15] 514.2525 517.4141 518.3636 516.4444 515.3030 514.6263 516.0606
    ##  [22] 516.0101 512.6667 508.4040 515.0303 518.2828 518.0606 506.4141
    ##  [29] 509.8990 503.8889 514.1919 514.8889 503.1616 514.3737 515.6364
    ##  [36] 506.7374 516.3737 516.6465 518.4848 511.0202 514.1616 518.2626
    ##  [43] 518.1414 518.3838 515.3939 516.5960 506.6465 514.0808 515.1616
    ##  [50] 516.9192 506.2020 512.8485 512.5758 515.7172 516.8485 510.5051
    ##  [57] 506.9293 517.9697 518.3939 516.1515 514.8081 513.6566 517.9495
    ##  [64] 513.2727 515.5455 517.2121 512.2727 515.3030 512.0404 516.8283
    ##  [71] 518.1818 517.5354 517.0909 518.4040 517.2222 517.0000 518.3535
    ##  [78] 510.2121 518.3535 500.5253 515.0000 514.1313 514.2626 504.9192
    ##  [85] 505.9091 517.1313 498.3030 518.4141 518.0909 512.0505 507.3333
    ##  [92] 513.0606 517.5455 499.9394 515.1919 518.3535 511.3333 514.1010
    ##  [99] 511.1313 515.4242
    ## 
    ## $call
    ## jackknife(x = srs, theta = mean)

``` r
#Remove 2 observations with jackknife
jackknifeSamples2 = numeric()
for (i in 1:n) {
  for (j in (i+1):n) {
    jackknifeSamples2 = c(jackknifeSamples2, mean(srs[-c(i,j)]))
  }
}
```

Compare Samples from Jackknfie Resampling
-----------------------------------------

``` r
plot(jackknifeSamples, xlab = "Sample", ylab = "Average Points", main = "Samples from Jackknife")
```

![](nba_jackknife_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
plot(jackknifeSamples2, xlab = "Sample", ylab = "Average Points", main = "Samples from Jackknife with Two Removals")
```

![](nba_jackknife_files/figure-markdown_github/unnamed-chunk-3-2.png)

Compare Accuracies
------------------

``` r
mean(dat_points) #Population 
```

    ## [1] 546.6054

``` r
mean(srs) #Simple Random Sample
```

    ## [1] 513.31

``` r
mean(jackknifeSamples) #Jackknife Resampling with 1 removal - note that this will be very close to the population parameter (Bias = 0) 
```

    ## [1] 513.31

``` r
mean(jackknifeSamples2) 
```

    ## [1] 513.3109

``` r
sd(dat_points)
```

    ## [1] 489.0156

``` r
sd(srs)
```

    ## [1] 509.4869

``` r
sd(jackknifeSamples)
```

    ## [1] 5.146332

``` r
sd(jackknifeSamples2)
```

    ## [1] 7.277773
