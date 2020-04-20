This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Cmd+Shift+Enter*.
setwd(“/Users/mproberts99/Desktop/R”) getwd()

***Predictive Model Building***

    greenbuildings = read.csv("/Users/mproberts99/Desktop/R/greenbuildings.csv", header = TRUE)
    greenbuildings = na.omit(greenbuildings)

    library(tidyverse)

    ## ── Attaching packages ───────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.4
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ──────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    set.seed(482)

    newbuildings <- greenbuildings %>% 
      select(-CS_PropertyID, -cluster, -cluster_rent, -LEED, -Energystar, -cd_total_07, -hd_total07, -total_dd_07, -Precipitation, -Gas_Costs, -Electricity_Costs, -empl_gr, -leasing_rate, -stories) %>% 
      group_by(Rent)

    lm_all = lm(Rent ~ size + age + renovated + green_rating + amenities, data = newbuildings)
    coef(lm_all)

    ##   (Intercept)          size           age     renovated  green_rating 
    ##  2.866693e+01  6.669617e-06 -1.119709e-02 -3.512693e+00  9.885880e-02 
    ##     amenities 
    ##  7.040377e-02

    lm_step = step(lm_all, 
                scope=~(.)^3)

    ## Start:  AIC=42195.5
    ## Rent ~ size + age + renovated + green_rating + amenities
    ## 
    ##                          Df Sum of Sq     RSS   AIC
    ## + size:age                1    7024.4 1714549 42166
    ## + size:renovated          1    6090.9 1715482 42170
    ## + size:amenities          1    5091.0 1716482 42174
    ## + size:green_rating       1    4130.6 1717442 42179
    ## + age:amenities           1    3392.4 1718181 42182
    ## + renovated:amenities     1    2119.1 1719454 42188
    ## + renovated:green_rating  1    2115.4 1719458 42188
    ## + age:green_rating        1    1197.3 1720376 42192
    ## - green_rating            1       5.7 1721579 42194
    ## - amenities               1       7.8 1721581 42194
    ## <none>                                1721573 42195
    ## - age                     1     698.4 1722271 42197
    ## + age:renovated           1      49.7 1721523 42197
    ## + green_rating:amenities  1      46.6 1721526 42197
    ## - renovated               1   17474.3 1739047 42272
    ## - size                    1   25629.7 1747203 42309
    ## 
    ## Step:  AIC=42165.52
    ## Rent ~ size + age + renovated + green_rating + amenities + size:age
    ## 
    ##                          Df Sum of Sq     RSS   AIC
    ## + size:renovated          1    9370.4 1705178 42125
    ## + age:amenities           1    7918.9 1706630 42131
    ## + size:amenities          1    4202.3 1710346 42148
    ## + renovated:amenities     1    3333.8 1711215 42152
    ## + size:green_rating       1    2218.0 1712331 42157
    ## + renovated:green_rating  1    1818.9 1712730 42159
    ## - amenities               1       0.5 1714549 42164
    ## + age:green_rating        1     841.1 1713708 42164
    ## - green_rating            1      59.5 1714608 42164
    ## <none>                                1714549 42166
    ## + age:renovated           1     116.0 1714433 42167
    ## + green_rating:amenities  1       4.9 1714544 42168
    ## - size:age                1    7024.4 1721573 42195
    ## - renovated               1   17689.3 1732238 42244
    ## 
    ## Step:  AIC=42124.67
    ## Rent ~ size + age + renovated + green_rating + amenities + size:age + 
    ##     size:renovated
    ## 
    ##                          Df Sum of Sq     RSS   AIC
    ## + age:amenities           1    7590.4 1697588 42092
    ## + size:green_rating       1    3062.8 1702115 42113
    ## + size:amenities          1    3008.9 1702169 42113
    ## + renovated:green_rating  1    2376.1 1702802 42116
    ## + age:green_rating        1    1034.8 1704143 42122
    ## - amenities               1       1.0 1705179 42123
    ## - green_rating            1       9.1 1705187 42123
    ## + renovated:amenities     1     478.0 1704700 42124
    ## <none>                                1705178 42125
    ## + age:renovated           1     235.6 1704943 42126
    ## + green_rating:amenities  1      16.4 1705162 42127
    ## - size:renovated          1    9370.4 1714549 42166
    ## - size:age                1   10303.9 1715482 42170
    ## 
    ## Step:  AIC=42091.78
    ## Rent ~ size + age + renovated + green_rating + amenities + size:age + 
    ##     size:renovated + age:amenities
    ## 
    ##                          Df Sum of Sq     RSS   AIC
    ## + size:amenities          1    4176.4 1693411 42075
    ## + size:green_rating       1    2757.7 1694830 42081
    ## + renovated:green_rating  1    2250.6 1695337 42083
    ## + age:green_rating        1    1031.0 1696557 42089
    ## - green_rating            1       0.8 1697589 42090
    ## + renovated:amenities     1     609.1 1696979 42091
    ## + green_rating:amenities  1     456.2 1697132 42092
    ## <none>                                1697588 42092
    ## + age:renovated           1      45.4 1697542 42094
    ## - age:amenities           1    7590.4 1705178 42125
    ## - size:renovated          1    9041.9 1706630 42131
    ## - size:age                1   15179.5 1712767 42159
    ## 
    ## Step:  AIC=42074.52
    ## Rent ~ size + age + renovated + green_rating + amenities + size:age + 
    ##     size:renovated + age:amenities + size:amenities
    ## 
    ##                          Df Sum of Sq     RSS   AIC
    ## + size:green_rating       1    2781.0 1690630 42064
    ## + renovated:green_rating  1    2282.9 1691129 42066
    ## + age:green_rating        1    1100.0 1692311 42071
    ## + size:age:amenities      1     929.3 1692482 42072
    ## - green_rating            1      11.0 1693422 42073
    ## + renovated:amenities     1     588.8 1692823 42074
    ## <none>                                1693411 42075
    ## + green_rating:amenities  1     282.8 1693129 42075
    ## + age:renovated           1      56.0 1693355 42076
    ## - size:amenities          1    4176.4 1697588 42092
    ## - size:renovated          1    7653.9 1701065 42108
    ## - age:amenities           1    8757.9 1702169 42113
    ## - size:age                1   14126.1 1707538 42137
    ## 
    ## Step:  AIC=42063.67
    ## Rent ~ size + age + renovated + green_rating + amenities + size:age + 
    ##     size:renovated + age:amenities + size:amenities + size:green_rating
    ## 
    ##                          Df Sum of Sq     RSS   AIC
    ## + renovated:green_rating  1    2308.3 1688322 42055
    ## + age:green_rating        1     958.3 1689672 42061
    ## + size:age:amenities      1     855.5 1689775 42062
    ## + renovated:amenities     1     606.3 1690024 42063
    ## <none>                                1690630 42064
    ## + age:renovated           1      58.8 1690572 42065
    ## + green_rating:amenities  1       0.4 1690630 42066
    ## - size:green_rating       1    2781.0 1693411 42075
    ## - size:amenities          1    4199.7 1694830 42081
    ## - size:renovated          1    8383.6 1699014 42100
    ## - age:amenities           1    8433.7 1699064 42101
    ## - size:age                1   11223.4 1701854 42113
    ## 
    ## Step:  AIC=42054.98
    ## Rent ~ size + age + renovated + green_rating + amenities + size:age + 
    ##     size:renovated + age:amenities + size:amenities + size:green_rating + 
    ##     renovated:green_rating
    ## 
    ##                               Df Sum of Sq     RSS   AIC
    ## + size:age:amenities           1     844.4 1687478 42053
    ## <none>                                     1688322 42055
    ## + renovated:amenities          1     346.6 1687976 42055
    ## + size:renovated:green_rating  1     340.6 1687981 42055
    ## + age:green_rating             1     121.7 1688200 42056
    ## + green_rating:amenities       1      28.2 1688294 42057
    ## + age:renovated                1       2.9 1688319 42057
    ## - renovated:green_rating       1    2308.3 1690630 42064
    ## - size:green_rating            1    2806.5 1691129 42066
    ## - size:amenities               1    4232.3 1692554 42073
    ## - age:amenities                1    8304.1 1696626 42091
    ## - size:renovated               1    8901.2 1697223 42094
    ## - size:age                     1   10898.4 1699220 42103
    ## 
    ## Step:  AIC=42053.07
    ## Rent ~ size + age + renovated + green_rating + amenities + size:age + 
    ##     size:renovated + age:amenities + size:amenities + size:green_rating + 
    ##     renovated:green_rating + size:age:amenities
    ## 
    ##                               Df Sum of Sq     RSS   AIC
    ## <none>                                     1687478 42053
    ## + size:renovated:green_rating  1     331.4 1687146 42054
    ## + renovated:amenities          1     314.3 1687163 42054
    ## + age:green_rating             1     144.2 1687333 42054
    ## - size:age:amenities           1     844.4 1688322 42055
    ## + green_rating:amenities       1      13.2 1687464 42055
    ## + age:renovated                1       6.2 1687472 42055
    ## - renovated:green_rating       1    2297.2 1689775 42062
    ## - size:green_rating            1    2732.8 1690210 42064
    ## - size:renovated               1    8390.2 1695868 42090

    coef(lm_step)

    ##            (Intercept)                   size                    age 
    ##           2.674987e+01           1.270567e-05          -5.293893e-03 
    ##              renovated           green_rating              amenities 
    ##          -1.783551e+00           1.166726e+00           5.183928e+00 
    ##               size:age         size:renovated          age:amenities 
    ##           9.847932e-08          -7.497642e-06          -8.731404e-02 
    ##         size:amenities      size:green_rating renovated:green_rating 
    ##          -1.209114e-05          -7.436264e-06           4.658266e+00 
    ##     size:age:amenities 
    ##           1.067432e-07

    n = nrow(newbuildings)
    n_train = round(0.8*n)  # round to nearest integer
    n_test = n - n_train
    train_cases = sample.int(n, n_train, replace=FALSE)
    test_cases = setdiff(1:n, train_cases)
    buildings_train = newbuildings[train_cases,]
    buildings_test = newbuildings[test_cases,]

    lm1 = update(lm_all, data=buildings_train)
    lm2 = update(lm_step, data=buildings_train)

    yhat_test1 = predict(lm1, buildings_test)
    yhat_test2 = predict(lm2, buildings_test)

    rmse = function(y, yhat) {
      sqrt( mean( (y - yhat)^2 ) )
    }

    c(rmse(buildings_test$Rent, yhat_test1),
        rmse(buildings_test$Rent, yhat_test2))

    ## [1] 14.16985 14.07527

    newbuildings$lm2_fitted = fitted(lm_step)

    ggplot(data = newbuildings) + geom_point(mapping = aes(x = Rent, y = lm2_fitted, color = green_rating))

![](Exercises-3_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    newbuildings %>% 
      group_by(green_rating) %>% 
      summarise(mean(lm2_fitted))

    ## # A tibble: 2 x 2
    ##   green_rating `mean(lm2_fitted)`
    ##          <int>              <dbl>
    ## 1            0               28.3
    ## 2            1               30.0

The purpose of this model was to predict Rent cost from a number of
building variables. I fit a linear regression model to predict the rent
price using variables that were more likely to influence price. This
included size, age, renovation status, green rating, amenities, and
several interactions between the variables. The variables excluded were
any that involved cluster or geographic information rather than the
actual building information. These numbers were constant for each
cluster and did not differ based on the individual buildings. The
strongest individual variable predictor was age of the building. On
average, green rating increased rent by 1.16 while holding all other
variables constant.

***What causes what?*** 1. You cannot just get data from a few different
cities and run the regression of “Crime” on “Police” to understand how
more cops in the streets affect crime because the relationship found
would only be correlational, not causal. There may be a correlation
between the presence of police and the amount of crime or vice versa,
but that does not mean the amount of crime affects police presence or
the amount of police presence affects crime rate. There could be many
confounding factors that explain the relationship such as the example
used in the podcast, terrorism. No city is the same and many factors can
influence both of these variables. It would be silly to think they could
solely be explained by each other.

1.  The researchers of UPenn were able to isolate this effect in
    Washington D.C. by looking at how crime rates changed based on
    whether or not the city was on high alert for terrorism. This was
    because the amount of police presence in Washington D.C. could be
    explained by its status as the nation’s capital, making it a target
    for terrorists. In table 2, the researchers summarize the results of
    two approaches. The first one they studied the relationship between
    crime rate and the presence of high alert for terrorism. They found
    that crime rate dropped when the alert went up. In the second model,
    they controlled for Metro ridership to see if the high terror alert
    reduced the amount of tourists present, therefore reducing crime due
    to less victims available. They found that crime was still decreased
    on high terror alert days and no decrease in tourism was found.

2.  They controlled for Metro ridership because they were trying to
    capture another possible confounding variable. On high terrorism
    alert days, it is possible that crime drops due to less tourists
    being on the streets and therefore less people to fall victim to
    crime.

3.  The model being described in table 4 is looking at crime on
    high-alert days being concentrated on the national mall, which is
    District 1 in D.C.. The coefficient for high alert in District 1 is
    more negative than the high alert effect in other districts. When
    there is high alert for terrorism, crime is more reduced in District
    1 than in other districts. However, it is still reduced in other
    districts as well.

***Clustering and PCA***

    wine = read.csv("/Users/mproberts99/Desktop/R/wine.csv", header = TRUE)
    set.seed(123)
    # PCA
    library(ggplot2)
    library(tidyverse)

    wine %>% 
      select(-color) %>% 
      select(-quality) %>%
      scale() %>%            # scale to 0 mean and unit variance
      prcomp() ->            
      pca    
    pca

    ## Standard deviations (1, .., p=11):
    ##  [1] 1.7406518 1.5791852 1.2475364 0.9851660 0.8484544 0.7793021 0.7232971
    ##  [8] 0.7081739 0.5805377 0.4771748 0.1811927
    ## 
    ## Rotation (n x k) = (11 x 11):
    ##                              PC1         PC2         PC3         PC4        PC5
    ## fixed.acidity        -0.23879890  0.33635454 -0.43430130  0.16434621 -0.1474804
    ## volatile.acidity     -0.38075750  0.11754972  0.30725942  0.21278489  0.1514560
    ## citric.acid           0.15238844  0.18329940 -0.59056967 -0.26430031 -0.1553487
    ## residual.sugar        0.34591993  0.32991418  0.16468843  0.16744301 -0.3533619
    ## chlorides            -0.29011259  0.31525799  0.01667910 -0.24474386  0.6143911
    ## free.sulfur.dioxide   0.43091401  0.07193260  0.13422395 -0.35727894  0.2235323
    ## total.sulfur.dioxide  0.48741806  0.08726628  0.10746230 -0.20842014  0.1581336
    ## density              -0.04493664  0.58403734  0.17560555  0.07272496 -0.3065613
    ## pH                   -0.21868644 -0.15586900  0.45532412 -0.41455110 -0.4533764
    ## sulphates            -0.29413517  0.19171577 -0.07004248 -0.64053571 -0.1365769
    ## alcohol              -0.10643712 -0.46505769 -0.26110053 -0.10680270 -0.1888920
    ##                              PC6         PC7          PC8        PC9
    ## fixed.acidity        -0.20455371 -0.28307944  0.401235645  0.3440567
    ## volatile.acidity     -0.49214307 -0.38915976 -0.087435088 -0.4969327
    ## citric.acid           0.22763380 -0.38128504 -0.293412336 -0.4026887
    ## residual.sugar       -0.23347775  0.21797554 -0.524872935  0.1080032
    ## chlorides             0.16097639 -0.04606816 -0.471516850  0.2964437
    ## free.sulfur.dioxide  -0.34005140 -0.29936325  0.207807585  0.3666563
    ## total.sulfur.dioxide -0.15127722 -0.13891032  0.128621319 -0.3206955
    ## density               0.01874307 -0.04675897  0.004831136  0.1128800
    ## pH                    0.29657890 -0.41890702 -0.028643277  0.1278367
    ## sulphates            -0.29692579  0.52534311  0.165818022 -0.2077642
    ## alcohol              -0.51837780 -0.10410343 -0.399233887  0.2518903
    ##                              PC10          PC11
    ## fixed.acidity        -0.281267685 -0.3346792663
    ## volatile.acidity      0.152176731 -0.0847718098
    ## citric.acid           0.234463340  0.0011089514
    ## residual.sugar       -0.001372773 -0.4497650778
    ## chlorides            -0.196630217 -0.0434375867
    ## free.sulfur.dioxide   0.480243340  0.0002125351
    ## total.sulfur.dioxide -0.713663486  0.0626848131
    ## density              -0.003908289  0.7151620723
    ## pH                   -0.141310977 -0.2063605036
    ## sulphates             0.045959499 -0.0772024671
    ## alcohol              -0.205053085  0.3357018784

    round(pca$rotation[,1:3],2) 

    ##                        PC1   PC2   PC3
    ## fixed.acidity        -0.24  0.34 -0.43
    ## volatile.acidity     -0.38  0.12  0.31
    ## citric.acid           0.15  0.18 -0.59
    ## residual.sugar        0.35  0.33  0.16
    ## chlorides            -0.29  0.32  0.02
    ## free.sulfur.dioxide   0.43  0.07  0.13
    ## total.sulfur.dioxide  0.49  0.09  0.11
    ## density              -0.04  0.58  0.18
    ## pH                   -0.22 -0.16  0.46
    ## sulphates            -0.29  0.19 -0.07
    ## alcohol              -0.11 -0.47 -0.26

    pca_data <- data.frame(pca$x, color = wine$color, quality = wine$quality)

    ggplot(pca_data, aes(x = PC1, y = PC2, color = color)) + 
      geom_point()

![](Exercises-3_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    percent <- 100*pca$sdev^2 / sum(pca$sdev^2)
    percent

    ##  [1] 27.5442604 22.6711457 14.1486087  8.8232007  6.5443174  5.5210156
    ##  [7]  4.7559888  4.5591845  3.0638550  2.0699615  0.2984618

    lm1 = lm(quality ~ PC1 + PC2 + PC3, data = pca_data)

    plot(quality ~ fitted(lm1), data = pca_data)

![](Exercises-3_files/figure-markdown_strict/unnamed-chunk-2-2.png) The
graph produced using PC1 and PC2 separates the data points based on the
color of wine fairly well. There is some overlap, but most of the white
wines aggregated with the other white wines and the same for the reds.
Other graphs that did not include PC1 could not separate based on color,
indicating PC1 is largely responsible for the diciphering between white
and red wine. This model with PC1 and PC2 was therefore sufficient in
separating the two colors. A linear model including the first 3 PCs was
used to predict quality. The model only explained about 15% of the
variation in quality even though the first 3 PCs accounted for more than
half of the information available on the wines. Therefore, this model
was not able to predict the quality of wine as well as it could with the
color. This indicates the variables used in the PCA were not good
predictors for quality.

    set.seed(234)
    #cluster
    library(ggplot2)
    library(LICORS)  # for kmeans++
    library(foreach)

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

    library(mosaic)

    ## Loading required package: lattice

    ## Loading required package: ggformula

    ## Loading required package: ggstance

    ## 
    ## Attaching package: 'ggstance'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     geom_errorbarh, GeomErrorbarh

    ## 
    ## New to ggformula?  Try the tutorials: 
    ##  learnr::run_tutorial("introduction", package = "ggformula")
    ##  learnr::run_tutorial("refining", package = "ggformula")

    ## Loading required package: mosaicData

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Registered S3 method overwritten by 'mosaic':
    ##   method                           from   
    ##   fortify.SpatialPolygonsDataFrame ggplot2

    ## 
    ## The 'mosaic' package masks several functions from core packages in order to add 
    ## additional features.  The original behavior of these functions should not be affected by this.
    ## 
    ## Note: If you use the Matrix package, be sure to load it BEFORE loading mosaic.

    ## 
    ## Attaching package: 'mosaic'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     mean

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     count, do, tally

    ## The following object is masked from 'package:purrr':
    ## 
    ##     cross

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     stat

    ## The following objects are masked from 'package:stats':
    ## 
    ##     binom.test, cor, cor.test, cov, fivenum, IQR, median, prop.test,
    ##     quantile, sd, t.test, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     max, mean, min, prod, range, sample, sum

    library(dplyr)

    wine %>% 
      group_by(color) %>% 
      tally()

    ## # A tibble: 2 x 2
    ##   color     n
    ##   <fct> <int>
    ## 1 red    1599
    ## 2 white  4898

    X_wine = wine[,(1:11)]
    X_wine = scale(X_wine, center=TRUE, scale=TRUE)

    mu_wine = attr(X_wine,"scaled:center")
    sigma_wine = attr(X_wine,"scaled:scale")

    clust1_wine = kmeans(X_wine, 2, nstart=25)

    wine_cluster <- cbind(wine, clusterNum = clust1_wine$cluster)

    wine_counts <- table(wine_cluster$color, wine_cluster$clusterNum)
    prop.table(wine_counts, 2)

    ##        
    ##                   1           2
    ##   red   0.958612295 0.004944376
    ##   white 0.041387705 0.995055624

    clust2_wine = kmeans(X_wine, 4, nstart=25)

    wine_cluster2 <- cbind(wine, clusterNum = clust2_wine$cluster)

    table(wine_cluster2$color, wine_cluster2$cluster)

    ##        
    ##            1    2    3    4
    ##   red     40    4  638  917
    ##   white 2890 1869   49   90

    mean_highquality <- 
      wine_cluster2 %>%
      filter(quality >= 6)

    mean_lowquality <- 
      wine_cluster2 %>%
      filter(quality < 6)
      
    table(mean_highquality$clusterNum)

    ## 
    ##    1    2    3    4 
    ## 2232 1022  433  426

    table(mean_lowquality$clusterNum)

    ## 
    ##   1   2   3   4 
    ## 698 851 254 581

The clustering method was very effective at sorting between the red and
white wines. I used two clusters. 99.5% of cluster one contained white
wines, while red wins only occupied 0.49%. 95.86% of cluster two
contained red wines, while white wines only occupied 4.14%. Clustering
did not effectively decipher between low quality wines (scores from 1-5)
and high quality wines(scores from 6-10). These wines were scattered
between the 4 clusters, showing no indication that they separated
according to their quality.

The clustering method does make more sense with this data considering
red and white wine have very distinct characteristics due to the
ingredients used for each. Clustering, however would not make sense to
distinguish between high quality and low quality. The criteria for each
is not clear and no particular combination of the 11 ingredients
determines if a wine is high or low quality. The data point could also
be skewed by the bias of the wine snobs.

***Market Segmentation***

    social_marketing = read.csv("/Users/mproberts99/Desktop/R/social_marketing.csv", header = TRUE)
    set.seed(529)
    library(tidyverse)

    tweets <- social_marketing %>% 
      filter(spam == 0, adult == 0) %>% 
      select(-X, -spam, -adult) %>% 
      scale() %>% 
      prcomp()

    plot(tweets)

![](Exercises-3_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    round(tweets$rotation[,1:3],2) 

    ##                   PC1   PC2   PC3
    ## chatter          0.12 -0.20  0.07
    ## current_events   0.10 -0.07  0.05
    ## travel           0.12 -0.05  0.43
    ## photo_sharing    0.18 -0.30 -0.02
    ## uncategorized    0.09 -0.15 -0.04
    ## tv_film          0.10 -0.08  0.08
    ## sports_fandom    0.29  0.32 -0.05
    ## politics         0.13 -0.02  0.49
    ## food             0.30  0.24 -0.10
    ## family           0.24  0.20 -0.05
    ## home_and_garden  0.12 -0.05  0.03
    ## music            0.12 -0.15 -0.02
    ## news             0.12  0.04  0.34
    ## online_gaming    0.07 -0.08  0.04
    ## shopping         0.13 -0.21  0.04
    ## health_nutrition 0.12 -0.14 -0.21
    ## college_uni      0.09 -0.12  0.07
    ## sports_playing   0.13 -0.11  0.04
    ## cooking          0.19 -0.31 -0.20
    ## eco              0.15 -0.09 -0.03
    ## computers        0.14 -0.04  0.37
    ## business         0.14 -0.11  0.11
    ## outdoors         0.14 -0.11 -0.13
    ## crafts           0.19  0.02  0.00
    ## automotive       0.13  0.03  0.19
    ## art              0.10 -0.06  0.04
    ## religion         0.30  0.31 -0.09
    ## beauty           0.20 -0.21 -0.16
    ## parenting        0.30  0.30 -0.09
    ## dating           0.11 -0.07  0.03
    ## school           0.28  0.19 -0.08
    ## personal_fitness 0.13 -0.14 -0.21
    ## fashion          0.18 -0.28 -0.14
    ## small_business   0.12 -0.10  0.10

    social_marketing = merge(social_marketing, tweets$x[,1:9], by="row.names")

    rotation_data <- data.frame(
      tweets$rotation, 
      variable = row.names(tweets$rotation)
    )

    arrow_style <- arrow(
      length = unit(0.01, "inches"),
      type = "closed"
    )

    ggplot(rotation_data) + 
      geom_segment(aes(xend = PC1, yend = PC2), x = 0, y = 0, arrow = arrow_style) + 
      geom_text(aes(x = PC1, y = PC2, label = variable), hjust = 0, size = 1.5, color = "red") + 
      xlim(0, .5) + 
      ylim(-.5, .5)

![](Exercises-3_files/figure-markdown_strict/unnamed-chunk-4-2.png)

    ggplot(rotation_data) + 
      geom_segment(aes(xend = PC3, yend = PC4), x = 0, y = 0, arrow = arrow_style) + 
      geom_text(aes(x = PC3, y = PC4, label = variable), hjust = 0, size = 1.5, color = "red") + 
      xlim(0, .5) + 
      ylim(-.5, .5)

    ## Warning: Removed 16 rows containing missing values (geom_segment).

    ## Warning: Removed 16 rows containing missing values (geom_text).

![](Exercises-3_files/figure-markdown_strict/unnamed-chunk-4-3.png) The
data from the PCA analysis revealed some interesting trends. PC2 is
largely composed of sports fandom, food, family, religion, and
parenting. These categories all points towards family life, particularly
parents. This indicates there is likely a group of young parents
following the brand. PC1 explained variance in most variables,
particularly fashion, photo sharing, cooking, and beauty. These
categories generally appeal to young women or mothers, revealing another
follower group of the brand. PC3 revealed a follower base of mostly
colleged-aged boys, strongly correlated with the categories of sports
playing, online gaming, and university. PC4 strongly contained the
categories news, travel, computers, and politics. These categories could
generally describe a young, single, professional individual. Automotive
was also strongly linked with PC4. Stereotypes should be considered when
building their advertisement campaign, but these categories are
generally linked to young adult men.

When you save the notebook, an HTML file containing the code and output
will be saved alongside it (click the *Preview* button or press
*Cmd+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.
