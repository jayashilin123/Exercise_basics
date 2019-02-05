Risk Stratification
================

Load the cleaned data

``` r
data <- read.csv("data/data_cleaned.csv", header=TRUE, 
                  na.strings = c("NA","na",""," ","?"), stringsAsFactors = FALSE)
```

save the ID of patients and Remove the ID before analysis

``` r
ID <- data$ID
data <- data[2:42]
```

Format the readmitted variable

``` r
data$readmitted[data$readmitted == "YES"] <- "1"
data$readmitted[data$readmitted == "NO"] <- "0"
table(data$readmitted)
```

    ## 
    ##     0     1 
    ## 53316 46176

``` r
data$readmitted <- as.factor(data$readmitted)
```

Sampling down the data

``` r
set.seed(123)
data_svm <- data[sample(nrow(data), nrow(data)*0.25, replace = F),]

# split data into train and test in 0.75:0.25 ratio, respectively
set.seed(123)

indices <- sample(2, nrow(data_svm), replace = T, prob = c(0.75, 0.25))
train <- data_svm[indices == 1, ]
test <- data_svm[indices == 2, ]
```

Try SVM

``` r
svm.linear <- ksvm(readmitted~., data=train, scale =FALSE, kernel="vanilladot")
```

    ##  Setting default kernel parameters

``` r
svm.predict <- predict(svm.linear, test[,-1])
confusionMatrix(svm.predict, test$readmitted)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 2898 1852
    ##          1  489  984
    ##                                           
    ##                Accuracy : 0.6238          
    ##                  95% CI : (0.6116, 0.6359)
    ##     No Information Rate : 0.5443          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.2108          
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.8556          
    ##             Specificity : 0.3470          
    ##          Pos Pred Value : 0.6101          
    ##          Neg Pred Value : 0.6680          
    ##              Prevalence : 0.5443          
    ##          Detection Rate : 0.4657          
    ##    Detection Prevalence : 0.7633          
    ##       Balanced Accuracy : 0.6013          
    ##                                           
    ##        'Positive' Class : 0               
    ##
