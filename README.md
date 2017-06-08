# CQR
This is the R implementation of Three-Step Censored Quantile Regression that is explained in Chernozhukov and Han(2002).


# CQR package

## about this package

CQR is developed to analyze the Quantile Treatment Effect with censored data.
This package is based on the Three-step Censored Quantile Regression which is introduced in [Chernozhukov and Han(2002)](http://www.mit.edu/~vchern/papers/Chernozhukov%20and%20Hong%20(JASA%202002)%20Three%20Step%20Censored%20Quantile%20Regression.pdf).


The original idea is introduced in Chernozhukov and Han(2002).
This package is still in developing, though you are able to reproduce the table 1 and Figure 1 in the above paper some degree.

## install

You are able to install with install_github function in devtools package.
```
devtools::install_github("yasui-salmon/CQR")
```

## Extramarital_affair dataset

The original dataset is possible to download from [here](https://fairmodel.econ.yale.edu/rayfair/pdf/2011b.htm)

RB data is the dataset what used in Chernozhukov and Han(2002). 

## Simulation Example
You are able to find the example code and result in reproducing [here](https://github.com/yasui-salmon/CQR/blob/master/EXAMPLE/Simulation_example.md), please check this before you go for your analysis.


## Reference
Buchinsky, Moshe, and Jinyong Hahn. "An alternative estimator for the censored quantile regression model." Econometrica (1998): 653-671.

Chernozhukov, Victor, and Han Hong. "Three-step censored quantile regression and extramarital affairs." Journal of the American Statistical Association 97.459 (2002): 872-882.
