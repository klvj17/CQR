\name{two_three_step}
\alias{two_three_step}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  2nd and 3rd step of the three-step estimator.
}
\description{
This function executes the 2nd and 3rd step of the three-step estimator.


}
\usage{
two_three_step(first_step_model, taus, q1, q2, YV, XV, cqr_data, method = "fn")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{first_step_model}{
    The model object of the first step estimator. Currently, glm is assumed to be insert here.
}
  \item{taus}{
    quantiles that you want to estimate.
}
  \item{q1}{
    This is the percentile that you want to discard from the sample where the fitted value of step1 above tau. The original paper suggested to use 0.1 here based on their simulation result.
}
  \item{q2}{
    The percentile that you want to discard from the sample which fitted value of 2nd step quantile regression is above censore point. The original paper did not give any information about this value, though their implementation of Censored QUantile Instumental Variable in Stata used 0.03 for their modified three-step estimator as default value.
}
  \item{YV}{
  Dependent vairable's name. Must be character.
}
  \item{XV}{
  Inependent vairable's name. Must be character.
}
  \item{cqr_data}{
  Dataset what you want to run a model in data.frame.
}
  \item{method}{
  Specify the optimization method for the quantile regression. This value will be passed to rq function of quant_reg package directly.
}
}
\details{
  This is the main function of this package. First step is somthing you need to modify by yourself according to your dataset, so second and third steps are implemented as different funtion.

}
\value{
  This function returns a list. The first component is the list of result of 3rd step, which is called three-step estimator. The second companent is robustness stats that is recommended to check.
%% ...
}
\references{
Chernozhukov, Victor, and Han Hong. "Three-step censored quantile regression and extramarital affairs." Journal of the American Statistical Association 97.459 (2002): 872-882.
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

repetitions <- 1000
N <- 1000
N_est <- 400
cut_value <- -0.75

taus <- 0.5
q1 <- 0.1
q2 <- 0.03

param_true <- c(1, 1, 0.5, -1, -0.5, 0.25)

MS_df <- matrix(nrow = repetitions, ncol = 6)
robust_stat <- matrix(nrow = repetitions, ncol = 3)

#set up X_i i in 1:5
data <- data.frame(
  X1 = rnorm(n = N, 0, 1),
  X2 = rnorm(n = N, 0, 1),
  X3 = rnorm(n = N, 0, 1),
  X4 = rnorm(n = N, 0, 1),
  X5 = rnorm(n = N, 0, 1))

#make |X_i| < 2
data <- data[ifelse(apply(data,1,max) > 2, F, T), ]
data <- data[sample(NROW(data), N_est, replace = F), ]

#make u ~ N(0, 25)
u <- rnorm(n = N_est, 0, 5)

#make eps
hetero_factor <- apply(data + data^2, 1, sum)
eps <- u * (1 + 0.5 * hetero_factor)

#make y
y <- cbind(1,as.matrix(data)) \%*\% param_true + eps

#make y as censored data with cut_value which is -0.75 in the original papaer
y <- ifelse(y < cut_value, cut_value, y)

#finally obtained dataset of (1, X_i, y)
data <- data \%>\% cbind(y)

#getting into three step estimation
#1st step
YV <- "y" #dependent variable
XV <- paste("X", 1:5, sep = "") #independent variables

#rename dataset this part might be troublesome in large dataset
cqr_data <- data

#prepare X_i^2 as the original paper(this part should be modified for your dataset)
#original paper changed this part in the extramarital affairs example
sq_extra <- cqr_data \%>\%
  select_(paste("-",YV, sep = "")) \%>\%
  mutate_at(vars(-matches(YV)), pt, n = 2)

colnames(sq_extra) <- paste("sq_", colnames(sq_extra), sep = "")

#bind into original dataset then make binary variable which indicates the y is censored or not.
lr_extra <- cbind(cqr_data, sq_extra) \%>\%
  mutate(binom_var = ifelse(y > cut_value, 1, 0))

#run the logistic regression of first step
first_logit <- glm(lr_extra, formula = paste("binom_var ~ . -", YV), family = binomial())

#run the second step and third step
#you will get summary of the third step model and robustnes stats to check whether J0 is subset of J1.
three_step_result <- two_three_step(first_step_model = first_logit,
                                    taus = taus,
                                    q1 = q1,
                                    q2 = q2,
                                    YV = YV,
                                    XV = XV,
                                    cqr_data = cqr_data)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
