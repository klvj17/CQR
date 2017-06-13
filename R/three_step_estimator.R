two_three_step <- function(first_step_model, first_step_fitted = NA, taus, q1, q2, YV, XV, cqr_data, method = "fn"){

  if(is.na(first_step_fitted[1])){
    #get the propensity score
    step_one <- fitted(first_logit)
  }else{
    #get the propensity score
    step_one <- first_step_fitted
  }

  #get the q1th quantile point in sample where P(x) > tau
  cp_min <- cp_rate_list(taus = taus,
                         step_one = step_one,
                         q1 = q1)

  output_list <- list()
  robust_stats <- matrix(ncol = 3, nrow = (length(taus)))
  colnames(robust_stats) <- c("J1_rate_total", "J1_rate_J0", "J1_notin_J0")

  for(i in 1:length(taus)){

    #2nd step
    J_0_flag <- ifelse(step_one > cp_min$cp[i], T, F)
    step_two_model <- rq(data = cqr_data[J_0_flag, ],
                         formula = paste(YV, "~ ."),
                         tau = taus[i],
                         method = method)
    pred_value <- predict(step_two_model, cqr_data)

    #3rd step
    dp_min <- dp_rate(pred_value, cut_value = cut_value, q2 = q2)
    J_1_flag <- ifelse(pred_value > dp_min, T, F)
    tf_tab <- table(data.frame(J_0_flag, J_1_flag))

    #assess robustness stats
    J1_rate_total <- sum(J_1_flag)/NROW(cqr_data)
    J1_rate_J0 <- tf_tab[2,2]/sum(tf_tab[2,])
    J1_notin_J0 <- tf_tab[1,2]

    robust_stats[i,] <- c(J1_rate_total, J1_rate_J0, J1_notin_J0)

    #obtain three step estimator
    model_result <- rq(data = cqr_data[J_1_flag,],
                       formula = paste(YV, "~ ."),
                       tau = taus[i],
                       method = method
                       )

    #output
    output_list[[i]] <- summary(model_result)

  }

  return(list(output_list, robust_stats))
}
