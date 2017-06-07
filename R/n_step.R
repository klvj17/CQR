# flag making for J_0
J_0_flagger <- function(step_one, cp_min){

  lapply(cp_min$cp, function(x){ifelse(step_one > x, T, F)})

}


# calculate q against certain delta with each tau
dp_rate_lsit <- function(cut_value, q2, result_before){

  out <- lapply(result_before, dp_rate, cut_value = cut_value, q2 = q2)  %>% sapply("[[", 1)
  return(out)

}

# list of delta and percentile(q2)
dp_rate <- function(q2, pred_value, cut_value){

  nodp_flag <- ifelse(pred_value > (cut_value), T, F)

  delta <- pred_value[nodp_flag] %>% quantile(q2)

  return(delta)
}

# flag making for J_n
J_n_flagger <- function(result_before, dp_min, cut_value){

  Map(function(dp, result_before){ ifelse(result_before > dp + cut_value, T, F) }, dp_min, result_before)

}



# step two and three and more
step_fun <- function(Yvar, Xvar, cut_flag, data, taus, cut_value, dp, R, opt_q2){

  data_cut <- data[cut_flag,]

  step <- qr_step_tt(Yvar = Yvar, Xvar = Xvar, data_cut = data_cut, data = data, taus = tau, R = R)

  fitted <- step[[3]]

  dp_min <- lapply(seq(0, taus, 0.001), dp_rate, pred_value = fitted, cut_value = cut_value) %>%
    bind_rows() %>%
    mutate(qd = abs(q - opt_q2)) %>%
    filter(qd == min(qd)) %>%
    filter(dp == min(dp))


  flag <- ifelse( unlist(fitted) > (cut_value + dp_min$dp), T, F)

  list(step[[1]], step[[2]], flag, fitted)
}

qr_step_tt <- function(Yvar, Xvar, data_cut, data, taus, R){

  res <- quant_reg(Yvar = Yvar, Xvar = Xvar, data = data_cut, taus = taus)
  names(res) <- c("constant", Xvar)

  res_se <- quant_reg_bst(Yvar = Yvar, Xvar = Xvar, data = data_cut, taus = taus, R = R)
  names(res_se) <- paste(c("constant", Xvar), "_se", sep = "")

  fit <- as.matrix(cbind(1, data[, Xvar])) %*% res

  list(res, res_se, fit[,1])
}




## summary for simulation result
stats_result <- function(df, param_true){
  bias_df <- df - param_true

  intercept_rmse <- mean((df[,1] - param_true[1])^2, na.rm = T) %>% sqrt()
  slope_rmse <- mean((df[,-1] - param_true[-1])^2, na.rm = T) %>% sqrt()

  intercep_mean_bias <- mean((df[,1] - param_true[1]), na.rm = T)
  slope_mean_bias <- mean((df[,-1] - param_true[-1]), na.rm = T)

  intercep_median_bias <- median((df[,1] - param_true[1]), na.rm = T)
  slope_median_bias <- median((df[,-1] - param_true[-1]), na.rm = T)

  intercep_mae_bias <- mean(abs(df[,1] - param_true[1]), na.rm = T)
  slope_mae_bias <- mean(abs(df[,-1] - param_true[-1]), na.rm = T)

  out_df <- matrix(ncol = 4, nrow = 2)
  out_df[1,] <- c(intercept_rmse, intercep_mean_bias, intercep_mae_bias, intercep_median_bias)
  out_df[2,] <- c(slope_rmse, slope_mean_bias, slope_mae_bias, slope_median_bias)
  colnames(out_df) <- c("rmse", "mean_bias", "mae_bias", "median_bias")
  row.names(out_df) <- c("intercept", "slope")

  return(out_df)
}
