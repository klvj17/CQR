censored_qr <- function(Yvar, Xvar, data, taus, cut_value, type, maxit){

  initial_par <- quant_reg(Yvar = Yvar, Xvar = Xvar, data = data, taus = taus)

  result_df <- matrix(ncol = length(Xvar) + 1, nrow = maxit) %>% as.data.frame()
  result_df[1,] <- initial_par

  for(i in 2:maxit){
    result_df[i,] <- quant_reg_step(Yvar = Yvar, Xvar = Xvar,
                                    data = data, taus = taus, cut_value = cut_value,
                                    parameter = unlist(result_df[(i-1),]))
  }

  colnames(result_df) <- c("constant", Xvar)

  return(result_df[maxit,])

}


quant_reg_step <- function(Yvar, Xvar, data, taus, cut_value, type, parameter){

  pred <- as.matrix(cbind(1, data[, Xvar])) %*% parameter

  cut_flag <- ifelse(pred < cut_value, F, T)

  quant_reg(Yvar = Yvar, Xvar = Xvar, data = data[cut_flag,], taus = taus)

}



censored_quant_reg_bst_step <- function(x, Yvar, Xvar, data, taus, cut_value, type, maxit){


  sample_flag <- sample(NROW(data), replace = T)

  res <- censored_qr(Yvar = Yvar,
              Xvar = Xvar,
              data = data[sample_flag,],
              taus = taus,
              cut_value = cut_value,
              type = type,
              maxit = maxit)

  cbind(x, res)

}

censored_quant_reg_bst <- function(Yvar, Xvar, taus, cut_value, type, maxit, data, R = 300){


  result <- lapply(1:R,
                   censored_quant_reg_bst_step,
                   Yvar = Yvar,
                   Xvar = Xvar,
                   data = data,
                   taus = taus,
                   cut_value = cut_value,
                   type = type,
                   maxit = maxit) %>%
    bind_rows()

  out <- apply(result[,-1], 2, sd)
  names(out) <- c("constant", Xvar)

  return(out)
}



cqr_lm <- function(Yvar,Xvar, data, taus, cut_value, maxit, R){

  censored_qreg <- censored_qr(Yvar = Yvar, Xvar = Xvar, data = data, taus = taus, cut_value = cut_value, maxit = maxit)

  censored_qr_se <- censored_quant_reg_bst(Yvar = Yvar, Xvar = Xvar, data = data, taus = taus, cut_value = cut_value, maxit = maxit)

  out <- rbind(censored_qreg, censored_qr_se) %>% t
  colnames(out) <- c("coef", "se")

  out %>% cbind(tau = taus, variable = c("constant", Xvar)) %>% as.data.frame()
}
