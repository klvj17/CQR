quant_reg <- function(Yvar, Xvar, taus, data){

  if(NROW(data) > 1000){
    method <- "fn"
  }else{
    method = "br"
  }

  rq_res <- rq(data = data, paste(Yvar, "~", paste(Xvar, collapse = " + "), " "), tau = taus, method = method)
  out <- rq_res$coefficients
  names(out) <- c("constant", Xvar)

  return(out)
}


obj_fun <- function(par, Yvar, Xvar, tau){

  pred <- (Xvar %*% par)

  u <- (Yvar - pred)

  pn_f <- ifelse(u > 0, 1, 0)

  cqf <- pn_f * tau * abs(u) + (1 - pn_f) * (1 - tau) * abs(u)

  sum(cqf)/length(Yvar)

}


quant_reg_bst_step <- function(x, Yvar, Xvar, taus, data){


  sample_flag <- sample(NROW(data), replace = T)

  res <- quant_reg(Yvar = Yvar, Xvar = Xvar, taus = taus, data = data[sample_flag,])

  c(x, res)
}

quant_reg_bst <- function(Yvar, Xvar, taus, data, R = 300){


  result <- sapply(1:R,
                   quant_reg_bst_step,
                   Yvar = Yvar,
                   Xvar = Xvar,
                   data = data,
                   taus = taus) %>%
    t() %>% as.data.frame()

  out <- apply(result[,-1], 2, sd)

  names(out) <- c("constant", Xvar)

  return(out)
}



qr_lm <- function(Yvar, Xvar, data, taus, R){

  qureg <- quant_reg(Yvar = Yvar, Xvar = Xvar, data = data, taus = taus)
  qureg_se <- quant_reg_bst(Yvar = Yvar, Xvar = Xvar, data = data, taus = taus, R = R)

  out <- rbind(qureg, qureg_se) %>% t
  colnames(out) <- c("coef", "se")

  out %>% cbind(tau = taus, variable = c("constant", Xvar)) %>% as.data.frame()
}




