diff_ineq <- function(data, ineq_group, outcome, adjustment=NULL, full_results = FALSE) {
  library(magrittr)
  library(dplyr)
  
  # means <- aggregate(reformulate(termlabels = ineq_group, response = outcome), 
  #                    data = data, FUN = mean)
  # 
  # cont <- lapply(means$y, function(x) {
  #   means$y - x
  # }) %>% do.call(rbind,.)
  # 
  # cont_names <- lapply(means$ineq_group, function(x) {
  #   paste0(means$ineq_group,"_",x)
  # }) %>% do.call(rbind,.)
  # 
  # cont <- cont[upper.tri(cont)]
  # names(cont) <- cont_names[upper.tri(cont_names)]
  
  
  data$ineq_group <- data[,ineq_group]
  
  # Number of groups
  n_groups <- data[,"ineq_group"] %>% unique %>% length

  # Run model
  
  bin_outcome <- all(data[,outcome] %in% c(0,1))
  
  if (bin_outcome) {
    model <- glm(reformulate(termlabels = c("ineq_group",adjustment), response = outcome, intercept = FALSE), 
                 data = data, family = "binomial")
  } else {
    model <- glm(reformulate(termlabels = c("ineq_group",adjustment), response = outcome, intercept = FALSE), 
                 data = data, family = "gaussian")
  }
  
  #means <- aggregate(reformulate(termlabels = ineq_group, response = outcome), data = data, FUN = mean)
  
  # Get contrasts from model output
  contrasts <- list()
  cont_name <- NULL
  group_names <- names(model$coefficients)[grepl("ineq_group",names(model$coefficients))]
  group_names <- gsub("ineq_group","",group_names)

  

  
  for (c1 in 1:(n_groups-1)) {
    for(c2 in (c1+1):n_groups) {
      cont <- rep(0,n_groups)
      cont[c(c1,c2)] <- c(1,-1)
      contrasts <- c(contrasts, list(cont))

      cont_name <- c(cont_name,paste0(group_names[c1], " vs ", group_names[c2]))
    }
  }
  
  contrasts <- do.call(rbind,contrasts) %>% as.data.frame 
  
  if (!is.null(adjustment)) {
    z <- matrix(data = rep(0,nrow(contrasts)*length(adjustment)), nrow = nrow(contrasts))
    contrasts <- cbind(contrasts,z)
  }

  
  group_contrasts <- apply(contrasts,1,FUN=function(x) {
    lincom_JAL(model, contrast = x)
  }) %>% t %>% as.data.frame
  row.names(group_contrasts) <- cont_name
  
  # Get values per group from model output
  group_values <- model %>%
    summary %$%
    coefficients %>%
    as.data.frame %>%
    dplyr::select(est = Estimate, se = "Std. Error") %>%
    mutate(ci_low = est - qnorm(0.975)*se,
           ci_high = est + qnorm(0.975)*se)

  
  group_values <- group_values[grepl(pattern = "ineq_group", x = rownames(group_values)),]

  if (full_results) {
    return(list(values = group_values,
               contrasts = group_contrasts,
               model = ifelse(bin_outcome,"log binomial","linear")))
  } else {
    output <- c(group_values$est,group_contrasts$est)

    names(output) <- c(row.names(group_values),row.names(group_contrasts))

    names(output) <- c(group_names,row.names(group_contrasts))

    return(output)
  }


}


lincom_JAL = function(model, contrast, confid=0.95) {
  
  modfamily <- family(model)[[1]]
  modlink <- family(model)[[2]]
  modcoef <- model$coeff
  vcm <- vcov(model)
  n <- length(modcoef)
  covar <- var <- matrix(0, n, n)
  
  if (n != length(contrast)) stop ("Contrast vector length not equal to number of model coefficients!")
  
  est <- sum(modcoef * contrast)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i==j) 
        var[i,j] = contrast[i]^2 * vcm[i,i] 
      else 
        covar[i,j] = contrast[i] * contrast[j] * vcm[i,j]
    }
  }
  
  se = sqrt( sum(var,na.rm=T) + sum(covar,na.rm=T) )
  
  Z = abs( qnorm( (1 - confid)/2) )
  CI = est + c(-Z,Z)*se
  names(CI) <- c("low_ci","high_ci")
  
  return(c(est=est,se=se,CI))
}
