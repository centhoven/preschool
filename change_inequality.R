
ineq_fun <- function(dat, inds, outcome, mediator, ineq_group, covars,
                     interaction = TRUE, set_mediator_to, standardize = NULL,
                     ineq_function = "diff_ineq", 
                     params = list(ineq_group = ineq_group, 
                                   outcome = outcome)) {
  
  # Inequality pre-intervention
  
  data <- dat[inds,]
  
  params <- c(data = list(data), params)
  
  pre_ineq <- do.call(match.fun(ineq_function),params)
  names(pre_ineq) <- paste0("pre_", names(pre_ineq))
  
  
  # Intervention ---------------------------------------------------------------
  
  
  # Setting mediator to right value
  
  # Estimate effect of the mediator on the outcome
  bin_outcome <- all(data[,outcome] %in% c(0,1))
  
  if (bin_outcome) {
    model <- glm(reformulate(termlabels = 
                               c(ifelse(interaction,
                                        paste0(ineq_group,"*",mediator),
                                        c(ineq_group,mediator)),
                                 covars),
                             response = outcome), 
                 data = data, 
                 family = binomial)
  } else {
    model <- glm(reformulate(termlabels = 
                               c(ifelse(interaction,
                                        paste0(ineq_group,"*",mediator),
                                        c(ineq_group,mediator)),
                                 covars),
                             response = outcome), 
                 data = data, 
                 family = "gaussian")
  }
  
  int_split <- lapply(set_mediator_to, function(x) {
    strsplit(x, "(?=([<>=]))", perl=TRUE) %>% unlist
  })
  
  data_modified <- data
  
  for (i in 1:length(int_split)) {
    x <- int_split[[i]]
    x <- c(x[1],paste0(x[x %in% c("<",">","=")], collapse = ""),x[length(x)])
    data_modified[!do.call(x[2],
                           list(data_modified[,x[1]],
                                as.numeric(x[3]))),x[1]] <- as.numeric(x[3])
  }
  
  vars_intervened <- unique(sapply(int_split, function(x) x[1]))
  
  # for (var in vars_intervened) {
  #   varname <- paste0(var,"_mod")
  #   data[[varname]] <- (data[,var]-data_modified[,var])
  # }
  
  
  # Correct the outcome to what it would be if the mediator was set to what it's set to
  #data$y_post <- data[,outcome] + rowSums(model$coefficients*data[,paste0(vars_intervened,"_mod")])
  data$y_post <- predict(model, newdata = data_modified, type="response")
  
  params_post <- params
  params_post$data <- data
  params_post$outcome <- "y_post"
  post_ineq <- do.call(match.fun(ineq_function),params_post)
  names(post_ineq) <- paste0("post_", names(post_ineq))
  
  dif_ineq <- post_ineq - pre_ineq
  names(dif_ineq) <- paste0("diff_", names(dif_ineq))
  
  ratio_ineq <- post_ineq/pre_ineq
  names(ratio_ineq) <- paste0("ratio_", names(ratio_ineq))
  
  out <- c(pre_ineq, post_ineq, dif_ineq, ratio_ineq)
  
  return(out)
  
}





