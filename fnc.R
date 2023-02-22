library(ggplot2)
library(rlang)

#' Create a plot based on synthetic datasets
#'
#' @param syn.obj a list of synthetic datasets
#' @param lm.formula a linear model formula that user specify e.g., y ~ x
#'
#'
#' @return a ggplot object

synplot <- function(syn.obj, lm.formula){
  vars <- all.vars(lm.formula)
  # row-bind all dfs
  dplyr::bind_rows(!!!syn.obj, .id="id") %>% 
  # suppose lm.formula contains one predictor x
    ggplot(aes_string(x = vars[2], y = vars[1], color= "id")) +
    geom_point(size=1, alpha = 0.2) +
    geom_line(stat = "smooth", method = lm, alpha = 0.5, position = position_dodge(width = 1)) +
    theme_classic() +
    labs(title = paste("Linear Regression", deparse(lm.formula)), color="")
}


pred <- make.predictorMatrix(boys)
meth <- make.method(boys)
pred[c("hgt", "wgt"), "bmi"] <- 0
meth["bmi"] <- "~ I(wgt/(hgt/100)^2)"
imp <- mice(boys, 
            m=1, 
            maxit = 10,
            pred = pred, 
            meth = meth, 
            print = FALSE)
truth <- complete(imp)

syn <- mice(truth, 
            meth = "cart", 
            m = 5, 
            maxit = 1, 
            print = FALSE,
            where = matrix(TRUE, nrow(truth), ncol(truth)))

model <- complete(syn, "all") %>% map(~.x %$% lm(age ~ bmi + hc))

synplot <- function(model.list, smoother = "lm"){
  fit <- model.list[[1]]
  form <- formula(fit)
  vars <- form %>% all.vars()
  # row-bind all dfs
  mice::complete(mids, "long") %>% 
    # suppose lm.formula contains one predictor x
    ggplot(aes(x = fit$fitted.values, 
               y = vars[1], 
               color= .imp)) +
    geom_point(size = 1, alpha = 0.2) +
    geom_line(stat = "smooth", 
              method = smoother, 
              alpha = 0.5, 
              position = position_dodge(width = 1)) +
    theme_classic() +
    labs(title = paste("Line for", deparse(form)), color="")
}

A <- model %>% map(~.x[c("fitted.values", "model")] %>% 
                do.call("cbind", .) %>% 
                select(c(1,2))) %>% 
  do.call("rbind", .) %>% 
  mutate(imp = rep(1:length(model), rep(dim(.)[[1]]/length(model), length(model))))

