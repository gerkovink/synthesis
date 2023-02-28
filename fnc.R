library(ggplot2)


#' Create a plot based on synthetic datasets
#'
#' @param syn.obj a list of synthetic datasets
#' @param lm.formula a linear model formula that user specify e.g., y ~ x
#'
#'
#' @return a ggplot object

synplot1 <- function(syn.obj, lm.formula){
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




#' Create a plot of fitted values vs. observed values
#'
#' @param model.list a list of fitted model objects
#' @param smoother a smoothing method to use (default = "lm")
#'
#'
#' @return a ggplot object

synplot2 <- function(model.list, smoother = "lm"){
  # get the model formula
  form <- formula(model.list[[1]])
  model.list %>% 
    purrr::map(
      ~.x[c("fitted.values", "model")] %>% 
        dplyr::bind_cols() %>% 
        # select fitted.values & DV which are the 1st and 2nd vars
        dplyr::select(1:2)
    ) %>% 
    dplyr::bind_rows(.id = "id") %>% 
    ggplot(aes(x = fitted.values, 
               # grab the dependent variable and unquote it
               y = !!rlang::sym(all.vars(form)[1]), 
               color= id)) +
    geom_point(size = 1, alpha = 0.2) +
    geom_line(stat = "smooth", 
              method = smoother, 
              alpha = 0.5, 
              position = position_dodge(width = 1)) +
    theme_classic() +
    labs(title = paste("Line for", deparse(form)), color="")
}
