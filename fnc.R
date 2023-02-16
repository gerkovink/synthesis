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


