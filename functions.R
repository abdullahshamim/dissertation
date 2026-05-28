library(tidyverse)

mymerge <- function(dfA,dfB){
  
  ABprime <- inner_join(dfA,dfB) %>% 
    mutate(merge=3)
  Aprime <- anti_join(dfA,dfB) %>% 
    mutate(merge=1)
  Bprime <- anti_join(dfB,dfA) %>% 
    mutate(merge=2)
  
  dfAB <- bind_rows(ABprime,Aprime,Bprime)
  
  return(dfAB)
}


theme_nice <- function() {
  theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 10),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey80", color = NA),
      legend.title = element_text(face = "bold")
    )
}


`%notin%` <- Negate(`%in%`)


ivfstat <- function(ivreg_list, vcov_list=NULL) {
  
  fstats <- numeric(length(ivreg_list))
  
  for (i in 1:length(ivreg_list)) {
    
    if (is.null(vcov_list)) {
      fstat <- summary(ivreg_list[[i]])$diagnostics[1,3]
    } else {
      fstat <- summary(ivreg_list[[i]], vcov_list[[i]])$diagnostics[1,3]
    }
    
    fstats[i] <- fstat
    
  }
  
  return(round(fstats,2))
  
}

weighted.sum <- function(x, w, na.rm = FALSE) { 
  sum(x * w, na.rm = na.rm) 
}

qcut <- function(x, qbreaks, group_obs = NULL, as_percent = FALSE, 
                 percent_digits = 2, digits = 3) {
  
  # Simple formatting function
  simple_fmt <- function(x) format(x, trim = TRUE, scientific = FALSE)
  
  # Generate factor with quantile-based cuts
  y <- Hmisc::cut2(
    x,
    cuts = quantile(x, probs = qbreaks, na.rm = TRUE),
    m = group_obs,
    formatfun = simple_fmt
  )
  
  # Extract numeric bounds from factor levels
  bounds <- stringr::str_match(levels(y), "\\[?\\s*([0-9\\.eE\\-]+),\\s*([0-9\\.eE\\-]+)\\s*\\]?\\)?")
  
  # Convert bounds to numeric
  lower <- as.numeric(bounds[, 2])
  upper <- as.numeric(bounds[, 3])
  
  # Format levels
  if (as_percent) {
    # Convert to percent and format with specified digits
    formatted_levels <- paste0(
      sprintf(paste0("%.", percent_digits, "f%%"), lower * 100),
      " – ",
      sprintf(paste0("%.", percent_digits, "f%%"), upper * 100)
    )
  } else {
    # Format as numbers with `digits` decimal places
    formatted_levels <- paste0(
      formatC(lower, format = "f", digits = digits, big.mark = ","),
      " – ",
      formatC(upper, format = "f", digits = digits, big.mark = ",")
    )
  }
  
  # Assign new labels
  levels(y) <- formatted_levels
  
  return(y)
}


# qcut <- function(x, qbreaks, group_obs = NULL, as_percent = FALSE, percent_digits = 2) {
#   
#   # Simple formatting function
#   simple_fmt <- function(x) format(x, trim = TRUE, scientific = FALSE)
#   
#   # Generate factor with quantile-based cuts
#   y <- Hmisc::cut2(
#     x,
#     cuts = quantile(x, probs = qbreaks, na.rm = TRUE),
#     m = group_obs,
#     formatfun = simple_fmt
#   )
#   
#   # Extract numeric bounds from factor levels
#   bounds <- stringr::str_match(levels(y), "\\[?\\s*([0-9\\.eE\\-]+),\\s*([0-9\\.eE\\-]+)\\s*\\]?\\)?")
#   
#   # Convert bounds to numeric
#   lower <- as.numeric(bounds[, 2])
#   upper <- as.numeric(bounds[, 3])
#   
#   # Format levels
#   if (as_percent) {
#     # Convert to percent and format with specified digits
#     formatted_levels <- paste0(
#       sprintf(paste0("%.", percent_digits, "f%%"), lower * 100),
#       " – ",
#       sprintf(paste0("%.", percent_digits, "f%%"), upper * 100)
#     )
#   } else {
#     # Format with commas
#     formatted_levels <- paste0(
#       format(lower, big.mark = ",", trim = TRUE),
#       " – ",
#       format(upper, big.mark = ",", trim = TRUE)
#     )
#   }
#   
#   # Assign new labels
#   levels(y) <- formatted_levels
#   
#   return(y)
# }

# qcut <- function(x, qbreaks, group_obs = NULL) {
#   
#   # Use a simple formatting function that avoids padding
#   simple_fmt <- function(x) format(x, trim = TRUE, scientific = FALSE)
#   
#   # Generate factor with clean intervals
#   y <- Hmisc::cut2(
#     x,
#     cuts = quantile(x, probs = qbreaks, na.rm = TRUE),
#     m = group_obs,
#     formatfun = simple_fmt
#   )
#   
#   # Extract numeric bounds from levels
#   bounds <- stringr::str_match(levels(y), "\\[?\\s*([0-9\\.]+),\\s*([0-9\\.]+)\\s*\\]?\\)?")
#   
#   # Build new labels with commas and en-dash
#   formatted_levels <- paste0(
#     format(as.numeric(bounds[, 2]), big.mark = ",", trim = TRUE),
#     " – ",
#     format(as.numeric(bounds[, 3]), big.mark = ",", trim = TRUE)
#   )
#   
#   # Assign new levels
#   levels(y) <- formatted_levels
#   
#   return(y)
#   
# }
