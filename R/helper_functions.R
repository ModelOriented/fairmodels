### HELPER FUNCTIONS ###


fairness_check_metrics <- function(){
  out <- c('ACC', "TPR", 'PPV', 'FPR', 'STP')
  return(out)
}


drop_metrics_with_na <- function(data){
na_col_index <- apply(data, 2,  function(x) any(is.na(x)))
cols_with_missing <- names(data)[na_col_index]

if (any(na_col_index)) warning("Found metric with NA: ", paste(cols_with_missing, collapse = ", "), ", omiting it\n")

data <- data[, !na_col_index]
return(data)
}

assert_base_metrics <- function(metric){
  assert_parity_metrics(metric)
}

assert_performance_metrics <- function(metric){
  if( !( is.character(metric)    & length(metric) ==1 )) stop("metric must be character")
  metrics <- c("accuracy", "f1", "precision", "recall", "auc")
  if (! metric %in% metrics) stop ("Metric not in available metrics")
  invisible(return())
}

parity_loss_metrics <- function(){
  return( c("TPR","TNR","PPV","NPV","FNR","FPR","FDR","FOR","TS","STP","ACC","F1"))
}

assert_parity_metrics <- function(metric){

  if( !( is.character(metric)    & length(metric) ==1 )) stop("metric must be character")
  metrics <- parity_loss_metrics()
  if (! metric %in% metrics) stop ("Metric not in available metrics")

  invisible(return())
}


assert_equal_parameters <- function(x, parameter) {
  param_to_compare <- x[[1]][[parameter]]

  for (obj in x){
     if (obj[[parameter]] != param_to_compare) stop("Parameters have different values")
  }
}


extract_data  <- function(x, parameter){

  data_list         <- lapply(x, function(x) x[[parameter]])
  data              <- do.call("rbind", data_list)
  return(data)
}

assert_different_label <- function(x){

  labels <- unlist(lapply(x, function(x) x$label))

  if (length(labels) != length(unique(labels))) stop("Some models have the same fairness labels, be sure to print/plot objects with different label ")

}


get_objects <- function(x, class){

  stopifnot(class(x) == "list")

  explainers <- list()
  j <- 1

  for (i in seq_along(x)){
    if (class(x[[i]]) == class) {
      explainers[[j]] <- x[[i]]
      j               <- j + 1
    }
  }

  return(explainers)
}


colors_fairmodels <- function(n = 2){
# bases on drwhy color guide
# 13 distinct colors needed
  if (n < 8){
    return(DALEX::colors_discrete_drwhy(n = n))
  }
  if (n == 8) {
    return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD"))
    }
  if (n == 9){
   return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD",
                     "#c295f0"))
  }
  if (n == 10) {
    return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD",
                     "#c295f0",
                     "#037B63"))
  }
  if (n == 11){
   return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD",
                     "#c295f0",
                     "#037B63",
                     "#733E6B"))
  }
  if (n == 12){

   return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD",
                     "#c295f0",
                     "#037B63",
                     "#733E6B",
                     "#9fdf9f"))
  }

  if (n == 13){
    return(c(DALEX::colors_discrete_drwhy(n = 7),
                     "#B622AD",
                     "#c295f0",
                     "#037B63",
                     "#733E6B",
                     "#9fdf9f",
                     "#794469"))
  }

    c(DALEX::colors_discrete_drwhy(n = 7),
      "#B622AD",
      "#c295f0",
      "#037B63",
      "#733E6B",
      "#9fdf9f",
      "#794469")[((0:(n - 1))%%13) + 1]

}



quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}


############# checks #############

check_unique_names <- function(x){
  return(length(unique(names(x))) == length(names(x)))
}

check_names_in_names_vector <- function(x, y){
  return(names(x) %in% y)
}


check_list_elements_numeric <- function(x){
  stopifnot(is.list(x))
  return(all(is.numeric(unlist(x))))
}


check_values <- function(x, lower, upper){
  return((all(x >= lower) & all(unlist(x) <= upper)))
}

check_if_numeric_and_single <- function(x){
  return((is.numeric(x) & length(x) == 1))
}

calculate_parity_loss <- function(gmm, privileged){

  gmm_scaled      <- apply(gmm, 2 , function(x) x  / gmm[, privileged])
  gmm_abs         <- apply(gmm_scaled, 2 , function(x) sapply(x, function(y) abs(log(y))))
  gmm_loss        <- rowSums(gmm_abs)
  # when there is Inf in data change it to NA
  parity_loss     <- sapply(gmm_loss, function(y) ifelse(is.infinite(y), NA, y))

  return(parity_loss)
}


check_protected <-  function(protected, fobjects, verbose) {

  if (is.null(protected)) {
    if (length(fobjects) > 0) {
      # getting from first explainer - checking is later
      protected <- fobjects[[1]][["protected"]]
      verbose_cat(color_codes$yellow_start, "from first fairness object", color_codes$yellow_end, ") \n", verbose = verbose)
    } else {
      stop("\nProtected cannot be NULL if fairness_objects are not provided")
    }} else {
      if (is.factor(protected)) {
        verbose_cat(color_codes$green_start, "Ok", color_codes$green_end, ") \n", verbose = verbose)
      } else {
        verbose_cat(color_codes$yellow_start, "changed from", class(protected),  color_codes$yellow_end, ")\n", verbose = verbose)
        protected <- as.factor(protected)
      }}

  return(protected)

  }

check_privileged <- function(privileged, fobjects, verbose) {

  if (is.null(privileged)) {

    if (length(fobjects) > 0) {

      # getting from first explainer - checking is done later
      privileged <- fobjects[[1]][["privileged"]]
      verbose_cat(class(privileged), "(" , verbose = verbose)
      verbose_cat(color_codes$yellow_start, "from first fairness object", color_codes$yellow_end, ") \n", verbose = verbose)

      } else {
      stop ("\nPrivileged cannot be NULL if fairness_objects are not provided")}

    } else {
      # if protected and privileged are not characters, changing them
      if (is.character(privileged) | is.factor(privileged)) {
        verbose_cat(class(privileged), "(", verbose = verbose)
        verbose_cat(color_codes$green_start, "Ok", color_codes$green_end, ")\n", verbose = verbose)
      } else {
        verbose_cat("character (", verbose = verbose)
        verbose_cat(color_codes$yellow_start, "changed from", class(privileged), color_codes$yellow_end, ")\n", verbose = verbose)
        privileged <- as.character(privileged)
      }
    }

  return(privileged)
}

check_fobjects <- function(fobjects, protected, privileged, verbose){

  if (length(fobjects) == 1){
    verbose_cat(" object ", verbose = verbose)
  } else {
    verbose_cat(" objects ", verbose = verbose)
  }


  if (length(fobjects) > 0) {
    if(! all(sapply(fobjects, function(x) x$protected == protected))) {
      verbose_cat("(",color_codes$red_start, "not compatible" ,color_codes$red_end, ") \n", verbose = verbose)
      stop("fairness objects must have the same protected vector as one passed in fairness check")
    }
    if(! all(sapply(fobjects, function(x) x$privileged == privileged))) {
      verbose_cat("(", color_codes$red_start, "not compatible" ,color_codes$red_end, ") \n", verbose = verbose)
      stop("fairness objects must have the same privlieged argument as one passed in fairness check")
    }
    verbose_cat("(", color_codes$green_start, "compatible", color_codes$yellow_end,  ")\n", verbose = verbose)
  } else {
    verbose_cat("\n", verbose = verbose)}

  return(fobjects)
}


check_explainers_clf <- function(all_explainers, protected, verbose){

  if(! all(sapply(all_explainers, function(x) x$model_info$type == 'classification'))) {
    verbose_cat("(", color_codes$red_start, "model type not supported", color_codes$red_end, ")\n", verbose = verbose)
    stop("All models must be binary classification type. To check fairness in regression use 'fairness_check_regression()'")

  }

  return(check_explainers(all_explainers, protected, verbose))
}


check_explainers_reg <- function(all_explainers, protected, verbose){

  if(! all(sapply(all_explainers, function(x) x$model_info$type == 'regression'))) {
    verbose_cat("(", color_codes$red_start, "model type not supported", color_codes$red_end, ")\n", verbose = verbose)
    stop("All models must be regression type. To check fairness in binary classification use 'fairness_check()'")

  }

  return(check_explainers(all_explainers, protected, verbose))

}

check_explainers <- function(all_explainers, protected, verbose){

  y_to_compare <- all_explainers[[1]]$y

  if(! all(sapply(all_explainers, function(x) length(y_to_compare) == length(x$y)))) {
    verbose_cat("(", color_codes$red_start, "y not equal", color_codes$red_end, ")\n", verbose = verbose)
    stop("All explainer predictions (y) must have same length")
  }

  if(! all(sapply(all_explainers, function(x) y_to_compare == x$y))) {
    verbose_cat("(", color_codes$red_start, "y not equal", color_codes$red_end, ")\n", verbose = verbose)
    stop("All explainers must have same values of target variable")
  }


  if(! all(sapply(all_explainers, function(x) length(x$y) == length(protected)))) {
    verbose_cat("(", color_codes$red_start, "not compatible", color_codes$red_end, ")\n", verbose = verbose)
    stop("Lengths of protected variable and target variable in explainer differ")
  }

  verbose_cat("(", color_codes$green_start, "compatible", color_codes$yellow_end,  ")\n", verbose = verbose)
  return(all_explainers)
}

check_labels <- function(label, explainers, fobjects_label){

  if (is.null(label)) {
    label     <- sapply(explainers, function(x) x$label)
  } else {
    if (length(label) != length(explainers)) stop("Number of labels must be equal to number of explainers (outside fairness objects)")
  }

  # explainers must have unique labels
  if (length(unique(label)) != length(label) ) {
    stop("Explainers don't have unique labels
        ( pass paramter \'label\' to fairness_check() or before to explain() function)")
  }

  # fobjects must have unique labels
  if (length(unique(fobjects_label)) != length(fobjects_label) ) {
    stop("Fairness objects don't have unique labels
        ( make sure that Fairness Objects will have unique labels of explainers before running fainress check")
  }

  # labels must be unique for all explainers, those in fairness objects too
  if (any(label %in% fobjects_label)) {
    stop("Explainer has the same label as label in fairness_object")
  }
  return(label)
}



get_nice_ticks <- function(min_value, max_value, max_ticks = 9){

  tick_range <- readable_number(max_value - min_value, FALSE)
  tick_spacing <- readable_number(tick_range / (max_ticks - 1), TRUE)
  readable_minimum <- floor(min_value / tick_spacing) * tick_spacing
  readable_maximum <- ceiling(max_value / tick_spacing) * tick_spacing
  return(list(min = readable_minimum, max = readable_maximum, spacing = tick_spacing))
}

readable_number <- function(tick_range, round_number){

  exponent <- floor(log10(tick_range))
  fraction <- tick_range/(10**exponent)

  if (round_number){

    if(fraction < 1.5){
      readable_tick <- 1
    } else if (fraction < 3) {
      readable_tick <- 2
    } else if (fraction < 7) {
      readable_tick <- 5
    } else {
      readable_tick <- 10
    }
  } else {

    if(fraction <= 1){
      readable_tick <- 1
    } else if (fraction <= 2) {
      readable_tick <- 2
    } else if (fraction <= 5) {
      readable_tick <- 5
    } else {
      readable_tick <- 10
    }
  }

  return(readable_tick * 10**exponent)
}


color_codes <- list(yellow_start = "\033[33m", yellow_end = "\033[39m",
                    red_start = "\033[31m", red_end = "\033[39m",
                    green_start = "\033[32m", green_end = "\033[39m")



verbose_cat <- function(..., verbose = TRUE) {
  if (verbose) {
    cat(...)
  }
}













