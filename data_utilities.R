# data completeness tests
require(dplyr)

check_data_completeness <- function(md, na.thresh = 0) {
  # find numeric variables that have more than `na.thresh` missing values
  # defaults to zero, all rows for each variable must be finite
  navar <- 
    md %>%
    summarise(across(where(is.numeric), function(x) sum(is.na(x)))) %>%
    select_if(function(x) x > na.thresh)
  # isolate variable names that are missing and complete by this criterion
  missing <- names(navar)
  complete <- base::setdiff(names(md), missing)
  # variables with zero variance are useless for regression
  zerovar <- 
    md %>%
    summarise(across(where(is.numeric), function(x) var(x))) %>%
    select_if(function(x) x == 0) %>%
    names
  # final list of plausible variables for modeling under `na.thresh` criterion
  okay <- base::setdiff(complete, zerovar)
  return(list(missing=missing, complete=complete, zerovar=zerovar, okay=okay))
}

run_continuity <- function(idx, max_obs, verbose = F) {
  # idx is an id variable that performs like an integer
  # e.g. run of years works as-is, or any factor that can be
  # releveled into integers
  # max_obs is the length of a complete run of idx
  inclusive <- seq(min(idx), max(idx), 1) # full run of this index
  n_obs <- length(idx) # 
  complete = n_obs == max_obs # compare this run to full possible run of index
  longest_run <- length(inclusive) # default, gets updated in test below
  longest_missing <- 0 # default
  # recent_end <- year(today()) -max(idx) <2 # this doesn't work for yearmon!
  #             ^^^^^^^^^^^^^ this needs to be an index
  # recent_end <- max_obs - length(idx) < 2
  recent_end <- max_obs - max(idx) <2
  recent <- n_obs *recent_end
  if (length(inclusive) -n_obs >0) {
    # convert the run of observation years into run of 0 (=not obs) and 1 (=obs)
    z <- rep(0, length(inclusive))
    z[match(idx, inclusive)] <- 1
    run <- rle(z)
    if (verbose) print(run)
    ii <- which(run$values > 0)
    longest_run <- max(run$lengths[ii])
    jj <- which(run$values < 1)
    if (length(jj) >0) longest_missing <- max(run$lengths[jj])
    last_run <- length(run$values)
    # using the fact we assigned 1 to obs to implement two indicator funcs
    # so recent is an integer only if both the most recent years were observed 
    # and the most recent obs are within the past 2 years
    recent <- run$values[last_run] *run$lengths[last_run] *recent_end
    #         run=1 indicator ----- return integer ------- cutoff indicator
  }
  tibble(start = min(idx)
         ,end = max(idx)
         ,n_obs = n_obs
         #,idx = end -start +1
         ,longest_run
         ,longest_missing
         ,recent
         ,complete
  )
}

# ma_window_FROM_DF <- function(m, v, id, n_vec, f = lead, na.rm = T) {
#   # returns a ts object of moving average using a forward looking window
#   # (f=lead) by default. can optionally use backward window (f=lag).
#   # for m an input tibble or dataframe, v the name of the column to apply
#   # moving average to, id the name of the column that orders the vector v,
#   # and n_vec the specific set or sequence of lead/lag values to take of
#   # vector v. necessarily produces NA values during lead/lag in ma_matrix
#   # so the last option is whether to take means with or without dropping NAs.
#   # na.rm =TRUE in args will eliminate them from the ts object and adjust
#   # the ts index if NAs are omitted
#   lead_window <- function(m, v, n_vec, f = f) {
#     map(n_vec, f, x = pull(m, v)) %>%
#       set_names(paste0(v, "_", n_vec)) %>% 
#       as.data.frame()
#   }
#   ma_matrix <- mutate(lead_window(m, v, n_vec, f))
#   # print(ma_matrix)
#   ma <- rowMeans(ma_matrix, na.rm = na.rm) # arg is used here
#   return( na.omit( with(m, ts(ma, start = min(pull(m, id)))) ) )
# }

ma_window <- function(v, n_max, f = lead, na.rm = T) {
  # returns a ts object of moving average using a forward looking window
  # (f=lead) by default. can optionally use backward window (f=lag).
  # for m an input tibble or dataframe, v the name of the column to apply
  # moving average to, id the name of the column that orders the vector v,
  # and n_vec the specific set or sequence of lead/lag values to take of
  # vector v. necessarily produces NA values during lead/lag in ma_matrix
  # so the last option is whether to take means with or without dropping NAs.
  # na.rm =TRUE in args will eliminate them from the ts object and adjust
  # the ts index if NAs are omitted
  lead_window <- function(m, v, n, f = f) {
    map(n, function(i) f(as.vector(v), i)) %>%
      set_names(paste0("value_", n)) %>% 
      as.data.frame()
  }
  n <- 0:(n_max-1)
  ma_matrix <- mutate(lead_window(m, v, n, f))
  # print(ma_matrix)
  # return( na.omit( with(m, ts(ma, start = min(pull(m, id)))) ) )
  ma <- ts( rowMeans(ma_matrix, na.rm = na.rm) ) # arg is used here
  tsp(ma) <- tsp(v)
  return(ma)
}

# func_window <- function(v, n_max, f = mean, direction = lead, na.rm = T) {
#   # returns a ts object of moving average using a forward looking window
#   # (f=lead) by default. can optionally use backward window (f=lag).
#   # for m an input tibble or dataframe, v the name of the column to apply
#   # moving average to, id the name of the column that orders the vector v,
#   # and n_vec the specific set or sequence of lead/lag values to take of
#   # vector v. necessarily produces NA values during lead/lag in ma_matrix
#   # so the last option is whether to take means with or without dropping NAs.
#   # na.rm =TRUE in args will eliminate them from the ts object and adjust
#   # the ts index if NAs are omitted
#   lead_window <- function(m, v, n, f = direction) {
#     map(n, function(i) direction(as.vector(v), i)) %>%
#       set_names(paste0("value_", n)) %>% 
#       as.data.frame()
#   }
#   n <- 0:n_max
#   ma_matrix <- mutate(lead_window(m, v, n, f))
#   # print(ma_matrix)
#   # return( na.omit( with(m, ts(ma, start = min(pull(m, id)))) ) )
#   ma <- ts( rowMeans(ma_matrix, na.rm = na.rm) ) # arg is used here
#   tsp(ma) <- tsp(v)
#   return(ma)
# }

slope_window <- function(v, n = 3) {
  # returns a ts object of the slope of a vector of ordered values in
  # a data frame. slope is from a centered (default=3) window.
  # n_vec must be an odd number to assign the derivative at each point.
  # for m an input tibble or dataframe, v the name of the column to apply
  # moving average to, id the name of the column that orders the vector v,
  # and n_vec the specific set or sequence of lead/lag values to take of
  # vector v. necessarily produces NA values during lead/lag in ma_matrix
  # so the last option is whether to take means with or without dropping NAs.
  centered_window <- function(v, n) {
    n_part <- (n -1) /2 # already excluded even n
    n_lag <- n_part:0; n_lead <- 1:n_part
    cbind(
      map(n_lag, function(n) dplyr::lag(as.vector(v), n)) %>%
        set_names(paste0("value", "_m", n_lag)) %>%
        data.frame(),
      map(n_lead, function(n) dplyr::lead(as.vector(v), n)) %>%
        set_names(paste0("value", "_", n_lead)) %>% 
        data.frame()
    )
  }
  if (n/2 == round(n/2)) stop("requires odd integer for n\n")
  cent_matrix <- centered_window(v, n)
  # print(cent_matrix)
  x <- 1:n
  s <- ts( apply(cent_matrix, 1, function(y) coef(lm(y ~ x))["x"]) )
  tsp(s) <- tsp(v)
  return(s)
}
