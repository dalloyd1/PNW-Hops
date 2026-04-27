require(tseries)

# Adapted from response at Cross-Validated
# https://stats.stackexchange.com/questions/30569/what-is-the-difference-between-a-stationary-test-and-a-unit-root-test

urSieve <- function(x, k=1, cl=0.95) {
  # Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test for stationarity
  kpss <- suppressWarnings(kpss.test(x))
  kpss.rejectH0 <- kpss$p.value < 1. -cl
  # Augmented Dickey-Fuller unit-root test
  # H0: has unit root; Alt: Process has root outside unit circle
  #     Alt hypothesis is usually equiv. to stationarity or trend stationarity
  adf <- suppressWarnings(adf.test(x, alternative = "stationary", k=k))
  adf.rejectH0 <- adf$p.value < 1. -cl
  outcome <- case_when(!adf.rejectH0 & kpss.rejectH0 ~ "Unit root",
                       adf.rejectH0 & !kpss.rejectH0 ~ "Stationary",
                       !adf.rejectH0 & !kpss.rejectH0 ~ "Insufficient data",
                       adf.rejectH0 & kpss.rejectH0 ~ "Heteroscedastic?"
                       )
  outcome
}

