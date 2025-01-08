reprex::reprex({
  suppressMessages(library(tidyverse))
  library(fmsb)
  maxmin <- tibble(
    total = c(5, 1),
    phys = c(15, 3),
    psycho = c(3, 0),
    social = c(5, 1),
    env = c(5, 1))
  # data for radarchart function version 1 series, minimum value must be omitted from above.
  RNGkind("Mersenne-Twister")
  set.seed(123)
  dat <- tibble(
    total = runif(3, 1, 5),
    phys = rnorm(3, 10, 2),
    psycho = c(0.5, NA, 3),
    social = runif(3, 1, 5),
    env = c(5, 2.5, 4)
  )
  dat <- rbind(maxmin, dat)
  VARNAMES <- c("Total\nQOL", "Physical\naspects", "Phychological\naspects", 
                "Social\naspects", "Environmental\naspects")
  radarchart(
    dat, axistype = 1, seg = 5, plty = 1,
    vlabels = VARNAMES, 
    title="(axis=1, 5 segments, with specified vlabels)", 
      vlcex=0.5
    )
})
