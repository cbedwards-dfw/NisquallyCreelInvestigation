##################################################
## Collin Edwards
## Fri Aug  1 08:56:41 2025
## Simulations to make sure I know what's going on
##################################################

## I had a concern that we were not appropriately converting out interview data into
##   rate problems
##   Solution: simulations.
##   Answer: I *was* handling this incorrectly. The correct approach is offset(log(angler_minutes))

N = 50
data = data.frame(treat = as.factor(rep(c("A", "B"), each = N)))
data$duration = runif(nrow(data), min = 2, max = 6)

data = data |> 
  mutate(rate = if_else(treat == "A", 2, 5)) |> 
  mutate(expected = duration*rate)

data$count = rpois(nrow(data), lambda = data$expected)

out1 = gam(count ~ treat + duration,
           method = "REML",
           family = "nb", data = data)

out3 = gam(count ~ treat + treat:duration,
           method = "REML",
           family = "nb", data = data)


out1.5 = gam(count ~ treat + offset(duration),
           method = "REML",
           family = "nb", data = data)

out2 = gam(count ~ treat + offset(log(duration)),
           method = "REML",
           family = "nb", data = data)

out2.1 = gam(count ~ treat,
           method = "REML",
           family = "nb", data = data,
           offset = log(duration))



predmat = expand_grid(treat = unique(data$treat), duration = c(2, 5))
predmat = predmat |> 
  left_join(data |> 
              select(treat, rate) |> 
              distinct(), by = "treat") |> 
  mutate(expected = duration*rate)

predmat$pred_1 = predict(out1, newdata = predmat, type = 'response')
predmat$pred_1.5 = predict(out1.5, newdata = predmat, type = 'response')
predmat$pred_2 = predict(out2, newdata = predmat, type = 'response')
predmat$pred_2.1 = predict(out2.1, newdata = predmat, type = 'response')
predmat$pred_3 = predict(out3, newdata = predmat, type = 'response')

predmat
