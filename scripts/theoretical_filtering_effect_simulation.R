## Toy model: how do we expect our estimated markrate to change as MSF anglers
## catch fish. 
## Motivated by conversation with Craig on 1/2/25
## 
## Basic idea: imagine the fish are one giant pool and anglers are drawing from that pool, and returning unmarked fish (who experience a 10% catch mortality)
## We can look at how the catch markrate varies compared to the "intitial" markrate
## due to the filtering effect.
## 
## This is something we probably could solve analytically with combinitorics (maybe?), but simulation is pretty easy here
## Note that this is NOT a great representation of what's happening, because fish are MOVING. It's not the same pool for the whole season. We could think about addressing this by adding complexity to the sim, but this is just a test run. 

pool.tot = 10000 ## total size of our simulated pool
prop.ad = seq(0.1, 0.9, by = 0.1) # proportion of pool that is marked

parms.df = data.frame(init.ad = round(pool.tot*prop.ad),
                      init.um = round(pool.tot*(1-prop.ad)))
parms.df = expand_grid(parms.df, 
                       n = round(seq(500, 9000, by = 500))) #N is the total number of encounters


um.mort = 0.1

res.df = parms.df
res.df$init.mr = NA_real_
res.df$catch.mr = NA_real_


for(i.row in 1:nrow(parms.df)){
  ##check: how do I add 
  ## starting
  n = parms.df$n[i.row]
  pool.um = pool.um.init = parms.df$init.ad[i.row]
  pool.ad = pool.ad.init = parms.df$init.um[i.row]
  
  samp.marked.vec = NULL
  
  for(i in 1:n){
    samp.marked = sample(c(TRUE, FALSE), size = 1, prob = c(pool.ad, pool.um))
    samp.marked.vec = c(samp.marked.vec, samp.marked)
    
    if(samp.marked){
      pool.ad = pool.ad-1
    }else{
      pool.um = pool.um - (1*um.mort)
    }
  }
  
  res.df$catch.mr[i.row] = mean(samp.marked.vec)
  
  res.df$init.mr[i.row] = pool.ad.init / (pool.ad.init + pool.um.init)
}

res.df = res.df |> 
  mutate(total.pool = init.ad + init.um,
         catch.of.pool = n/total.pool)

ggplot(res.df |> 
         filter(init.mr %in% c(.3, .5, .7)), 
       aes(x = catch.of.pool, y = catch.mr, col = as.factor(init.mr)))+
  geom_path()
