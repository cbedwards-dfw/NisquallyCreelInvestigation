nbinom_prob_helper = function(x){dnbinom(0:30, size = cur.theta, mu = x)}

calc_probs_nbinom = function(mod, 
                             dat.new,
                             identifier.name = NULL){
  mus = predict(mod, newdata = dat.new, type = "response")
  cur.theta = mod$family$getTheta(TRUE)
  df = data.frame(mu = mus)|> 
    mutate(mat = do.call(rbind, purrr::map(mu, nbinom_prob_helper)))
  df.prob = as_tibble(df$mat)
  names(df.prob) = 0:(ncol(df.prob)-1)
  df.prob |> 
    mutate(row_number = 1:nrow(df.prob),
           .before = "0") |> 
    pivot_longer(cols = !any_of("row_number"),
                 names_to = "fish_count",
                 values_to = "prob") |> 
    mutate(fish_count = as.numeric(fish_count))
}

## simplify dat.new to include only important parts and add row identifier
fish.caught = dat.new |> 
  select(interview_id, mark_stage, fish_count_observed = fish_count) |> 
  mutate(row_number = 1:nrow(dat.new))
probs = calc_probs_nbinom(out.best, dat.new) |> 
  rename(fish_count_prob = fish_count) |> 
  group_by(row_number) |> 
  mutate(prob = prob/sum(prob)) |> 
  ungroup() |> 
  left_join(fish.caught, by = "row_number")

plower = probs |> 
  filter(fish_count_prob < fish_count_observed) |> 
  group_by(row_number, interview_id, fish_count_observed, mark_stage) |> 
  summarize(prob_lower = sum(prob)) |> 
  ungroup() 
phigher =  probs |> 
  filter(fish_count_prob > fish_count_observed) |> 
  group_by(row_number, interview_id, fish_count_observed, mark_stage) |> 
  summarize(prob_higher = sum(prob)) |> 
  ungroup() 
pexact = probs |> 
  filter(fish_count_prob == fish_count_observed) |> 
  select(row_number, interview_id, fish_count_observed, prob_exact = prob, mark_stage)

ptails = plower |> 
  full_join(phigher, 
            by = c("row_number", "interview_id", "fish_count_observed", "mark_stage") 
            ) |> 
  full_join(pexact, 
            by = c("row_number", "interview_id", "fish_count_observed", "mark_stage") 
  ) |> 
  relocate(mark_stage, .before = fish_count_observed)
ptails$prob_lower[ptails$fish_count_observed == 0] = 0
ptails = ptails |> 
  mutate(prob_min = pmin(prob_lower, prob_higher)+prob_exact)

p_final = ptails |> 
  select(-row_number, -prob_lower, -prob_higher, -prob_exact) |> 
  rename(fish = fish_count_observed) |> 
  mutate(mark_stage = gsub(" ", "_", mark_stage)) |> 
  pivot_wider(names_from = mark_stage,
              values_from = c(fish, prob_min)) |> 
  mutate(prob_total = prob_min_AD_X_Adult * 
           prob_min_UM_X_Adult *
           prob_min_AD_X_Jack *
           prob_min_UM_X_Jack
         )

dat.sum = left_join(dat.sum, p_final, by = "interview_id")

dat.sum |> 
  filter(kept_total+released_total > 0) |> 
  ggplot(aes(x = prob_total))+
  geom_histogram(bins = 100)+
  labs(x = "product of unlikelihood of all four reported catches",
       title = "Relative probability of interviews that caught 1+ fish")+
  theme_bw(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))

dat.sum |> 
  # filter(kept_total+released_total > 0) |> 
  ggplot(aes(x = prob_total))+
  geom_histogram(bins = 100)+
  labs(x = "product of unlikelihood of all four reported catches",
       title = "Relative probability of interviews")+
  theme_bw(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))        

dat.sum |> 
  # filter(kept_total+released_total > 0) |> 
  ggplot(aes(x = prob_min_UM_X_Adult, fill = as.factor(fish_UM_X_Adult)))+
  geom_histogram(bins = 100)+
  labs(x = "Unlikelihood of UM adults",
       title = "Predicted relative probability of reported UM adults",
       fill = "# of UM caught")+
  theme_bw(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))       

dat.sum |> 
  filter(kept_total+released_total > 0) |>
  ggplot(aes(x = prob_min_AD_X_Adult, fill = as.factor(fish_AD_X_Adult)))+
  geom_histogram(bins = 100)+
  facet_wrap(.~ as.factor(fish_AD_X_Adult), ncol = 1)+
  labs(x = "Unlikelihood of AD adults",
       title = "Predicted relative probability of reported UM adults",
       fill = "# of UM caught")+
  theme_bw(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))                     

## concern: probability of exact match is really high for 0s.
ptails |> 
  ggplot(aes(x = fish_count_observed, y = prob_exact))+
  geom_jitter()+
  labs("Probability of exact observed count",
       x = "observed count",
       y = "probability")+
  theme_bw(base_size = 13)


dat.sum |> 
  filter(prob_min_UM_X_Adult <0.1) |> 
  ggplot(aes(x = prob_min_UM_X_Adult))+
  geom_histogram(bins = 100)+
  labs(x = "Probability of UM catch or more extreme",
       title = "Relative probability of reported UM")+
  theme_bw(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))  
