calc_archetypes_vistis <- function(words, archetypes) {
  dt <- data.frame(text = words, archetype = archetypes, order = c(1:length(words)), previous_archetypes=NA) %>%
    mutate(prev_archetype = lag(archetype))
  # you want number of returns to an archetype
  for (ii in c(2:nrow(dt))) {
    dt$previous_archetypes[ii] <- paste(dt$archetype[1:ii-1], collapse = ";;")
  }
  
  res <- dt %>%
    rowwise() %>%
    mutate(is_return = grepl(archetype, previous_archetypes) & 
             archetype != prev_archetype &
             !is.na(prev_archetype) &
             !is.na(previous_archetypes)) %>%
    group_by(archetype) %>% summarise(returns = sum(is_return))
  return(res)
}