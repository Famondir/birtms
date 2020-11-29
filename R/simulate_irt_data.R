library(tidyverse)
library(brms)
library(magrittr)
# library(tictoc)

simulate_normal_distributions <- function(n = integer(), standard_deviations = double()) {
  theta_distributions <- map(standard_deviations, rnorm, n = n, mean = 0) %>% as_tibble()
}

thetas <- tibble(theta1 = 1, theta2 = 1, theta3 = 1) %>%
  simulate_normal_distributions(n = 1000)
thetas %>% walk2(names(.), ~hist(.x, main = .y, xlab = .y))

n_items <- 20

item_distributions <- tibble(beta = 1.5, logalpha1 = 0.5, logalpha2 = 0.6, logalpha3 = 0.4) %>%
  simulate_normal_distributions(n = n_items)

gamma <-  .0

map_alpha1 <- c(rep(1:0, each = 10))
map_alpha2 <- c(rep(0:1, each = 10))
map_alpha3 <- c(rep(0:1, 10))

map_alpha <- tibble(alpha1 = map_alpha1, alpha2 = map_alpha2, alpha3 = map_alpha3)

alphapars <- item_distributions %>% select(logalpha1:logalpha3)

alphas <- map_alpha*alphapars %>% exp()
# alphas$alpha2 <- 0
# alphas$alpha3 <- 0

# tic()
probabilities <- map2(alphas, thetas, ~.x %*% t(.y)) %>%
  reduce(`+`) %>%
  as_tibble(.name_repair = "unique") %>% setNames(c(paste0('p', c(1:1000)))) %>%
  add(item_distributions$beta) %>%
  inv_logit_scaled %>%
  multiply_by(1 - gamma) %>%
  add(gamma)
# toc()

len <- nrow(probabilities)
responses <- probabilities %>% map(~rbinom(n = len, size = 1, prob = .)) %>%
  as_tibble() %>% setNames(c(paste0('p', c(1:1000)))) %>%
  mutate(item = paste0('i', 1:n_items), .before = 1) %>%
  cbind(map_alpha) %>% mutate(map_alpha1 = as.integer(alpha1),# müssen integer nicht factors sein!
                              map_alpha2 = as.integer(alpha2),
                              map_alpha3 = as.integer(alpha3),
                              .before = 2) %>%
  select(-c(alpha1, alpha2, alpha3))

irt_data <- responses %>% pivot_longer(cols = !c('item', 'map_alpha1', 'map_alpha2', 'map_alpha3'),
                                       names_to = 'person', values_to = 'response')

saveRDS(irt_data, 'irt_data.RDS')
