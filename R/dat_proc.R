library(tidyverse)
library(lubridate)
library(mgcv)
library(colorspace)
library(here)

source('R/funcs.R')

load(file = here('data', 'modssta_chl.RData'))

colpal <- "PuBuGn"

mytheme <- theme_bw(base_family = 'serif', base_size = 16) +  
  theme(
    plot.background = element_rect(fill='transparent', 
                                   colour = NA),
    panel.background = element_rect(fill='transparent', 
                                    colour = NA),
    legend.background = element_rect(fill='transparent', 
                                     colour = NA),
    legend.key = element_rect(fill = 'transparent', 
                              colour = NA),
    axis.ticks.length = unit(.1, "cm"), 
    plot.margin = unit(c(1, 1, 1, 1), 'pt')
  )

# title graphic -----------------------------------------------------------

station <- c(18, 21, 22, 24, 27, 30, 32, 36)
modi <- 'gam2'

mods <- modssta_chl %>% filter(station %in% !!station)

rawchl <- mods %>% 
  filter(modi %in% !!modi) %>% 
  mutate(
    station = paste0('s', station),
    data = purrr::map(data, function(x){
      x %>% 
        mutate(
          day = day(date)
        )
    })
  ) %>% 
  select(station, data) %>% 
  unnest

prdplo <- mods %>%
  filter(modi %in% !!modi) %>% 
  mutate(prddat = pmap(list(data, modv), function(data, modv){
    
    prddat <- data.frame(
      dec_time = seq(min(data$dec_time), max(data$dec_time), length = 1000)
    ) %>%
      mutate(
        date = date_decimal(dec_time),
        date = as.Date(date),
        mo = month(date, label = TRUE),
        doy = yday(date),
        yr = year(date)
      )
    
    prd <- predict(modv, newdata = prddat)
    
    prddat <- prddat %>% mutate(chl = prd)
    
    return(prddat)
    
  })) %>% 
  select(station, prddat) %>% 
  unnest %>% 
  mutate(station = paste0('s', station))

p <- ggplot(prdplo, aes(x = doy, group = factor(yr), colour = yr)) + 
  geom_line(aes(y = chl)) + 
  mytheme + 
  theme(
    legend.position = 'none', 
    legend.title = element_blank(), 
    strip.background = element_blank()
  ) + 
  scale_colour_continuous_sequential(palette = colpal, rev = T, begin = 0.1) +
  labs(y = 'log10(chl)', x = 'Day of year') +
  facet_wrap(~ station, ncol = 8, scales = 'free_y') +
  guides(colour = guide_colourbar(barheight = 1, barwidth = 20))

pdf(here('fig', 'titlegraph.pdf'), height = 2, width = 15, family = 'serif')
p
dev.off()
