library(tidyverse)
library(lubridate)
library(mgcv)
library(colorspace)
library(here)
library(WRTDStidal)
library(png)
library(pdftools)
library(grid)
library(gridExtra)
library(gganimate)

pal <- function(x) rev(colorspace::sequential_hcl(x, palette = "PuBuGn"))

source('R/funcs.R')

load(file = here::here('data', 'modssta_chl.RData'))
load(file = here::here('data', 'map.RData'))
load(file = here::here('data', 'locs.RData'))
load(file = here::here('data', 'stat32res.RData'))

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

# schematic plots ---------------------------------------------------------

data(wrtds_ex)

# model subset and plot subsets
mod <- wrtds_ex
ylab <-  expression(paste("DIN (mg ", L^-1, ")"))
xlims <- as.Date(c('1976-01-01', '2013-12-31'))
ylims <- c(0, 6)

annuals <- prdnrmplot(mod, annuals = TRUE, plot = F) %>%
  .$nrms %>% 
  filter(taus == 0.5)
seasnls <- prdnrmplot(mod, annuals = FALSE, plot = F) %>%
  .$nrms %>% 
  filter(taus == 0.5)
residus <- select(mod, date, res, fit0.5) %>% 
  mutate(resfit = exp(res) - exp(fit0.5)) %>% 
  na.omit()

# default theme
mytheme <- theme_minimal(base_size = 16) + 
  theme(
    plot.background = element_rect(fill='transparent', 
                                   colour = NA),
    panel.background = element_rect(fill='transparent', 
                                    colour = NA),
    legend.background = element_rect(fill='transparent', 
                                     colour = NA),
    legend.key = element_rect(fill = 'transparent', 
                              colour = NA),
    axis.line.x = element_line(), 
    axis.line.y = element_line(), 
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(.1, "cm"), 
    panel.grid.major = element_line(colour = pal(num_col)[2]),
    panel.grid.minor = element_line(colour = pal(num_col)[2])
  )   

# plots
p1 <- ggplot(na.omit(mod), aes(x = date, y = exp(res))) + 
  geom_line() +
  mytheme +
  scale_y_continuous(ylab, limits = c(0, 4)) + 
  scale_x_date('Time', limits = xlims) + 
  labs(title = 'SF Bay Delta, station P8')

p2 <- ggplot(annuals, aes(x = date, y = exp(nrms_value))) + 
  geom_line() + 
  mytheme +
  scale_y_continuous(ylab, limits = c(0, 4)) + 
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) + 
  scale_x_date('Time', limits = xlims) + 
  ggtitle('Annual')

p3 <- ggplot(seasnls, aes(x = date, y = exp(nrms_value))) + 
  geom_line() + 
  mytheme +
  scale_y_continuous(ylab) + 
  scale_x_date('Time', limits = xlims) + 
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) + 
  ggtitle('Seasonal')

p4 <- ggplot(residus, aes(x = date, y = resfit)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 0)) + 
  mytheme +
  scale_y_continuous(ylab) + 
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) + 
  scale_x_date('Time', limits = xlims) + 
  ggtitle('Residual')

p5 <- dynaplot(mod, mo = 1, logspace = F, years = c(1976, 2000, 2013), col_vec = 'black') + 
  mytheme +
  theme(
    legend.position = 'none', 
    strip.text.x = element_blank(),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) + 
  scale_y_continuous(ylab) + 
  scale_x_continuous('Flow') + 
  ggtitle('Flow effects')

pdf('fig/ts_ex.pdf', height = 2, width = 12, family = 'serif')
p1
dev.off()

ht <- 1.75
wd <- 4.5

pdf('fig/schematic2.pdf', height = ht, width = wd, family = 'serif')
p2
dev.off()
pdf('fig/schematic3.pdf', height = ht, width = wd, family = 'serif')
p3
dev.off()
pdf('fig/schematic4.pdf', height = ht, width = wd, family = 'serif')
p4
dev.off()
pdf('fig/schematic5.pdf', height = ht, width = wd, family = 'serif')
p5
dev.off()


# map  --------------------------------------------------------------------

# ext <- make_bbox(locs$lon, locs$lat, f = 0.2)
# map <- get_stamenmap(ext,  zoom = 11, maptype = "toner-lite", where = getwd())
pbase <- ggmap(map) +
  theme_bw(base_size = 16) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.background = element_rect(fill='transparent', 
                                   colour = NA),
    panel.background = element_rect(fill='transparent', 
                                    colour = NA)
  )
p1 <- pbase + 
  geom_text(data = locs, aes(x = lon, y = lat, label = Station), size = 6, colour = pal(5)[4],
            fontface = 'italic') + 
  labs(title = 'South San Francisco Bay', subtitle = 'Long-term monitoring stations')

pdf('fig/map.pdf', height = 6, width = 5, family = 'serif')
p1
dev.off()


# gam examples ------------------------------------------------------------

prdplo <- stat32res %>%
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
  select(modi, prddat) %>% 
  unnest(prddat)

rawchl <- stat32res$data[[1]] %>% 
  mutate(
    day = day(date)
  )

ylabs <- expression(paste(log[10], ' Chl- ',italic(a),' (',italic('\u03bc'),'g ',L^-1,')'))

mods <- prdplo %>% 
  pull(modi) %>% 
  unique %>% 
  levels
for(i in seq_along(mods)){
  
  toplo <- prdplo %>% 
    filter(modi %in% mods[i])
  
  pblim <- range(log10(na.omit(rawchl$value)), na.rm = T)
  
  # plot
  pa <- ggplot() + 
    geom_point(data = rawchl, aes(x = date, y = log10(value)), size = 0.5) +
    geom_line(data = toplo, aes(x = date, y = chl), colour = 'darkgrey', size = 0.75, alpha = 0.8) + 
    stat_smooth(data = toplo, aes(x = date, y = chl), se = F, method = "loess", color = 'black', alpha = 0.7) +
    theme_bw(base_family = 'serif', base_size = 18) + 
    labs(y = ylabs) + 
    scale_y_continuous(limits = pblim) +
    scale_x_date(date_breaks = '5 years', date_labels = '%Y') + 
    theme(
      axis.title.x = element_blank(),
      plot.background = element_rect(fill='transparent', 
                                     colour = NA),
      panel.background = element_rect(fill='transparent', 
                                      colour = NA)
    )
  
  pb <- ggplot(toplo, aes(x = doy, group = factor(yr), colour = yr)) + 
    geom_line(aes(y = chl)) + 
    theme_bw(base_family = 'serif', base_size = 18) + 
    theme(
      legend.position = 'right', 
      legend.title = element_blank(), 
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      plot.background = element_rect(fill='transparent', 
                                     colour = NA),
      panel.background = element_rect(fill='transparent', 
                                      colour = NA), 
      legend.background = element_rect(fill = 'transparent', 
                                       colour = NA)
    ) + 
    scale_colour_continuous_sequential(palette = colpal, rev = T, begin = 0.1) +
    guides(colour = guide_colourbar(barheight = 20, barwidth = 1)) +
    scale_y_continuous(limits = pblim) +
    labs(x = 'Day of year')
  
  assign(paste0('pa', i), pa)
  assign(paste0('pb', i), pb)
  
}

pdf(here::here('fig', 'gamex.pdf'), family = 'serif', height = 4.5, width = 5)
pa1
pa2 
pa3
pa4
dev.off()

pdf(here::here('fig', 'gamex2.pdf'), family = 'serif', height = 4.5, width = 5)
pb1 
pb2
pb3
pb4
dev.off()


# animation ---------------------------------------------------------------

data("modssta_chl")

pal <- function(x) rev(colorspace::sequential_hcl(x, palette = "PuBuGn"))

# get predictions to plot
prdplo <- modssta_chl %>%
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
  select(station, modi, prddat) %>% 
  unnest(prddat)

ylabs <- expression(paste(log[10], ' Chl- ',italic(a),' (',italic('\u03bc'),'g ',L^-1,')'))

toplo <- prdplo %>% 
  filter(yr >= 1991)

bgcol <- pal(5)[1]

anim <- ggplot(toplo, aes(x = doy, colour = yr)) + 
  geom_line(aes(y = chl), size = 1) + 
  theme_bw(base_family = 'serif', base_size = 18) + 
  theme(
    legend.position = 'right', 
    legend.title = element_blank(), 
    axis.text.x= element_text(size = 8),
    plot.background = element_rect(fill= bgcol, 
                                   colour = bgcol),
    panel.background = element_rect(fill=bgcol, 
                                    colour = bgcol), 
    legend.background = element_rect(fill = bgcol, 
                                     colour = bgcol), 
    strip.background = element_blank()
  ) + 
  scale_colour_continuous_sequential(palette = colpal, rev = T, begin = 0.2) +
  guides(colour = guide_colourbar(barheight = 20, barwidth = 1)) +
  facet_grid(modi ~ station) +
  labs(x = 'Day of year', title ='Year: {frame_time}', y = ylabs) + 
  transition_time(yr) + 
  ease_aes('linear')

# save as png
animate(
  anim,
  renderer = file_renderer('fig', prefix = 'yrplo', overwrite = T), 
  width = 11, height = 6.5, res = 200, units = 'in',
  device = 'png', 
  nframes = 90
)

# convert png to pdf
fls <- list.files('fig', pattern = '^yrplo.*\\.png', full.names = T)
for(fl in fls){
  cat(fl, '\n')
  tmp <- rasterGrob(readPNG(fl, native = FALSE))
  pdf(gsub('\\.png', '\\.pdf', fl), width = 11, height = 6.5)
  grid.arrange(tmp)
  dev.off()
}

# combine files in a single pdf
fls <- list.files('fig', pattern = '^yrplo.*\\.pdf', full.names = T)
pdftools::pdf_combine(fls, output = 'fig/anitst.pdf')

# clean up
file.remove(fls)
file.remove(gsub('\\.pdf', '\\.png', fls))
