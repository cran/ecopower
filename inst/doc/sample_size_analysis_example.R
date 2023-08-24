## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_knit$set(global.par = TRUE)

## ---- warning=FALSE,message=FALSE---------------------------------------------
#to install latest version of ecopower on github:
#devtools::install_github("BenMaslen/ecopower")

#load libraries
library(ecopower)
library(mvabund)
library(ecoCopula)
library(ggplot2)
library(RColorBrewer)

## -----------------------------------------------------------------------------
#read in data
data("fish")

#create mvabund abundance matrix
fish_data <- fish
abund <- mvabund(fish_data[, 1:34])

#set Site.Type as a factor
fish_data$Site.Type <- factor(fish_data$Site.Type, levels = c("control", "restored", "reference"))

## ---- fig.height=5, fig.width=7-----------------------------------------------
#fit the model
fit       <- manyglm(abund ~ Site.Type, family = "negative.binomial", data = fish_data)
fit.cord  <- cord(fit)

#check model assumptions
plot(fit)

## -----------------------------------------------------------------------------
#specify 'increaser' and 'decreaser' species:
increasers <- c("Aplodactylus.lophodon", "Atypichthys.strigatus", "Cheilodactylus.fuscus",
                "Olisthops.cyanomelas", "Pictilabrius.laticlavius")
decreasers <- c("Abudefduf.sp", "Acanthurus.nigrofuscus", "Chromis.hypsilepis",
                "Naso.unicornis", "Parma.microlepis", "Parupeneus.signatus",
                "Pempheris.compressa", "Scorpis.lineolatus", "Trachinops.taeniatus")

#specify effect size of interest
coeff.alt <- effect_alt(fit, effect_size = 1.2, increasers, decreasers, term = "Site.Type")

## -----------------------------------------------------------------------------
fish_data_sim            <- fish_data
fish_data_sim$Site.Type[fish_data_sim$Site.Type=="reference"] <- "restored"
fish_data_sim$Site.Type  <- factor(fish_data_sim$Site.Type)

## -----------------------------------------------------------------------------
powersim(fit.cord, N=100, coeff.alt, term="Site.Type", nsim=50, ncrit=49, newdata = fish_data_sim, ncores=2)

## ----eval=FALSE---------------------------------------------------------------
#  #specify sample sizes to simulate under
#  sample_sizes    <- c(10, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300)
#  power_estimates <- rep(NA, length(sample_sizes))
#  
#  #loop through sample sizes and estimate power at each step
#  for (i in 1:length(sample_sizes)){
#    power_estimates[i] <- powersim(fit.cord, N = sample_sizes[i], coeff.alt, term = "Site.Type",
#                                   nsim = 1000, ncrit = 4999, newdata = fish_data_sim)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  #store the results in a data.frame
#  powercurve_dat <-  data.frame(sample_sizes = sample_sizes, Power = unlist(power_estimates))
#  
#  #plot power curves
#  ggplot(powercurve_dat, aes(x = sample_sizes, y = Power)) +
#    geom_line(size = 1.06) + theme_bw() + geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size=1) +
#    xlab("N") + ylab("Power") + scale_x_continuous(breaks = sample_sizes) +
#    scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0))

## ---- echo=FALSE, out.width="95%"---------------------------------------------
knitr::include_graphics("figure_1.jpg")

## ----eval=FALSE---------------------------------------------------------------
#  #specify effect sizes and sample sizes to loop through
#  sample_sizes <- c(3:12)*2
#  effect_sizes <- c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2)
#  mult_power_estimates <- rep(NA,length(sample_sizes)*length(effect_sizes))
#  
#  #loop through sample sizes and effect sizes
#  for (j in 1:length(effect_sizes)){
#    coeff.alt <- effect_alt(fit, effect_size = effect_sizes[j], increasers, decreasers, term = "Site.Type")
#    for (i in 1:length(sample_sizes)){
#      try(mult_power_estimates[10*(j-1)+i] <- powersim(fit.cord, N = sample_sizes[i], coeff.alt, term = "Site.Type", nsim = 1000,
#                                                       ncrit = 4999, newdata = fish_data_sim, nlv = 1, n.samp = 500))
#    }
#  }

## ----eval=FALSE---------------------------------------------------------------
#  #store the results in a data.frame
#  mult_powercurve_dat <- data.frame(sample_sizes = rep(sample_sizes, times = length(effect_sizes)),
#                                    power_estimates = unlist(mult_power_estimates),
#                                    Perc_Change = factor(rep(c("10%", "20%", "30%", "40%", "50%", "60%",
#                                                               "70%", "80%", "90%", "100%"), each = 10),
#                                                       levels = c("100%", "90%", "80%", "70%", "60%", "50%",
#                                                                "40%", "30%", "20%", "10%")),
#                                    effect_sizes = rep(effect_sizes, each = length(sample_sizes)))
#  
#  
#  
#  #plot multiple power curve
#  ggplot(mult_powercurve_dat, aes(x = sample_sizes, y = power_estimates, colour = Perc_Change)) +
#    geom_line(linewidth = 1.05) + theme_bw() + geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
#    xlab("N") + ylab("Power") + scale_x_continuous(breaks = sample_sizes) +
#    scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)) + scale_color_brewer(palette = "Paired", direction = -1, name = "% change")

## ---- echo=FALSE, out.width="95%"---------------------------------------------
knitr::include_graphics("figure_2.jpg")

## ----eval=FALSE---------------------------------------------------------------
#  #new cord model
#  fit.cord_doub_theta <- cord(fit)
#  
#  #change theta to be doubled
#  fit.cord_doub_theta$obj$theta <- fit.cord$obj$theta*2

## ----eval=FALSE---------------------------------------------------------------
#  increasers_no_abund <- c("Aplodactylus.lophodon", "Cheilodactylus.fuscus",
#                           "Olisthops.cyanomelas", "Pictilabrius.laticlavius" )

## ----eval=FALSE---------------------------------------------------------------
#  
#  coeff.alt_no_abund  <- effect_alt(fit, effect_size = 1.2, increasers_no_abund, decreasers, term = "Site.Type")
#  coeff.alt           <- effect_alt(fit, effect_size = 1.2, increasers, decreasers, term = "Site.Type")

## ----eval=FALSE---------------------------------------------------------------
#  sample_sizes <- c(10, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300)
#  power_estimatesa <- rep(NA,length(sample_sizes))
#  power_estimatesb <- rep(NA,length(sample_sizes))
#  
#  for (i in 1:length(sample_sizes)){
#    power_estimatesa[i] <- powersim(fit.cord, N = sample_sizes[i], coeff.alt_no_abund, term = "Site.Type",
#                                    nsim = 1000, ncrit = 4999, newdata = fish_data_sim)
#    power_estimatesb[i] <- powersim(fit.cord_doub_theta, N = sample_sizes[i], coeff.alt, term = "Site.Type",
#                                    nsim = 1000, ncrit = 4999, newdata = fish_data_sim)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  powercurve_experiment_dat <- data.frame(sample_sizes = rep(sample_sizes, 3),
#                                          Method = factor(rep(c("Original", "No abund", "Double theta"), each = 13),
#                                                          levels = c("Double theta", "Original", "No abund")),
#                                                Power = c(unlist(c(power_estimates, power_estimatesa, power_estimatesb))))
#  
#  
#  #plot power curve
#  ggplot(powercurve_experiment_dat, aes(x = sample_sizes, y = Power, colour = Method)) +
#    geom_line(size = 1.1) + theme_bw() + geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
#    xlab("N") + ylab("Power") + scale_x_continuous(breaks = sample_sizes) + theme(legend.text.align = 0) +
#    scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1.0)) +
#    scale_colour_manual(
#      values = c("#D55E00", "#56B4E9", "#009E73"),
#      labels = c("Halve overdispersion", "Original", expression(paste("No " , italic(A.strigatus))))
#    )

## ---- echo=FALSE, out.width="95%"---------------------------------------------
knitr::include_graphics("figure_3.jpg")

