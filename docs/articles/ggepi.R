## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.width = 7,
    fig.height = 7,
    fig.align = "center",
    warning = FALSE
    )
library(ggplot2)
library(ggepi)

## ------------------------------------------------------------------------
library(broom)
fit <- lm(Fertility ~ 0 + Catholic + Agriculture + Examination + Education + Infant.Mortality, data = swiss)
fit <- tidy(fit, conf.int = TRUE)

## ------------------------------------------------------------------------
view_estci(fit)

## ------------------------------------------------------------------------
ggplot(fit, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
    geom_estci()

## ------------------------------------------------------------------------
fit2 <- transform(fit, p.value = discrete_pvalue(fit$p.value))

p <- ggplot(fit2, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high,
                 alpha = p.value, size = p.value)) 
p + geom_estci(fatten = 1, center.linetype = "dotted")

## ------------------------------------------------------------------------
p + geom_estci(fatten = 1, center.linetype = "dotted", height = 0.5, linetype = "dashed")

