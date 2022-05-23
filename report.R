## Prepare plots and tables for report

## Before: sofia20_proportions.csv (bootstrap/data), results.rds (model),
##         current_status.csv, stock_timeseries.csv (output)
## After:  bbmsy.png, cpue_1.png, driors_1.png, posterior_1.png,
##         status_by_year.png, status_sofia.png, status_sraplus.png,
##         stock_posterior.pdf, stock_timeseries.pdf (report)

library(TAF)
library(SOFIA)
suppressMessages(library(dplyr)) # mutate
suppressMessages(library(egg))   # ggarrange
library(ggplot2)
suppressMessages(library(purrr)) # map2, walk2
library(sraplus) # plot_driors, plot_prior_posterior, plot_sraplus

mkdir("report")

stocks <- readRDS("model/results.rds")

## Plot CPUE
pdf("report/stock_cpue.pdf")
for(i in seq_len(nrow(stocks)))
{
  x <- stocks$driors[[i]]$effort_years
  y <- with(stocks$driors[[i]], catch[years %in% x] / effort)
  plot(x, y, ylim=lim(y), main=stocks$stock[i], xlab="", ylab="CPUE", type="l")
}
dev.off()

## Barplots of stock status
taf.png("status_sraplus")
current_status <- read.taf("output/current_status.csv")
current_status$status <- ordered(current_status$status,
                                 c("Underfished","Fully fished","Overfished"))
barplot(prop.table(table(current_status$status)), col=c(3,7,2), ylim=0:1,
        xlab="Category", ylab="Proportion")
dev.off()
taf.png("status_sofia")
results_sofia <- read.taf("bootstrap/data/sofia20_proportions.csv")
results_sofia$Category <- ordered(results_sofia$Category,
                                  c("Underfished","Fully fished","Overfished"))
barplot(Proportion~Category, results_sofia, col=c(3,7,2), ylim=0:1)
dev.off()

## Plot posteriors and time series for each stock
stocks <- stocks %>%
  mutate(plot_prior_posterior_plot=
           map2(sraplus_fit, driors, plot_prior_posterior))
savefoo <- function(stock, plot) print(plot + labs(title=stock))
pdf("report/stock_posterior.pdf")
walk2(stocks$stock, stocks$plot_prior_posterior_plot, savefoo)
dev.off()
stocks <- stocks %>%
  mutate(sraplus_fit_plot = map(sraplus_fit, plot_sraplus))
savefoo <- function(stock, plot) print(plot + labs(title=stock))
pdf("report/stock_timeseries.pdf")
walk2(stocks$stock, stocks$sraplus_fit_plot, savefoo)
dev.off()

## Plot time series for each stock
newResTab <- read.taf("output/stock_timeseries.csv")
taf.png("status_by_year")
p1 <- plotCat(newResTab, method="effEdepP", cats=3, type="count")
p2 <- plotCat(newResTab, method="effEdepP", cats=3, type="stock")
ggarrange(p1, p2, ncol=1)
dev.off()

## Overlay B/Bmsy time series of all stocks in a single plot
ggplot(newResTab, aes(x=year, y=bbmsy, colour=stock, group=stock)) +
  geom_line(show.legend=TRUE) +
  geom_hline(yintercept=0.8, linetype="dashed", color="red", size=2) +
  geom_hline(yintercept=1.2, linetype="dashed", color="green", size=2)
ggsave("report/bbmsy.png")

## Overlay B/Bmsy time series of all stocks in a single plot without legend
ggplot(newResTab, aes(x=year, y=bbmsy, colour=stock, group=stock)) +
  geom_line(show.legend = FALSE) +
  geom_hline(yintercept=0.8, linetype="dashed", color = "red", size=2) +
  geom_hline(yintercept=1.2, linetype="dashed", color = "green", size=2)
ggsave("report/bbmsywo.png")
