# Script to calculate and visual medians for signal types per acoustic events
# Yvonne Barkley
# October 27, 2025



# find median number of signals per event in the prediction data set

sig_count <- read.csv(file.path('output', 'CountsSApreds_20251010.csv'))

mean(sig_count$TS)
max(sig_count$TS)

sig_summ <- sig_count %>% summarise(mean_ec = mean(NClicks),
                        mean_dw = mean(NWhistle),
                        mean_cep = mean(NCep),
                        mean_ts = mean(TS),
                        med_ec = median(NClicks),
                        med_dw = median(NWhistle),
                        med_cep = median(NCep),
                        med_ts = median(TS) )
