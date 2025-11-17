quantile(sig_count$TS, probs = 0.95)

acstudy_test <- sampleDetector(acstudy_filt, n=10)  
drop_Sp = c('77','177','277','377','49','59','61','65')
# drop_Sp = c('49','59','61','65') #remove only BWs


# Using all available data
banter_testdata <- export_banter(acstudy_filt, training=FALSE, dropSpecies = drop_Sp) 


## Count all the signals per event ####
banter_list <- list(banter_pred=banter_testdata)

counts_signals <- list()

# Set up blank df
for (key in names(banter_list)){
  
  banter_obj <- banter_list[[key]]
  
  # Set up placeholder df for detector counts
  EventCounts <- banter_obj$events %>% 
    group_by(event.id) %>% 
    mutate(NClicks=0,NWhistle=0,NCep=0) %>% 
    select(event.id,NClicks,NWhistle,NCep)
  
  # Make sure the detectors are correctly indexed- check order in banter_obj
  # get clicks
  click_dets<-dplyr::bind_rows(banter_obj$detectors[1:6])
  click_counts <- click_dets %>% 
    reframe(UClicks=unique(call.id), .by = event.id) %>% 
    reframe(NClicks=n(), .by = event.id) %>% 
    mutate(NWhistle=0,NCep=0)
  
  # get whistles
  dw_counts <- dplyr::bind_rows(banter_obj$detectors[8]) %>% 
    reframe(UWhistle=unique(call.id), .by = event.id) %>%
    reframe(NWhistle=n(), .by = event.id) %>% 
    mutate(NClicks=0,NCep=0)
  
  # get cepstrum
  cep_counts <- dplyr::bind_rows(banter_obj$detectors[7]) %>% 
    reframe(UCep=unique(call.id), .by = event.id) %>%
    reframe(NCep=n(), .by = event.id) %>% 
    mutate(NClicks=0,NWhistle=0)
  
  click_totals <- dplyr::union_all(inner_join(EventCounts["event.id"], click_counts ), anti_join(EventCounts, click_counts["event.id"] )) 
  click_totals <- click_totals %>%
    arrange(desc(event.id))
  dw_totals <- union_all(inner_join(EventCounts["event.id"], dw_counts ), anti_join(EventCounts, dw_counts["event.id"] )) 
  dw_totals <- dw_totals %>%
    arrange(desc(event.id))
  cep_totals <- union_all(inner_join(EventCounts["event.id"], cep_counts ), anti_join(EventCounts, cep_counts["event.id"] ))
  cep_totals <- cep_totals %>%
    arrange(desc(event.id))
  
  all_signal_counts <- cbind.data.frame(click_totals, dw_totals, cep_totals)
  all_signal_counts <- subset(all_signal_counts, select = c(1,2,6,10))
  all_signal_counts <- all_signal_counts %>% 
    mutate(TS = sum(NClicks,NWhistle,NCep), .by = event.id) # sum for Total Signals
  
  counts_signals[[key]] <- all_signal_counts #Save all counts by banter obj
}

counts_signalsDF <- data.frame(do.call(rbind, counts_signals), row.names=NULL)




## Run classifications steps on 'median sized' test data ####

origSpCodes <- data.frame(banter_testdata[["events"]])

## Change 'other' species code to 577
banter_testdata <- new_sp_id(banter_testdata, "577")

## Run predictions, save data
pred_results <- banter::predict(bant_model, banter_testdata)

pred_resultsDF <- as.data.frame(pred_results[["predict.df"]])

pred_resultsDF <- pred_resultsDF[, c(1, 5, 2:4,6)]

pred_resultsDF <- inner_join(origSpCodes, pred_resultsDF, by = 'event.id')

## Compute confusion matrix
confMat <- caret::confusionMatrix(factor(pred_resultsDF$predicted),
                                  factor(pred_resultsDF$original), 
                                  mode = 'everything', 
                                  positive = 'X33')




# Precision-Recall Steps ####

## Change pr_data2 depending on the trial ##

pr_data2 <- pred_resultsDF # includes all signals
# pr_data2 <- df_sa_10sigs # includes all events with > 10 signals
# pr_data2 <- df_ssc2 # includes all events that meet SSC


true_labels <- factor(pr_data2$original, levels= c("X33", "X577")) 
prob_33 <- pr_data2$X33 
prevalence = sum(true_labels=='X33')/ (sum(true_labels=='X33') +sum(true_labels=='X577'))


pr_obj <- pr.curve(scores.class0 = prob_33[true_labels == "X33"],
                   scores.class1 = prob_33[true_labels == "X577"],
                   curve = TRUE)

pr_data <- data.frame(Recall = pr_obj$curve[,1],
                      Precision = pr_obj$curve[,2],
                      Threshold = pr_obj$curve[,3],
                      AUPRC = pr_obj$auc.integral,
                      prev = prevalence) %>% filter(Recall > 0)

pr_data$f1 <- 2 * pr_data$Precision * pr_data$Recall / (pr_data$Precision+pr_data$Recall)

f1_pt <- pr_data[which.max(pr_data$f1), ]   



### Plot P-R curve ####

pr_plot <- ggplot(data = pr_data, aes(x = Recall, y = Precision)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = prevalence, linetype = "dashed", linewidth=1.2, color = "darkgray") +
  geom_point(x=f1_pt$Recall, y=f1_pt$Precision, color = 'red') +
  labs(title = "Precision-Recall Curves",
       subtitle = paste("Test data with all signals (median). Baseline Prevalence:", round(prevalence,3), ', F-1=', round(f1_pt$f1, 2)),
       x = "Recall (Sensitivity)",
       y = "Precision") +
  theme_bw()

pr_plot

ggsave( file= file.path('output', paste0('prec_rec_ssc_', Sys.Date() , '.png') ), plot = pr_plot, width = 10, height = 6, dpi = 300 ) 


# Signal# - Remove events with too few signals ####

df_sa_10sigs <- filter(counts_signalsDF, TS > 10)

df_sa_10sigs <- inner_join(df_sa_10sigs, pred_resultsDF, by = "event.id")


# SSC - Remove events that don't meet SSC ####

folder_path <- file.path('data', 'ssc')

df_ssc <- list.files(file.path('data', 'ssc'), pattern = "\\.csv$", full.names = TRUE) %>% # files of events to KEEP
map_dfr(~ read_csv(.x) %>% # automatically row-binds dataframes
          mutate(source_file = basename(.x))) %>% # adds file name
  rename(event.id = eventId)
df_ssc$event.id <- as.character(df_ssc$event.id)

#need to add in event.id from old data
id_new <- df_ssc$event.id
id_old <- pred_resultsDF[1:17,1]

id_all <- c(id_new, id_old)

df_ssc2 <- pred_resultsDF %>% 
  filter(event.id %in% id_all)


## Both ####

# Total up signals from df_sa_10sigs?
# Take 10sigs and save evennts that match id_all

df_sa_ssc <- df_sa_10sigs %>% 
  filter(event.id %in% id_all)

intersect(df_ssc2$event.id, df_sa_ssc$event.id)
setdiff(df_sa_ssc$event.id, df_ssc2$event.id)

