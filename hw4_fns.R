### define some functions--------------------------------------------------------
plot_prob_dens <- function(testProbs_obj, subtitle) {
  
  ggplot(testProbs_obj, aes(x = probs, fill = as.factor(outcome))) +
          geom_density(alpha = 0.7) +
          facet_grid(outcome ~ .) +
          xlim(0, 1) +
          labs(x = "Response", y = "Density of probabilities",
               title = "Distribution of predicted probabilities by observed outcome",
               subtitle = subtitle) +
          plotTheme() + theme(strip.text.x = element_text(size = 18),
                              legend.position = "none")
  
}  

plot_gof <- function(cvFit_obj, model_type){
  
    title <- paste0("CV Goodness of Fit Metrics: ", model_type)
  
    dplyr::select(cvFit_obj$resample, -Resample) %>%
      gather(metric, value) %>%
      left_join(gather(cvFit_obj$results[2:4], metric, mean)) %>%
      ggplot(aes(value)) +
      geom_histogram(bins=35, fill = "#FF006A") +
      facet_wrap(~metric) +
      geom_vline(aes(xintercept = mean), colour = "#981FAC", linetype = 3, size = 1.5) +
      scale_x_continuous(limits = c(0, 1)) +
      labs(x="Goodness of Fit", y="Count", 
           title= title,
           subtitle = "Across-fold mean reprented as dotted lines") +
      plotTheme()
  
}

roc_plot <- function(testProbs_obj, model_type){
  
  title <- paste0("ROC Curve: ", model_type)
  
  auc <- round(pROC::auc(testProbs_obj$outcome, testProbs_obj$probs), 3)
  roc_subtitle = paste0("AUC: ", auc)
    
    ggplot(testProbs_obj, aes(d = as.numeric(outcome), m = probs)) +
      geom_roc(n.cuts = 50, labels = FALSE, colour = "#FE9900") +
      style_roc(theme = theme_grey) +
      geom_abline(slope = 1, intercept = 0, size = 1.5, color = 'grey') +
      labs(title = title,
           subtitle = roc_subtitle)
  
}


print_cb_tab <- function(testProbs_obj, caption){
  
  cost_benefit_table <-
    testProbs_obj %>%
    count(pred_outcome, outcome) %>%
    summarize(True_Negative = sum(n[pred_outcome==0 & outcome==0]),
              True_Positive = sum(n[pred_outcome==1 & outcome==1]),
              False_Negative = sum(n[pred_outcome==0 & outcome==1]),
              False_Positive = sum(n[pred_outcome==1 & outcome==0])) %>%
    gather(Variable, Count) %>%
    mutate(Revenue =
             case_when(Variable == "True_Negative"  ~ 0,
                       Variable == "True_Positive"  ~ 12400 * Count,
                       Variable == "False_Negative" ~ 0,
                       Variable == "False_Positive" ~  Count * -2850)) %>%
    bind_cols(data.frame(Description = c(
      "Predicted correctly homeowner would not enter the credit program, no marketing resources were allocated, and no credit was allocated.",
      "Predicted correctly homeowner would enter credit program; allocated the marketing resources, and 25% ultimately achieved the credit",
      "We predicted that a homeowner would not enter the credit program but they did.",
      "Predicted incorrectly that a homeowner would enter the credit program.")))
  
  cost_benefit_table %>%
    kbl(caption = caption) %>%
    kable_minimal()
  
}

make_thresh <- function(testProbs_obj){
  
  whichThreshold <-
    iterateThresholds(
      data=testProbs_obj, observedClass = outcome, predictedProbs = probs) %>%
    dplyr::select(starts_with("Count"), Threshold) %>%
    gather(Variable, Count, -Threshold) %>%
    mutate(Revenue =
             case_when(Variable == "Count_TN"  ~ 0,
                       Variable == "Count_TP"  ~ 12400 * Count,
                       Variable == "Count_FN"  ~ 0,
                       Variable == "Count_FP"  ~ Count * -2850))
  
}

print_thresh_tab <- function(whichThreshold_obj, model_type){
  
  whichThreshold_obj %>%
      ggplot(.,aes(Threshold, Revenue, colour = Variable)) +
      geom_point() +
      labs(title = "Revenue by confusion matrix type and threshold",
           y = "Revenue",
           subtitle = model_type) +
      plotTheme() +
      guides(colour=guide_legend(title = "Confusion Matrix"))
  
}

plot_count <- function(whichThreshold_obj, model_type) {
  
  whichThreshold_count <- 
    whichThreshold_obj %>% 
    group_by(Threshold) %>% 
    filter(Variable == "Count_TP") %>%
    summarize(total_credits = (sum(Count))* 5000 * 0.25)
  
  title1 <- paste0("Total Credits Applied By Threshold \nFor Test Sample: ", model_type)
  
  countplot <-  ggplot(whichThreshold_count)+
    geom_line(aes(x = Threshold, y = total_credits))+
    labs(title = title1)
}

plot_rev <- function(whichThreshold_obj, model_type) {
  
  whichThreshold_revenue <- 
    whichThreshold_obj %>% 
    group_by(Threshold) %>% 
    summarize(Revenue = sum(Revenue))
  
  max_revenue <- paste0("Maximum Revenue: ", dollar_format()(max(whichThreshold_revenue$Revenue)))
  
  max_revenue_data <- whichThreshold_revenue[whichThreshold_revenue$Revenue == max(whichThreshold_revenue$Revenue), ]
  
  title2 <- paste0("Model Revenues By Threshold For Test Sample: ", model_type)
  
  revenueplot <- ggplot(whichThreshold_revenue) +
    geom_line(aes(x = Threshold, y = Revenue)) +
    geom_vline(xintercept = pull(arrange(whichThreshold_revenue, -Revenue)[1,1]), linetype="dashed") +
    geom_text(aes(x = max_revenue_data$Threshold, y = max_revenue_data$Revenue, label = max_revenue), vjust = 1, hjust = -0.25) +
    labs(title = title2,
         subtitle = "Vertical Line Denotes Optimal Threshold")
  
}