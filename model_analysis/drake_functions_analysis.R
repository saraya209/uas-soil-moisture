# Model selector and tune

resampleListFunction <- function(trainset.lst, mydata, train = TRUE, rem.cols = TRUE){
  if (train){
    sub.dt <- left_join(trainset.lst, mydata, 
                        by = c( "Transect", "wyd"))
  }else {
    sub.dt <- anti_join(mydata, trainset.lst, 
                        by = c( "Transect", "wyd"))
  }
  if (rem.cols){
    # Remove non predictor columns:
    sub.dt <- subset(sub.dt, select= -c(Transect, Distance, wyd))
  }
  return(sub.dt)
}
# Model Performance Functions
mean_absolute_error <- function(observed, predicted, na.rm = TRUE){
  mean(abs(observed - predicted), na.rm = TRUE)
}

mean_bias_error <- function(observed, predicted, na.rm = TRUE){
  mean(predicted - observed, na.rm = TRUE)
}

coef_determination <- function(observed, predicted, na.rm = TRUE){
  obs_mean <- mean(observed, na.rm = TRUE)
  sse <- sum((observed - predicted)^2, na.rm = TRUE)
  ssto <- sum((observed - obs_mean)^2, na.rm = TRUE)
  r2 <- 1 - (sse/ssto) 
  
  return(r2)
}

# Residual and MAE plot 
perf_box <- function(full.data, summ.data){
  # values for plot
  max.whisker  <- ceiling(max(summ.data$res_iqr+summ.data$res_q3)*1.5)
  avg.mae = mean(summ.data$mae)
  
  # Box plot
  p.box <- ggplot(data = full.data, 
                  aes(x = forcats::fct_reorder(as.factor(label), mae, .fun=mean),
                      y = abs(diff)))+
    geom_boxplot(outlier.shape = NA)+
    #geom_jitter(shape=16, position=position_jitter(0.2))+
    geom_point(data = summ.data, aes(y = mae), 
               color = "red", size = 2)+
    geom_text(data = summ.data, hjust="inward",
              aes(y = max.whisker, label = r2.fmt))+
    geom_text(data = summ.data,#hjust="inward", 
              aes(y = -1, label = n) )+
    geom_hline(data = summ.data, yintercept = avg.mae)+
    labs(title = "Distribution of Residuals",
         subtitle = paste0("Models with different training-testing split",
                          "\nAverage MAE: ", round(avg.mae,2) ),
         caption = paste("Values on left and right: N and R-squared.",
                         "Red dots = MAE",
                         "Vertical line = Mean MAE",
                         sep = "\n"),
         y = "|Residual|",
         x = "")+
    theme(axis.text.y = element_text(size=rel(1.15)),
          plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
    theme_bw()+
    coord_flip(ylim = c(-1,max.whisker))
  
  return(p.box)
  
}
perf_box_pty <- function(full.data, summ.data){
  # values for plot
  max.whisker  <- ceiling(max(summ.data$res_iqr+summ.data$res_q3))
  avg.mae = mean(summ.data$mae)
  
  # Box plot
  p.box <- ggplot(data = full.data, 
                  aes(x = forcats::fct_reorder(as.factor(label), mae, .fun=mean),
                      y = abs(diff)))+
    geom_boxplot(outlier.shape = NA)+
    stat_summary(fun.y = mean, colour="black", geom="point", size=3.25)+
    geom_hline(aes(yintercept = mean(mae)))+
    
    # geom_point(data = summ.data, aes(y = mae), 
    #            shape = 1, size = 3)+
    geom_text(data = summ.data,#hjust="inward", 
              aes(y = -1, label = n) )+
    # geom_hline(data = summ.data, yintercept = avg.mae)+
    labs(#title = "Distribution of Residuals",
         subtitle = paste0("Average MAE: ", round(avg.mae,2) ),
         y = "Absolute Residuals (% VWC)",
         x = "")+
    theme_bw()+
    theme(axis.text.y = element_text(size=rel(1.15)),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
    coord_flip(ylim = c(-1,max.whisker))
  
  return(p.box)
  
}


# Subset models by testing set split
best_models <- function(summ.data, nmodels = 4){
  # filter nmodels models with lowest selpara
  summ.data <- summ.data %>% 
    dplyr::arrange(desc(mae)) %>% 
    dplyr::top_n(-nmodels, mae)# slice(1:nmodels)
  
  best.models <- unique(summ.data$label)
  
  return(best.models)
}

# Measured vs. predicted plot 
one_to_one <- function(full.data, summ.data, best.models, num_cols = 2){
  # filter npanel models with lowest MAE
  summ.data <- summ.data %>% 
    dplyr::filter(label %in% best.models)
  
  full.data <- full.data %>% 
    dplyr::filter(label %in% best.models)%>%
    dplyr::mutate(label_ordered = factor(label, ordered = TRUE, levels = levels(summ.data$label_ordered)))
  
  # values for plot
  p.max = ceiling(max(full.data$observed, full.data$predicted))
  p.min = floor(min(full.data$observed, full.data$predicted))
  
  # Plot
  mp = ggplot() +  
    geom_point(data = full.data, 
               aes(y = VWC, x = predicted, fill = as.factor(wyd)),
               alpha = 0.85, size = 2, shape = 21) +
    geom_abline(intercept = 0, slope = 1, linetype = 2 ) + 
    # geom_text(data = summ.data ,
    #           aes(x = p.min, y = p.max,
    #               label = paste("MAE = ", round(mae, digits = 3), "\n",
    #                             expression(italic(R)^2),"=" , round(r2.fmt, digits = 1),
    #                             "\nMBE = ", round(mbe, digits = 3)
    #               )),
    #           vjust = "inward", hjust = "inward", parse = F) +
    geom_text(data = summ.data ,
              aes(x = p.min, y = p.max,
                  label = mae_txt
                  ),
              vjust = "inward", hjust = "inward", parse = T) +
    geom_text(data = summ.data ,
              aes(x = p.min, y = p.max-5,
                  label = mbe_txt
              ),
              vjust = "inward", hjust = "inward", parse = T) +
    geom_text(data = summ.data ,
              aes(x = p.min, y = p.max-10,
                  label = r2_txt
              ),
              vjust = "inward", hjust = "inward", parse = T) +
    labs(#title = "Best performing training-testing split",
         #caption = paste0("N = ", prettyNum(N,big.mark = ",")),
         x = "Predicted (% VWC)",
         y = "Measured (% VWC)",
         fill = "Water year day")+
    coord_fixed(ratio = 1, xlim = c(p.min,p.max), ylim = c(p.min,p.max))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+ 
    facet_wrap(~label_ordered, ncol = num_cols)+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"),
          legend.position="top") +
    scale_fill_viridis_d()
  
  return(mp)
}

one_to_one_bw <- function(full.data, summ.data, best.models, num_cols = 2){
  # filter npanel models with lowest MAE
  summ.data <- summ.data %>% 
    dplyr::filter(label %in% best.models)
  
  full.data <- full.data %>% 
    dplyr::filter(label %in% best.models)%>%
    dplyr::mutate(label_ordered = factor(label, ordered = TRUE, levels = levels(summ.data$label_ordered)))
  
  # values for plot
  p.max = ceiling(max(full.data$observed, full.data$predicted))
  p.min = floor(min(full.data$observed, full.data$predicted))
  
  # Plot
  mp = ggplot() +  
    geom_point(data = full.data, 
               aes(y = VWC, x = predicted, shape = as.factor(wyd)) ) +
    geom_abline(intercept = 0, slope = 1, linetype = 2 ) + 
    
    geom_text(data = summ.data ,
              aes(x = p.min, y = p.max,
                  label = mae_txt
              ),
              vjust = "inward", hjust = "inward", parse = T) +
    geom_text(data = summ.data ,
              aes(x = p.min, y = p.max-5,
                  label = mbe_txt
              ),
              vjust = "inward", hjust = "inward", parse = T) +
    geom_text(data = summ.data ,
              aes(x = p.min, y = p.max-10,
                  label = r2_txt
              ),
              vjust = "inward", hjust = "inward", parse = T) +
    labs(x = "Predicted (% VWC)",
         y = "Measured (% VWC)",
         shape = "Water year day")+
    coord_fixed(ratio = 1, xlim = c(p.min,p.max), ylim = c(p.min,p.max))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+ 
    facet_wrap(~label_ordered, ncol = num_cols)+
    theme_bw()+
    theme(panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"),
          legend.position="top")
  
  return(mp)
}

one_to_one_combined <- function(full.data, summ.data){
  
  # values for plot
  p.max = ceiling(max(full.data$observed, full.data$predicted))
  p.min = 0
  
  # Plot
  mp = ggplot() +  
    geom_point(data = full.data, 
               aes(y = VWC, x = predicted, 
                   shape = as.factor(wyd),
                   color = label_ordered) ) +
    geom_abline(intercept = 0, slope = 1, linetype = 2 ) + 
    
    geom_text(data = summ.data ,
              aes(x = p.min, y = p.max,
                  label = mae_txt
              ),
              vjust = "inward", hjust = "inward", parse = T) +
    geom_text(data = summ.data ,
              aes(x = p.min, y = p.max-5,
                  label = mbe_txt
              ),
              vjust = "inward", hjust = "inward", parse = T) +
    geom_text(data = summ.data ,
              aes(x = p.min, y = p.max-10,
                  label = r2_txt
              ),
              vjust = "inward", hjust = "inward", parse = T) +
    labs(x = "Predicted (% VWC)",
         y = "Measured (% VWC)",
         #caption = paste("N =", n),
         shape = "Water year day",
         color = "Model")+
    coord_fixed(ratio = 1, xlim = c(p.min,p.max), ylim = c(p.min,p.max))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+ 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+ 
    #facet_wrap(~label_ordered, ncol = num_cols)+
    theme_bw()+
    guides(color=guide_legend(ncol=4))+
    # theme(panel.grid.minor = element_blank(),
    #       strip.background = element_blank(),
    #       panel.border = element_rect(colour = "black"),
    #       legend.position="top")+
    theme(legend.text=element_text(size=rel(0.7)))+
    scale_color_viridis_d()
  
  return(mp)
}

# Variable importance plot
imp_plot <- function(imp.data, nvar = 10){
  # filter npanel models with lowest MAE
  # filter top nvar predictors and sort order
  imp.data <- imp.data %>%
    dplyr::top_n(nvar, importance)%>% #slice(1:nvar)
    dplyr::ungroup() %>% 
    # Arrange by
    #   i.  facet group
    #   ii. bar height
    dplyr::arrange(label, importance) %>% 
    # Add order column of row numbers
    dplyr::mutate(order = row_number())

  #Plot
  imp.p <- ggplot(imp.data, 
                  aes(x = order,#reorder(variable, importance), 
                      y = importance)) +
    geom_col(position = position_dodge(width = 0.85), 
             width = 0.7, color = "grey20", size = 0.25)+
    labs(x = NULL,
         y = "Relative Importance",
         title = "Best performing training-testing split") +
    theme_bw()+
    theme(axis.text.y = element_text(size=rel(1.25)),
          #axis.title = element_text(size = rel(1.25) , face = "bold"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position="none",
          #axis.ticks.y = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    #ylim(0, 60)+
    scale_x_continuous(breaks = imp.data$order,
                       labels = imp.data$variable#,
                       #expand = c(0,0)
                       )+
    scale_y_continuous(#breaks = scales::pretty_breaks(n = 6),
                       expand = c(0,1)) +
    facet_wrap(~label, scales = "free")+
    coord_flip()
  
  return(imp.p)
}

imp_plot_line <- function(imp.data, best.models, nvar = 25, grouped = FALSE){
  
  # filter best models filter top nvar predictors and sort order
  if (grouped){
    #order variables
    var_order = levels(reorder(imp.data$Variable_Type_Abr, -imp.data$importance_2))
    
    imp.data = imp.data %>% 
      dplyr::filter(label %in% best.models)%>%
      dplyr::group_by(label, Variable_Type_Abr)%>%
      dplyr::summarise(importance_2 = sum(importance_2),
                       Domain = unique(Domain))%>%
      dplyr::mutate(Abr_Name_ordered = factor(Variable_Type_Abr, levels = rev(var_order)))%>%
      dplyr::arrange(Abr_Name_ordered)
  }else{
    #order and subset first nvars
    var_order = levels(reorder(imp.data$Abr_Name, -imp.data$importance_2))[1:nvar]
    
    imp.data = imp.data %>% 
      dplyr::filter(label %in% best.models &
                      Abr_Name %in% var_order)%>%
      dplyr::mutate(Abr_Name_ordered = factor(Abr_Name, levels = rev(var_order)))%>%
      dplyr::arrange(Abr_Name_ordered)
  }
  
  #Plot
  imp.p = ggplot(imp.data, 
                  aes(y = importance_2, 
                      x = Abr_Name_ordered,
                      group = label,
                      linetype = label,
                      shape = label) )+
    geom_bar(stat = "identity", aes(y = Inf, fill = Domain))+
    geom_point()+
    geom_path()+
    labs(x = NULL,
         y = "Relative Importance",
         linetype = "Model",
         shape = "Model",
         fill = "Variable Group") +
    theme_bw()+
    theme(axis.text.y = element_text(size=rel(1.25)),
          #axis.title = element_text(size = rel(1.25) , face = "bold"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.text=element_text(size=rel(0.8)),
          legend.justification=c(1,0), 
          legend.position=c(1,0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
      expand = c(0,0.5)
      )+
    scale_fill_viridis_d(alpha = 0.25) +
    coord_flip()
  
  return(imp.p)
}

imp_plot_box <- function(imp.data, best.models, nvar = 25, grouped = "none"){
  
  # filter best models filter top nvar predictors and sort order
  if (grouped == "sum"){
    #order variables
    var_order = levels(reorder(imp.data$Variable_Type_Abr, -imp.data$importance_2))
    
    imp.data = imp.data %>% 
      #dplyr::filter(label %in% best.models)%>%
      dplyr::group_by(label, Variable_Type_Abr)%>%
      dplyr::summarise(importance_2 = sum(importance_2),
                       Domain = unique(Domain),
                       Abr_Name = unique(Variable_Type_Abr))
  }else if (grouped == "mean"){
    #order variables
    var_order = levels(reorder(imp.data$Variable_Type_Abr, -imp.data$importance_2))
    
    imp.data = imp.data %>% 
      #dplyr::filter(label %in% best.models)%>%
      dplyr::group_by(label, Variable_Type_Abr)%>%
      dplyr::summarise(importance_2 = mean(importance_2),
                       Domain = unique(Domain),
                       Abr_Name = unique(Variable_Type_Abr))
  }else{
    #order and subset first nvars
    var_order = levels(reorder(imp.data$Abr_Name, -imp.data$importance_2))[1:nvar]
    
    imp.data = imp.data %>% 
      dplyr::filter(#label %in% best.models &
                      Abr_Name %in% var_order)
  }
  
  #Plot
  imp.p = ggplot(imp.data, 
                 aes(y = importance_2, 
                     x = forcats::fct_reorder(as.factor(Abr_Name),importance_2, .fun=median),
                     group = Abr_Name) )+
    #geom_bar(stat = "identity", aes(y = Inf, fill = Domain))+
    geom_boxplot(outlier.shape = NA, aes(fill = Domain))+
    #stat_summary(fun.y = mean, colour="black", geom="point", size=2.5)+
    #geom_path()+
    labs(x = NULL,
         y = "Relative Importance",
         fill = "Variable Group") +
    theme_bw()+
    theme(axis.text.y = element_text(size=rel(1.25)),
          #axis.title = element_text(size = rel(1.25) , face = "bold"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.text=element_text(size=rel(0.8)),
          legend.justification=c(0.95,0.05), 
          legend.position=c(0.95,0.05)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                       expand = c(0,0.5)
    )+
    scale_fill_viridis_d(alpha = 0.75) +
    coord_flip()
  
  return(imp.p)
}

# Tunning summary table and plot
summ_tune = function(model_list, model_name, sort_order){
  # Extract $bestTune table from list
  tune.lst <- lapply(model_list, function(x) x$bestTune)
  tune.dt = bind_rows(tune.lst, .id = "id")
  tune.dt = tune.dt%>%
    dplyr::mutate(label = paste(model_name, id),
                  label_ordered = factor(label, ordered = TRUE, levels = sort_order))
  return(tune.dt)
}
# tune_table_summ <- function(sel_model, model_id){
#   tune.pars <- data.frame(Model = model_id, 
#                           sel_model$bestTune,
#                           stringsAsFactors = FALSE)
#   
#   return(tune.pars)
# 
# }
tune_plot_summ <- function(model_list, model_name, model_id){
  tune.plot <- plot(model_list, main = paste(model_name, model_id))
  return(tune.plot)
  
}

explain_wrap = function(model_in, data_in, label_in, y){
  DALEX::explain(model = model_in,
                 data = data_in,
                 y = data_in[[y]], # y as vector.
                 label = label_in)
}

summ_perf = function(model_list, test_list, model_name){
  # Create explainer for models
  explainer.lst = mapply(explain_wrap, # function
                         model_in = model_list, # list 1
                         data_in = test_list, # list2
                         label_in = paste(model_name, 1:length(model_list)),
                         y = "VWC",
                         SIMPLIFY = F)
  # Calculate performances 
  perf.lst = lapply(explainer.lst, DALEX::model_performance)
  # Convert lists to one data frame and combine with test data frame
  perf.dt.lst = lapply(perf.lst, data.frame)
  perf.dt = dplyr::bind_rows(perf.dt.lst, .id = "id")
  test.dt = dplyr::bind_rows(test_list, .id = "id")
  perf.dt = dplyr::bind_cols(test.dt, perf.dt)
  
  # Calculate performance variables
  # Remove -ve values, ADD RMSE for plot sorting
  perf.dt = perf.dt %>% 
    dplyr::group_by(label) %>% 
    dplyr::mutate(predicted = ifelse(predicted<0, 0, predicted),
                  rmse = loss_root_mean_square(observed, predicted),
                  mae = mean_absolute_error(observed, predicted))
  # Summary statistics
  perf.summ.dt = perf.dt %>% 
    dplyr::group_by(label) %>% 
    dplyr::summarise(rmse = loss_root_mean_square(observed, predicted),
                     mae = mean_absolute_error(observed, predicted),
                     mbe = mean_bias_error(observed, predicted),
                     
                     r2 = coef_determination(observed, predicted),
                     r2.fmt = ifelse(r2>0, round(r2, 2), 0),
                     n = n(),
                     res_q1 = quantile(diff)[2],
                     res_q3 = quantile(diff)[4],
                     res_iqr = IQR(diff),
                     
                     mae_txt = paste0("MAE==", round(mae, digits = 3)),
                     mbe_txt = paste0("MBE==", round(mbe, digits = 3)),
                     r2_txt = paste0("italic(R) ^ 2 == ", r2.fmt)
                     )%>%
    dplyr::mutate(label_ordered = forcats::fct_reorder(as.factor(label), mae, .fun=mean))
  # Order perf.dt
  perf.dt = perf.dt %>% 
    dplyr::mutate(label_ordered = 
                    factor(label,levels = levels(perf.summ.dt$label_ordered)))
  ## Ensemble performance
  # Summary statistics
  c.perf.summ.dt = perf.dt %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(rmse = loss_root_mean_square(observed, predicted),
                     mae = mean_absolute_error(observed, predicted),
                     mbe = mean_bias_error(observed, predicted),
                     
                     r2 = coef_determination(observed, predicted),
                     r2.fmt = ifelse(r2>0, round(r2, 2), 0),
                     n = n(),
                     res_q1 = quantile(diff)[2],
                     res_q3 = quantile(diff)[4],
                     res_iqr = IQR(diff),
                     
                     mae_txt = paste0("MAE==", round(mae, digits = 3)),
                     mbe_txt = paste0("MBE==", round(mbe, digits = 3)),
                     r2_txt = paste0("italic(R) ^ 2 == ", r2.fmt) )
  
  
  perfsumm_lst = list(perf_dt = perf.dt,
                      perfsumm_dt = perf.summ.dt,
                      perfens_dt = c.perf.summ.dt,
                      explain_lst = explainer.lst)
  return(perfsumm_lst)
}


summ_imp = function(model_list, model_name, desc_table, sort_order, explain_list){
  # Extract $finalModel table from list
  m.lst <- lapply(model_list, function(x) x$finalModel) 
  # Tesst if model specific importance exists:
  vtest = tryCatch(varImp(m.lst[[1]]),error=function(e) e, warning=function(w) w)
  if (is(vtest,"error") ){
    imp_method = "Permutation based variable importance calculated as loss from feature dropout."
    # Extract model independent importance
    imp.lst <- lapply(explain_list, ingredients::feature_importance)
    imp.dt = bind_rows(imp.lst, .id = "id")
    # Calculate importance percent
    imp.dt = imp.dt %>% 
      dplyr::mutate(label = paste(model_name, id),
                    label_ordered = factor(label, ordered = TRUE, levels = sort_order)
                    )%>%
      dplyr::group_by(label) %>% 
      dplyr::filter(variable != "_baseline_" & 
                      variable != "_full_model_" &
                      variable %in% desc_table$variable) %>%
      dplyr::mutate(importance = dropout_loss - min(dropout_loss),
                    importance_1 = 100*importance/max(importance),
                    importance_2 = 100*importance/sum(importance))
  }else{
    imp_method = "Model specific variable importance."
    # Extract model specific importance
    imp.lst = lapply(m.lst, varImp)
    imp.lst = lapply(imp.lst, tibble::rownames_to_column, var = "variable")
    imp.dt = bind_rows(imp.lst, .id = "id")
    imp.dt = imp.dt%>%
      dplyr::mutate(label = paste(model_name, id),
                    label_ordered = factor(label, ordered = TRUE, levels = sort_order))%>%
      dplyr::group_by(label) %>% 
      dplyr::mutate(importance = Overall,
                    importance_1 = 100*importance/max(importance),
                    importance_2 = 100*importance/sum(importance))
  }
  
  
  imp.dt = left_join(imp.dt, desc_table, by = "variable")
  out.lst = list(imp.dt, imp_method)
  return(out.lst)
}


model_id <- function(fname_path){
  as.numeric(str_match_all(fname_path, "_(.*?)_")[[1]][[2,2]] ) 
}


ale_fun <- function(explained){
  var_name = c("TPI_3_7_500", "TPI_3_7_300", "TPI_3_7_100", "TPI_1_3_500",
              "C_Pro_dsm_100", "C_Pro_dsm_300", "C_Pro_dsm_500", "C_Pro_dsm_5000",
              "A_MF_dsm_60", "A_MF_dsm_100",  "A_MF_dsm_300", "A_MF_dsm_500",
              "N_dsm_60", "N_dsm_100", "N_dsm_300", "N_dsm_500",
              "S_dsm_60", "S_dsm_100", "S_dsm_300", "S_dsm_500")
  ale.dt = NULL
  for (i in var_name){
    ale.dt.i <- single_variable(explained, 
                                variable = i,
                                type = "ale")
    ale.dt = bind_rows(ale.dt, ale.dt.i)
  }
  
  return(ale.dt)
}

ale_summ <- function(ale_lst, pp_tbl, desc_tbl){
  p_order = c("Acc (60)", "Acc (100)", "Acc (300)", "Acc (500)",
              "N (60)", "N (100)", "N (300)", "N (500)",
              "Cur (Pro-100)", "Cur (Pro-300)", "Cur (Pro-500)", "Cur (Pro-5000)",
              "Slo (60)", "Slo (100)", "Slo (300)", "Slo (500)",
              "TPI (3,7)", "TPI (5,15)", "TPI (9,21)", "TPI (15,35)")
  
  p_names = c("Acc (0.6)", "Acc (1)", "Acc (3)", "Acc (5)",
              "N (0.6)", "N (1)", "N (3)", "N (5)",
              "Pro Cur (1)", "Pro Cur (3)", "Pro Cur (5)", "Pro Cur (50)",
              "Slo (0.6)", "Slo (1)", "Slo (3)", "Slo (5)",
              "TPI (3,7)", "TPI (5,15)", "TPI (9,21)", "TPI (15,35)")
  
  ale.dt = bind_rows(ale_lst)
  
  ale.dt = ale.dt %>%
    left_join(pp_tbl, by = "var") %>%
    left_join(desc_tbl, by = c("var" = "variable") )%>%
    mutate(r_x = (std*x) + mean,
           Abr_Name = factor(Abr_Name, 
                             levels = p_order,
                             labels = p_names))
  
  return(ale.dt)
  
}

ale_plot <- function(ale_tbl){
  ale.p = ggplot(ale_tbl, aes(x = r_x, y = y))+
    geom_line(aes(group = label), alpha = 0.2) +
    geom_smooth(color = "red", se = F, method = "gam", formula = y ~ s(x, bs = "cs"))+
    #stat_summary(fun.y = mean, geom = "line", col = "red", size = 1)+
    geom_rug(sides = "b", alpha = 0.2)+
    facet_wrap(~Abr_Name, scales = "free", ncol = 4)+ #Variable_Type_Abr
    labs(x = "Value",
         y = "Scaled soil moisture estimate (ALE)")+ #
    #coord_cartesian(ylim = c(-1,1.5))+
    theme_bw()+
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"))
  return(ale.p)
}
