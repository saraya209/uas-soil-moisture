## rf ----
plan_rf = drake_plan(
  # Dynamic names
  # ***************************************************
  model.lst = readRDS(file_in(!!rf_model_path)),
  summ_table = write.csv(
    perf.summ.print,
    file = file_out(!!rf_csv_path) ),
  perf_table = write.csv(
    perf.dt,
    file = file_out(!!rf_p_csv_path) ),
  report = rmarkdown::render(
    knitr_in(!!rmd_path),
    output_file = file_out(!!rf_html_path),
    quiet = TRUE ),
  pty.model.name = "RF",
  # *************************************************
  
  # Import Data and Files 
  dt = readRDS(file_in(!!data_path)),
  uav.dt = readRDS(file_in(!!uav_data_path)),
  subset.lst = readRDS(file_in(!!subset_path) ),
  var_desc.dt = read_excel("Variable_Desc.xlsx"),
  # Import Model

  # Label for reporting 
  model.name = model.lst[[1]][[1]],
  
  # Generate list of test data frames
  test.dt.lst = lapply(subset.lst, 
                       resampleListFunction, 
                       mydata = dt, 
                       train = FALSE,
                       rem.cols = FALSE),
  
  # Summarize Model 
  # Residual error 
  perfsumm.lst = summ_perf(model_list = model.lst, 
                           test_list = test.dt.lst, 
                           model_name = pty.model.name),
  perf.dt = perfsumm.lst$perf_dt,
  perf.summ.dt = perfsumm.lst$perfsumm_dt,
  c.perf.summ.dt = perfsumm.lst$perfens_dt,
  perf.summ.print = bind_rows(perf.summ.dt, c.perf.summ.dt),
  explainer.lst = perfsumm.lst$explain_lst,
  
  
  model_order = levels(perf.summ.dt$label_ordered),
  
  # Residual and MAE plot
  p.box = perf_box(full.data = perf.dt, summ.data = perf.summ.dt),
  # p.box
  p.box_pty = perf_box_pty(full.data = perf.dt, summ.data = perf.summ.dt),
  # One to one plot
  # subset best models
  best30 = best_models(summ.data = perf.summ.dt, nmodels = 30),

  o.plot = one_to_one(full.data = perf.dt, 
                      summ.data = perf.summ.dt, 
                      best.models = best30,
                      num_cols = 5),
  # o.plot black and white
  o.plot_bw = one_to_one_bw(full.data = perf.dt, 
                            summ.data = perf.summ.dt, 
                            best.models = best30,
                            num_cols = 5),
  
  # o.plot combined
  c.o.plot = one_to_one_combined(full.data = perf.dt, 
                                 summ.data = c.perf.summ.dt),
  
  # Model Specific  or Model Independent Importance
  # Calculate variable importance
  imp.lst = summ_imp(model_list = model.lst, 
                     model_name = pty.model.name,
                     desc_table = var_desc.dt,
                     sort_order = model_order,
                     explain_list = explainer.lst),
  imp.dt = imp.lst[[1]],
  imp.method = imp.lst[[2]],
  
  
  
  # Plot importance boxplot
  imp.plot_box = imp_plot_box(imp.data = imp.dt, best.models = best30),
  # Plot importance grouped by sum
  sumimp.plot_box = imp_plot_box(imp.data = imp.dt, best.models = best30, grouped = "sum"),
  # Plot importance grouped by mean
  meanimp.plot_box = imp_plot_box(imp.data = imp.dt, best.models = best30, grouped = "mean"),
  
  # Variable effect
  ale.lst = lapply(explainer.lst, ale_fun),
  
  # Calculate original values from standardized values
  # pre processing values
  pred.names = names(uav.dt)[-(1:4)],
  pp = preProcess(uav.dt, method = list(center = pred.names , scale = pred.names)),
  pp.dt = tibble(var = names(pp$mean), mean = pp$mean, std = pp$std),
  
  #Preprocess tables
  ale.dt = ale_summ(ale.lst, pp.dt, var_desc.dt),
  
  ale.p = ale_plot(ale.dt),

  # Tunning Summary for Best
  # Tune parameters table
  summ_tune.dt = summ_tune(model_list = model.lst, 
                           model_name = pty.model.name,
                           sort_order = model_order),
  
  
  # Tune parameters plots
  tune.plots = mapply(tune_plot_summ, 
                      model.lst, 
                      model_name = pty.model.name,
                      model_id = 1:length(model.lst),
                      SIMPLIFY = F)#,

)
