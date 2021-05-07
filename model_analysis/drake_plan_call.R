# Set up a drake pipeline
## *************************
# Update with model name and version
model_ids = data.frame(m.name = c("xgbTree", "xgbLinear", "gbm", "rf", "svmRadialCost", "rvmRadial", "nnet"),
                       m.ver = c(62, 62, 62, 62, 62, 62, 62))
## ************************
# Directories 
#wDir = "D:/saray/Box Sync/RESEARCH/UAV_ML/R_UAV"
tuneDir = file.path(wDir, "uavtune")
modelDir = file.path(tuneDir, "uavout")
analysisDir = file.path(wDir, "model_analysis")
reportDir = file.path(analysisDir, "Reports")

# File paths
data_path = file.path(tuneDir,"std_uav_data_v3.rds")
uav_data_path = file.path(tuneDir,"uav_data_v3.rds")
subset_path = file.path(tuneDir,"train_split_list.rds")
rmd_path = "report.Rmd" # file.path(analysisDir,"report.Rmd") #does not work with directories

# Dynamically create file names
for (i in 1:nrow(model_ids)){
  model_name = model_ids$m.name[i]
  model_ver = model_ids$m.ver[i]
  # paths
  modelpath = file.path(modelDir, paste(model_name, model_ver, "list.rds", sep = "_") )
  htmlpath = file.path(reportDir, paste(model_name, model_ver, "analysis.html", sep = "_") )
  csvpath = file.path(reportDir, paste(model_name, model_ver, "performance.csv", sep = "_") )
  csvpath_p = file.path(reportDir, paste(model_name, model_ver, "full_performance.csv", sep = "_") )
  # names
  modelpathname = paste0(model_name, "_model_path")
  htmlpathname = paste0(model_name, "_html_path")
  csvpathname = paste0(model_name, "_csv_path")
  csv_p_pathname = paste0(model_name, "_p_csv_path")
  # assign
  assign(modelpathname, modelpath)
  assign(htmlpathname, htmlpath)
  assign(csvpathname, csvpath)
  assign(csv_p_pathname, csvpath_p)
}



# BRT
source(file.path(analysisDir, "drake_plan_gbm.R"))
# XBRT
source(file.path(analysisDir, "drake_plan_xgbTree.R"))
# XBLM
source(file.path(analysisDir, "drake_plan_xgbLinear.R"))
# ANN
source(file.path(analysisDir, "drake_plan_nnet.R"))
# RF
source(file.path(analysisDir, "drake_plan_rf.R"))
# RVR
source(file.path(analysisDir, "drake_plan_rvmRadial.R"))
# SVR
source(file.path(analysisDir, "drake_plan_svmRadialCost.R"))

