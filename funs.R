# Set common ggplot theme options

stdtheme <- theme(text = element_text(size = 16))

# Format value as percent

formatpct <- function(x,dec = 1) round(x*100,dec)

# transpose a table and rejigger it

tabletranspose <- function(data){
  x <- t(data)
  colnames(x) <- x[1,]
  x <- x[-1,]
  x <- as.data.frame(x)
  return(x)
}

# Convert Dxy to AUROC

dxy2auc <- function(dxy){
  0.5 * (dxy + 1)
}

# Generate an empty dataframe to contain model summaries

empty_modsum <- function(){
  
  out <- data.frame(dat = character(1),
                    lab = character(1),
                    method = character(1),
                    ts = character(1),
                    max_train_auc = numeric(1),
                    max_train_auc_std = numeric(1),
                    max_test_auc = numeric(1),
                    max_test_auc_std = numeric(1),
                    max_train_aucpr = numeric(1),
                    max_train_aucpr_std = numeric(1),
                    max_test_aucpr = numeric(1),
                    max_test_aucpr_std = numeric(1),
                    iter = numeric(1),
                    note = character(1),
                    stringsAsFactors = F)
  
  # Lists don't like being defined in a call to data.frame
  out$preds <- list(0)
  
  return(out)
  
}

# function for looping through a set of predictor/outcome pairs

mbs_modsum <- function(data,             # a matrix containing all data
                       obs,              # row indexes for current subset
                       cols,             # list of column indexes of predictive datasets
                       outs,             # vector of column indexes of outcome family
                       methods,          # character vector of methods to be used
                       note = NA,        # note
                       preds = F,        # Save predictions?
                       verbose = F,      # Print output?
                       booster = "dart", # Set default values for xgboost params
                       rate_drop = 0.2,
                       nrounds = 200, 
                       nfold = 10,
                       max = T,
                       early_stopping_rounds = 50,
                       objective = "binary:logistic"){
  
  mod_summary <- empty_modsum()
  
  # For each set of predictors...
  for (i in 1:length(cols)){
    
    # Define predictor dataset
    dat <- data[obs,cols[[i]]]
    
    # For each outcome...
    for (j in 1:length(outs)){
      
      # Define vector of labels
      lab <- data[obs,outs[j]]
      
      if ("xgb" %in% methods){
        
        print(paste(names(cols[i]),dimnames(data)[[2]][outs[j]],Sys.time()))
        
        # Train model
        xgbmod <- xgb.cv(data = dat, 
                         label = lab, 
                         booster = booster,
                         rate_drop = rate_drop,
                         nrounds = nrounds, 
                         early_stopping_rounds = early_stopping_rounds,
                         prediction = preds,
                         nfold = nfold,
                         maximize = max,
                         verbose = T,
                         eval_metric = ("auc"),
                         eval_metric = ("aucpr"),
                         objective = objective)
        
        # Save model summary info to a dataframe
        outp <- data.frame(dat = names(cols[i]),
                           lab = dimnames(data)[[2]][outs[j]],
                           method = "xgb",
                           ts = as.character(Sys.time()),
                           max_train_auc = as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,2]),
                           max_train_auc_std = as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,3]),
                           max_test_auc = as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,6]),
                           max_test_auc_std = as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,7]),
                           max_train_aucpr = as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,4]),
                           max_train_aucpr_std = as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,5]),
                           max_test_aucpr = as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,8]),
                           max_test_aucpr_std = as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,9]),
                           iter = as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,1]),
                           note = note,
                           stringsAsFactors = F)
        
        # Update progress
        print(paste("auc",as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,6]),
                    "aucpr",as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,8]),
                    "iter",as.numeric(xgbmod$evaluation_log[xgbmod$best_iteration,1])))
        
        # Save out-of-sample predictions to a list maybe
        if(preds == T){
          
          outp$preds <- list(xgbmod$pred)
          
        }else{
          
          outp$preds <- list(0)
        }
        
        # Bind new predictions to output dataframe
        mod_summary <- rbind(mod_summary, outp)
        
        # Discard the model and clear up some memory
        remove(xgbmod)
        gc()
        
      }
      
      if("auc" %in% methods){
        
        # Make sure we have only a single predictor
        if(class(dat) != "numeric"){
          
          print("Need 1 predictor for AUROC")
          
        }else{
          t <- data.frame(dat = dat,
                          lab = lab) %>%
            filter(complete.cases(.))
          
          # Calculate AUCs and save to output dataframe
          tpr <- PRROC::pr.curve(t[t[2] == 1,1], 
                                 t[t[2] == 0,1])
          troc <- PRROC::roc.curve(t[t[2] == 1,1], 
                                   t[t[2] == 0,1])
          
          outp <- data.frame(dat = names(cols[i]),
                             lab = dimnames(data)[[2]][outs[j]],
                             method = "auc",
                             ts = as.character(Sys.time()),
                             max_train_auc = NA,
                             max_train_auc_std = NA,
                             max_test_auc = troc$auc,
                             max_test_auc_std = NA,
                             max_train_aucpr = NA,
                             max_train_aucpr_std = NA,
                             max_test_aucpr = tpr$auc.integral,
                             max_test_aucpr_std = NA,
                             iter = NA,
                             note = note,
                             stringsAsFactors = F)
          
          outp$preds <- list(0)
          
          mod_summary <- rbind(mod_summary, 
                               outp)
        }
      }
    }
  }
  
  return(mod_summary)
}

#Extract predictions from model summary table

extract_preds <- function(x){
  
  out <- matrix(nrow = length(x$preds[[1]]),
                ncol = nrow(x))
  
  dimnames(out)[[2]] <- x$lab
  
  for(i in 1:nrow(x)){
    out[,i] <- unlist(x$preds[[i]])
  }
  return(out)
}

formatpct <- function(x,dec = 1) round(x*100,dec)

tabletranspose <- function(data){
  x <- t(data)
  colnames(x) <- x[1,]
  x <- x[-1,]
  x <- as.data.frame(x)
  return(x)
}

# make a pallete of colors corresponding to dispatch priorities
poutpal <- c("#FF0000","#FFA600","#FFFF00","#00FF00","#2020FF","#FF00FF")

poutmap <- c("Unused","Referral","2B","2A","1B","1A")

# Define out-of-regon destinations

oordest <- c("Gävle Sjukhus",
             "Västerås Sjukhus",
             "Karolinska Sjukhus",
             "Örebro Sjukhus", 
             "Stockholm", 
             "Eskilstuna Sjukhus",
             "Avesta Sjukhus", 
             "Köping Sjukhus", 
             "Lindesberg Sjukhus", 
             "Falun Sjukhus", 
             "Falu Lasarett")

# Define pretty variable names

prettyname <- function(x,vn = varnames,mod = F){
  
  if(mod){
    vn <- vn[,-2]
  }else{
    vn <- vn[,-3]
  }
  
  if(length(x) == 1){
    
    o <- vn[which(vn[,2] == x),1]
    
  }else{
    
    o <- sapply(x, function(y){
      vn[which(vn[,2] == y),1]
      
    })
  }
  
  return(as.character(unname(o)))
  
}

varnames <- c(
  "caseid" = "Case ID",
  "null_age" = "Patient Age", 
  "null_female" = "Female gender", 
  "null_pcontacts" = "Number of prior contacts", 
  "null_lastcontact" = "Days since last contact", 
  "null_wkday" = "Weekday", 
  "null_time_sin" = "Contact Time (sin transformed)",
  "null_time_cos" = "Contact Time (cos transformed)",
  "null_month_sin" = "Contact Month (sin transformed)",
  "null_month_cos" = "Contact Month (cos transformed)",
  "amb_ambtxp" = "Ambulance transport", 
  "amb_meds" = "Medications administered", 
  "amb_pin" = "Lights & sirens\nto hospital", 
  "amb_o2" = "Supplemental oxygen", 
  "amb_ekgtx" = "EKG transmitted to hospital", 
  "amb_gauze" = "Bleeding control/bandages applied", 
  "amb_iv" = "IV placed", 
  "amb_immob" = "Spine/joint immobilization", 
  "amb_cpr" = "CPR administered by crew", 
  "amb_crit" = "Patient documented as critical", 
  "amb_alert" = "Pre-arrival notification given", 
  "amb_any" = "Any intervention\nprovided", 
  "v_br" = "Breathing rate", 
  "v_spo2" = "Capillary oxygen saturation", 
  "v_pr" = "Pulse rate", 
  "v_bp" = "Systolic blood pressure", 
  "v_temp" = "Temperature", 
  "v_avpu" = "AVPU", 
  "v_gcs" = "Glasgow Coma Scale",
  "news" = "NEWS score", 
  "news_hi" = "NEWS score > 7", 
  "news_lo" = "NEWS score > 4",
  "hosp_ed" = "Patient treated at ED", 
  "hosp_admit" = "Patient admitted\nto ward", 
  "hosp_icu" = "Patient treated\nat ICU", 
  "hosp_radio" = "CT scan performed", 
  "hosp_op" = "Surgical intervention", 
  "hosp_dead3day" = "3-day mortality", 
  "cat_Allergisk reaktion" = "Allergic Reaction", 
  "cat_Allmän barn" = "General Child", 
  "cat_Allmän vuxen" = "General Adult", 
  "cat_Allmän åldring" = "General Elderly", 
  "cat_Andningsbesvär" = "Difficulty Breathing", 
  "cat_Annat" = "Other", 
  "cat_Arm-, bensymtom (ej trauma)" = "Arm/leg sympoms (non-traumatic)", 
  "cat_Blod i urin" = "Blood in urine", 
  "cat_Blodig upphostning" = "Blood in cough", 
  "cat_Blodsocker lågt" = "Low Blood sugar", 
  "cat_Brand" = "Fire", 
  "cat_Brännskada" = "Burn", 
  "cat_Bröstsmärta" = "Chest pain", 
  "cat_Buk-, flanksmärta" = "Abdominal/flank pain", 
  "cat_CBRN" = "CBRN", 
  "cat_Diarré" = "Diarrhea", 
  "cat_Drunkningstillbud" = "Drowning", 
  "cat_Dykeriolycka" = "Diving accident", 
  "cat_Elektrisk skada" = "Electical injury", 
  "cat_Feber" = "Fever", 
  "cat_Flygolycka" = "Aircraft accident", 
  "cat_Förlossning" = "Childbirth", 
  "cat_Förvirring" = "Confusion", 
  "cat_Graviditet" = "Pregnancy", 
  "cat_Hallucination" = "Hallucination", 
  "cat_Halsont" = "Sore throat", 
  "cat_Hjärtstopp" = "Cardiac arrest", 
  "cat_Huvudvärk" = "Headache", 
  "cat_Hypotermi" = "Hypothermia", 
  "cat_ICD" = "ICD", 
  "cat_Illamående" = "Nausea", 
  "cat_Infektion" = "Infection", 
  "cat_Intox/förgiftning" = "Intoxication", 
  "cat_Kramper" = "Convulsions", 
  "cat_Kräkning" = "Vomiting", 
  "cat_Köldskada" = "Cold injury", 
  "cat_Luftvägsbesvär" = "Airway problems", 
  "cat_Mag-, tarmblödning" = "Stomach/Intestinal bleeding", 
  "cat_Näs-, svalgblödning" = "Nose/Throat bleeding", 
  "cat_Ormbett" = "Snake bite", 
  "cat_Pacemaker" = "Pacemaker", 
  "cat_Planerad" = "Planned",
  "cat_Psykiska besvär" = "Psychiatic problems", 
  "cat_Ryggsmärta" = "Back pain", 
  "cat_Rytmrubbning" = "Cardiac arythmia", 
  "cat_Rökexponering" = "Smoke exposure", 
  "cat_Saknas" = "MBS Unused", 
  "cat_Sensoriskt-, motoriskt bortfall" = "Sensory/motor deficiency", 
  "cat_Sjöolycka" = "Maritime accident", 
  "cat_Stroke" = "Stroke", 
  "cat_Svimning" = "Fainting", 
  "cat_Sårskada" = "Minor truama", 
  "cat_Sänkt vakenhet" = "Reduced Consciousness", 
  "cat_Trafikolycka" = "Traffic accident", 
  "cat_Trauma" = "Trauma", 
  "cat_Urinkateterstopp" = "Urinary catheter blockage", 
  "cat_Urinstämma" = "Urinary retention", 
  "cat_Urogenitala besvär" = "Urogenital problems", 
  "cat_Vaginal blödning" = "Vaginal bleeding", 
  "cat_Våld-hot-suicidhot" = "Violence/threats/Suicide", 
  "cat_Yrsel" = "Dizziness", 
  "cat_Ögon" = "Eye problems", 
  "freetext" = "", 
  "operator" = "", 
  "starttime" = "", 
  "pnrvalid" = "", 
  "pnrage" = "", 
  "pnrfemale" = "", 
  "pout" = "Dispatched Priority", 
  "closereason" = "", 
  "calltype" = "", 
  "ambj" = "", 
  "export" = "", 
  "cosmicj" = "", 
  "catpn" = "", 
  "recpoutncat" = "", 
  "recpoutalln" = "MBS priority (numeric)", 
  "recpoutall" = "MBS priority", 
  "cr" = "Care need", 
  "crdet" = "Disposition", 
  "crdest" = "Destination", 
  "crcorr" = "Care need", 
  "wkday" = "Weekday",
  "eddiag" = "ED Diagnosis", 
  "carehrs" = "Hours until discharge", 
  "daystodeath" = "Days to death", 
  "hour" = "Contact hour", 
  "dead" = "Patient died within follow-up", 
  "daystodeathpos" = "Days to death", 
  "daystodeathneg" = "Days to death", 
  "recpout" = "MBS priority", 
  "recpoutn" = "MBS priority", 
  "poutn" = "Dispatched priority",
  "ambptobs" = "Ambulance",
  "vobs" = "Vitals",
  "hospptobs" = "Hospital",
  "txpobs" = "ED Transports",
  "null" = "Null", 
  "cat" = "Call Types", 
  "mbs" = "MBS", 
  "alldisp" = "XGB model - Dispatch", 
  "allamb" = "XGB model - Ambulance",
  "train_auc_mean" = "Mean Training AUROC",
  "test_auc_mean" = "Mean Test AUROC")

varnames <- data.frame(pretty = varnames,
                       orig = names(varnames),
                       mod = make.names(names(varnames)),
                       stringsAsFactors = F)

# Clear non-medical calls and calls with no disposition data, classify referrals as to transport/care

cleandata <- function(x){
  
  o <- x %>%
    filter(crcorr %in% c("Ambulance need", "No Ambulance care need"),
           as.Date(starttime) > as.Date("2016-08-01")) %>%
    mutate(poutref = ifelse(pout == "Referral",
                            ifelse(crdet %in% c("Hänvisning till annat transportsätt",
                                                "Hänvisning till sjukresa",
                                                "Annat transportsätt",
                                                "Sjukresa",
                                                "Helikopter"),
                                   "Alt. txp",
                                   "Alt. care"),
                            as.character(pout))) %>%
    mutate(poutref = factor(poutref,levels = c("1A", "1B", "2A", "2B", "Alt. txp", "Alt. care")))
  
  return(o)
  
}
