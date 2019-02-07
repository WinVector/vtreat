
test_rquery <- function() {
  
  # see what tests we can run
  
  eval_examples <- requireNamespace("rquery", quietly = TRUE)
  eval_rqdt <- eval_examples && 
    requireNamespace("rqdatatable", quietly = TRUE)
  eval_db <- eval_examples &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)
  
  # regular in-memory runs
  
  dTrainC <- data.frame(x= c('a', 'a', 'a', 'b' ,NA , 'b'),
                        z= c(1, 2, NA, 4, 5, 6),
                        y= c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE),
                        stringsAsFactors = FALSE)
  dTrainC$id <- seq_len(nrow(dTrainC))
  treatmentsC <- designTreatmentsC(dTrainC, c("x", "z"), 'y', TRUE,
                                   verbose = FALSE)
  treated_c_1 <- prepare(treatmentsC, dTrainC)
  
  dTrainR <- data.frame(x= c('a', 'a', 'a', 'b' ,NA , 'b'),
                        z= c(1, 2, NA, 4, 5, 6),
                        y= as.numeric(c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)),
                        stringsAsFactors = FALSE)
  dTrainR$id <- seq_len(nrow(dTrainR))
  treatmentsN <- designTreatmentsN(dTrainR, c("x", "z"), 'y',
                                   verbose = FALSE)
  treated_n_1 <- prepare(treatmentsN, dTrainR)
  
  dTrainZ <- data.frame(x= c('a', 'a', 'a', 'b' ,NA , 'b'),
                        z= c(1, 2, NA, 4, 5, 6),
                        stringsAsFactors = FALSE)
  dTrainZ$id <- seq_len(nrow(dTrainZ))
  treatmentsZ <- designTreatmentsZ(dTrainZ, c("x", "z"), 
                                   verbose = FALSE)
  treated_z_1 <- prepare(treatmentsZ, dTrainZ)
  
  
  if(eval_examples) {
    rqplan_c <- as_rquery_plan(list(treatmentsC))
    rqplan_n <- as_rquery_plan(list(treatmentsN))
    rqplan_z <- as_rquery_plan(list(treatmentsZ))
    
    if(eval_rqdt) {
      treated_dt_c1 <- vtreat::rqdatatable_prepare(rqplan_c, dTrainC, 
                                                   extracols = "id")
      treated_dt_c2 <- vtreat::rqdatatable_prepare(rqplan_c, dTrainC, 
                                                   extracols = "id", 
                                                   non_join_mapping = TRUE)
      
      treated_dt_r <- vtreat::rqdatatable_prepare(rqplan_n, dTrainR, 
                                                  extracols = "id")
      
      treated_dt_z <- vtreat::rqdatatable_prepare(rqplan_z, dTrainZ, 
                                                  extracols = "id")
    }
    
    if(eval_db) {
      db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      
      source_data_c <- rquery::rq_copy_to(db, "dTrainC", dTrainC,
                                          overwrite = TRUE, temporary = TRUE)
      source_data_r <- rquery::rq_copy_to(db, "dTrainR", dTrainR,
                                          overwrite = TRUE, temporary = TRUE)
      source_data_z <- rquery::rq_copy_to(db, "dTrainZ", dTrainZ,
                                          overwrite = TRUE, temporary = TRUE)
      
      rest_c <- rquery_prepare(db, rqplan_c, source_data_c, "dTreatedC",
                               extracols = "id")
      resd_c <- DBI::dbReadTable(db, rest_c$table_name)
      
      rest_n <- rquery_prepare(db, rqplan_n, source_data_r, "dTreatedN",
                               extracols = "id")
      resd_n <- DBI::dbReadTable(db, rest_n$table_name)
      
      rest_z <- rquery_prepare(db, rqplan_z, source_data_z, "dTreatedZ",
                               extracols = "id")
      resd_z <- DBI::dbReadTable(db, rest_z$table_name)
      
      rquery::rq_remove_table(db, source_data_c$table_name)
      rquery::rq_remove_table(db, rest_c$table_name)
      rquery::rq_remove_table(db, source_data_r$table_name)
      rquery::rq_remove_table(db, rest_n$table_name)
      rquery::rq_remove_table(db, source_data_z$table_name)
      rquery::rq_remove_table(db, rest_z$table_name)
      
      DBI::dbDisconnect(db)
    }
    # TODO: compare frames
  }
  
  invisible(NULL)
}
