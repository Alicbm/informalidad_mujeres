library(data.table)

merge_month <- function(month) {
  
  key_variables <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "HOGAR", "FEX_C18")
  
  base_dir <- file.path(getwd(), "datos", month)
  
  all_files <- list.files(path = base_dir, pattern = "*.csv", full.names = TRUE, ignore.case = TRUE)
  
  final_df <- fread(file = all_files[1])
  
  for (file in all_files[-1]) {
    df <- fread(file = file)
    
    new_key_variables <- intersect(colnames(df), key_variables)
    
    final_df <- merge(final_df, df, by = new_key_variables, all.x = TRUE)
    
    cols_x <- grep("\\.x$", colnames(final_df), value = TRUE)
    setnames(final_df, old = cols_x, new = gsub("\\.x$", "", cols_x))
    
    cols_y <- grep("\\.y$", colnames(final_df), value = TRUE)
    final_df[, (cols_y) := NULL]
    
  }
  
  return(final_df)
}

agosto <- merge_month("agosto")
fwrite(agosto, "agosto.csv")


