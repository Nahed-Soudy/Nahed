classify_gene <- function(logFC, padj) {
  if (is.na(padj)) padj <- 1   # Replace missing padj with 1
  if (logFC > 1 & padj < 0.05) {
    return("Upregulated")
  } else if (logFC < -1 & padj < 0.05) {
    return("Downregulated")
  } else {
    return("Not_Significant")
  }
}
files <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")
for (file in files) {
  data <- read.csv(file, header = TRUE)
  
  data$padj[is.na(data$padj)] <- 1
  data$status <- mapply(classify_gene, data$logFC, data$padj)
  if (!dir.exists("Results")) {
    dir.create("Results")
  }
  out_file <- paste0("Results/Processed_", file)
  write.csv(data, out_file, row.names = FALSE)
  
  cat("\nSummary for:", file, "\n")
  print(table(data$status))
} 
 