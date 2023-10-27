library(ggplot2)

pscope_test <- read.table('~/../../../Users/Chironex/Documents/ecotaxa_export.tsv', sep = '\t', header = T)
zooscan_test <- read.table('~/../../../Users/Chironex/Documents/ecotaxa_2_zpkb_153_1.tsv')

root_dir <- "D:/USC_X3AI_4X_FOV300_R5"
dir(root_dir)

establish_flowprocess(root_dir, clean_files = F)

etx_zip(folder_name = root_dir,
        folder_location = root_dir)


file_meta <- read.csv(paste0(root_dir,"/",dir(root_dir)[1]))


ggplot(file_meta) +
  geom_point(aes(x = Capture.X, y = Capture.Y))+
  theme_minimal()
