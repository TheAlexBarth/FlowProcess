# library(ggplot2)
#
# pscope_test <- read.table('~/../../../Users/Chironex/Documents/ecotaxa_export.tsv',
#                           sep = '\t', header = T, comment.char = '[')
# zooscan_test <- read.table('~/../../../Users/Chironex/Documents/ecotaxa_2_zpkb_153_1.tsv',
#                            sep = '\t', header = T, comment.char = '[')
#
#
# root_dir <- "D:/FlowCam_X3AI_4x/_work/USC_X3AI_4X_FOV300_R7"
# dir(root_dir)
#
# establish_flowprocess(root_dir, clean_files = F)
#
#
# file_meta <- read.csv(paste0(root_dir,"/",dir(root_dir)[1]))
#
#
# ggplot(file_meta) +
#   geom_point(aes(x = Capture.X, y = Capture.Y))+
#   theme_minimal()
