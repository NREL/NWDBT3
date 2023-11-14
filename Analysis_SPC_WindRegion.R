# clear R workspace and concole
rm(list=ls(all=TRUE))
cat("\014")

library(ggplot2)
library(stringr)
library(extrafont)

source('/Users/jhao2/Downloads/ProbMetrics_2.R')

wspd_ci <- 4
wspd_co <- 25
wspd_r <- 13

# Define the variable 'a' as a character
a <- "0.2"
# Read data from the CSV file and create a data frame with header
df_full <- read.csv(file.path("/Users/jhao2/Desktop/NWDB/WTK_array/new_loss", paste("G10_alpha_", a, ".csv", sep = "")), header = TRUE, sep = ",", fill = TRUE)
#df_full <- data.frame(read.csv('/Users/jhao2/Desktop/NWDB/WTK_array/result_1/L1_1_lw3.csv',header = TRUE,sep = ",", fill=TRUE))
df_bci <- df_full[df_full$ws <= wspd_ci,]
df_ci <- df_full[df_full$ws > wspd_ci & df_full$ws <= wspd_r,]
df_r <- df_full[df_full$ws > wspd_r & df_full$ws <= wspd_co,]
df_co <- df_full[df_full$ws > wspd_co,]

list_df <- list(df_full, df_bci, df_ci, df_r)

# 
df_fmt <- function(df_input){
  df_ouput <- data.frame(df_input$Actual, df_input$Q50, df_input[,3:ncol(df_input)])
  colnames(df_ouput)[1:2] <- c('Actual', 'Forecast')
  return(df_ouput)
}

df_metrics <- NULL
df_reliability <- NULL
df_plot_reliability <- NULL

df_reliability <- cbind(seq(0.02, 0.98,0.02))
df_sharpness <- NULL
df_plot_sharpness <- NULL

list_name <- c('FullSet', 'Region1', 'Region2', 'Region3')
for (n_df in seq_along(list_df)) {
  list_metrics <- ProbMetrics_2(df_fmt(list_df[[n_df]]))
  df_metrics <- rbind(df_metrics, t(list_metrics[1:7]))
  df_reliability <- cbind(df_reliability, list_metrics$reliability)
  df_plot_reliability <- rbind(df_plot_reliability, cbind(seq(0.02, 0.98,0.02), list_metrics$reliability, list_name[n_df]))
  df_sharpness <- cbind(df_sharpness, list_metrics$sharpness)
  df_plot_sharpness <- rbind(df_plot_sharpness, cbind(seq(0.02, 0.98,0.02), list_metrics$sharpness, list_name[n_df]))
}

df_metrics <- as.data.frame(df_metrics)
rownames(df_metrics) <- list_name
# metrics table
#write.csv(apply(df_metrics,2,as.character), "/Users/jhao2/Desktop/NWDB/WTK_array/new_loss/TG10.csv", row.names=T, col.names = T)
# Construct the full file path
file_path <- file.path("/Users/jhao2/Desktop/NWDB/WTK_array/new_loss/results", paste("G10_alpha_", a, ".csv", sep = ""))
# Write the data frame to the CSV file
write.csv(apply(df_metrics, 2, as.character), file_path, row.names = TRUE, col.names = TRUE)



# 
df_plot_reliability <- rbind(df_plot_reliability, cbind(seq(0.02, 0.98,0.02), seq(0.02, 0.98,0.02), 'Ideal'))
df_plot_reliability <- as.data.frame(df_plot_reliability)
colnames(df_plot_reliability) <- c('Norminal', 'Empirical', 'Group')
df_plot_reliability$Norminal <- as.double(df_plot_reliability$Norminal)
df_plot_reliability$Empirical <- as.double(df_plot_reliability$Empirical)

# reliability plot

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols1 <- gg_color_hue(length(list_df))
ftsize <- 20
lnsize <- 1.5
box_border <- 1
txt_font <- "Times New Roman"

# Construct the full file path
pdf_file_path <- file.path("/Users/jhao2/Desktop/NWDB/WTK_array/new_loss/results", paste("G10reliability_alpha_", a, ".pdf", sep = ""))
# Set up the PDF device with the new file path
pdf(file = pdf_file_path, width = 9, height = 9, onefile = TRUE)
  p1 <- ggplot(data=df_plot_reliability, aes(x=Norminal, y = Empirical, group =Group, color = Group, shape = Group)) +
    geom_line(size = lnsize) +
    labs(y = 'Observed Probability',x = 'Nominal Probability') +
    scale_color_manual(breaks=c('Ideal', list_name), values=c('black', cols1)) +
    scale_shape_manual(breaks=c('Ideal', list_name), values=seq(1,5)) +
    theme_bw() +
    theme(axis.text=element_text(size=ftsize),
          axis.title=element_text(size=ftsize), # axises control
          panel.grid = element_line(size=.3, linetype="dashed"),
          panel.grid.minor = element_line(size=.3, linetype="dashed"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text.x = element_text(angle=0, vjust = 0.5),
          legend.title=element_blank(),
          legend.position=c(.23,.75),
          legend.text=element_text(size=ftsize, colour = 'grey35'),
          legend.direction = "vertical",
          legend.background = element_rect(color = NA, size = 0, linetype = 'solid',
                                           fill = NA)) +
    theme(strip.text = element_text(size=ftsize,lineheight=.5,family=txt_font),
          panel.spacing.x = unit(1, "mm"),
          panel.spacing.y = unit(1, "mm"),
          plot.margin=unit(c(.5,3.5,.5,.5),"mm")
    )
  print(p1)
dev.off()

# sharpness
df_plot_sharpness <- as.data.frame(df_plot_sharpness)
colnames(df_plot_sharpness) <- c('Norminal', 'NIW', 'Group')
df_plot_sharpness$Norminal <- as.double(df_plot_sharpness$Norminal)
df_plot_sharpness$NIW <- as.double(df_plot_sharpness$NIW)

# Construct the full file path
pdf_file_path <- file.path("/Users/jhao2/Desktop/NWDB/WTK_array/new_loss/results", paste("G10sharpness_alpha_", a, ".pdf", sep = ""))
pdf(file = pdf_file_path, width = 9, height = 9, onefile = TRUE)
p2 <- ggplot(data=df_plot_sharpness, aes(x=Norminal, y = NIW, group =Group, color = Group, shape = Group)) + 
  geom_line(size = lnsize) +
  labs(y = expression("Interval Width [kW]"),x = 'Nominal Probability') + 
  # scale_color_manual(breaks=c(list_), values=c(cols1)) +
  # scale_shape_manual(breaks=c(list_method), values=seq(1,7)) + 
  theme_bw() + 
  theme(axis.text=element_text(size=ftsize),
        axis.title=element_text(size=ftsize+5), # axises control
        panel.grid = element_line(size=.3, linetype="dashed"),
        panel.grid.minor = element_line(size=.3, linetype="dashed"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.x = element_text(angle=0, vjust = 0.5),
        legend.title=element_blank(),
        legend.position=c(.1,.87),
        legend.text=element_text(size=ftsize, colour = 'grey35'),
        legend.direction = "vertical",
        legend.background = element_rect(color = NA, size = 0, linetype = 'solid',
                                         fill = NA)) +
  theme(strip.text = element_text(size=ftsize+5,lineheight=.5,family=txt_font),
        panel.spacing.x = unit(1, "mm"),
        panel.spacing.y = unit(1, "mm"),
        plot.margin=unit(c(.5,3.5,.5,.5),"mm")
  ) 
print(p2)
dev.off()

# scatterplot by region
list_point <- c('Actual', 'Q1', 'Q50', 'Q99')
df_point <- NULL
for (n_point in list_point) {
  df_point <- rbind(df_point, cbind(df_full$ws, df_full[,n_point], n_point))
}
df_point <- as.data.frame(df_point)
colnames(df_point) <- c('WSPD', 'Power', 'Group')
df_point$WSPD <- as.double(df_point$WSPD)
df_point$Power <- as.double(df_point$Power)

df_curve <- data.frame(read.csv('/Users/jhao2/Desktop/NWDB/Data/GE1.5_ManufacurePowerCurve.csv',header = TRUE,sep = ",", fill=TRUE))
#df_curve <- data.frame(read.csv('/Users/jhao2/Desktop/NWDB/Data/MITS1.0_ManufacurePowerCurve.csv',header = TRUE,sep = ",", fill=TRUE))


cols1 <- gg_color_hue(3)

# Construct the full file path
pdf_file_path <- file.path("/Users/jhao2/Desktop/NWDB/WTK_array/new_loss/results", paste("G10scatterplot_alpha_", a, ".pdf", sep = ""))
pdf(file = pdf_file_path, width = 9, height = 5, onefile = TRUE)
#pdf(file = '/Users/jhao2/Desktop/NWDB/WTK_array/new_loss/TG10scatterplot.pdf', width=9, height= 5, onefile =TRUE)
p3 <- ggplot() + geom_point(data = df_point, aes(x = WSPD, y = Power, group = Group, color = Group, size = Group)) +
      scale_color_manual(breaks=list_point, values=c('grey', cols1)) +
      scale_size_manual(breaks=list_point, values=c(2, rep(1, 3))) + 
      geom_line(data = df_curve, aes(x = WSPD_M.S, y = Power_KW), color = 'black', size = 1) + 
      geom_vline(xintercept = c(3.5, 12, 25), color = 'red', linetype = 'dashed') +
      theme_bw() + labs(y = 'Wind Power [kW]',x = expression("Wind Speed [m/s]")) +
      theme_bw() +
      theme(axis.text=element_text(size=ftsize),
            axis.title=element_text(size=ftsize), # axises control
            panel.grid = element_line(size=.3, linetype="dashed"),
            panel.grid.minor = element_line(size=.3, linetype="dashed"),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            axis.text.x = element_text(angle=0, vjust = 0.5),
            legend.title=element_blank(),
            legend.position=c(.25,.85),
            legend.text=element_text(size=ftsize, colour = 'grey35'),
            legend.direction = "vertical",
            legend.background = element_blank()) +
      theme(strip.text = element_text(size=ftsize,lineheight=.5,family=txt_font),
            panel.spacing.x = unit(1, "mm"),
            panel.spacing.y = unit(1, "mm"))

print(p3)
dev.off()

# time series plot of percentiles
df_plot3 <- NULL
len <- 168 # 1 week
for (n_col in 1:9) {
  L <- paste0('Q', (1-.1*n_col)/2*100)
  U <- paste0('Q', (1-(1-.1*n_col)/2)*100)
  df_plot3 <- rbind(df_plot3, cbind(seq(1, len, 1), df_full[1:len,L], df_full[1:len,U], n_col))
}
df_plot3 <- as.data.frame(df_plot3)
colnames(df_plot3) <- c('Index', 'L', 'U', 'Group')
df_plot3$L <- as.double(df_plot3$L)
df_plot3$U <- as.double(df_plot3$U)
df_plot3$Group <- as.character(df_plot3$Group)

list_alpha <- seq(.1, .3, length = 9)
legend_alpha <- paste0(seq(90, 10, by = -10), '%')

pdf_file_path <- file.path("/Users/jhao2/Desktop/NWDB/WTK_array/new_loss/results", paste("G10TS_alpha_", a, ".pdf", sep = ""))
pdf(file = pdf_file_path, width = 15, height = 5, onefile = TRUE)
#pdf(file = '/Users/jhao2/Desktop/NWDB/WTK_array/new_loss/TG10TS.pdf', width=15, height= 5, onefile =TRUE)

p5<-   ggplot() +  
  geom_ribbon(data = df_plot3, aes(x = Index, ymin = L, ymax = U, alpha = Group))+
  geom_line(data = df_full[1:len,], aes(x=seq(1, len, 1), color = 'red', y=Q50)) + geom_point(data = df_full[1:len,], aes(x=seq(1, len, 1),color = 'red',  y=Q50))+
    scale_alpha_manual(values = list_alpha, labels = legend_alpha) +
    theme_bw() + labs(x="Time Step [dimensionless]",y=expression("Wind Power [kW]")) +
    theme(panel.grid = element_line(size=.3, linetype="dashed"),
          panel.grid.minor = element_line(size=.3, linetype="dashed"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          axis.text.x = element_text(size=ftsize),
          axis.title.x = element_text(size=ftsize),
          axis.text.y = element_text(size=ftsize),
          axis.title.y = element_text(size=ftsize),
          #axis.text.y.right = element_text(size=ftsize, family=txt_font, color = cols1[1]),
          #axis.title.y.right = element_text(size=ftsize, family=txt_font, color = cols1[1], vjust = 2.5),
          legend.text=element_text(size=(ftsize-5)),legend.title=element_blank(),
          legend.text.align = 0,legend.title.align = .5,
          legend.direction = "horizontal", legend.position=c(.15, .95),legend.key = element_blank(),
          legend.background = element_rect(color = NA, size = 0.5, linetype = "solid"),
          legend.key.width = unit(.5,"cm"),
          strip.text = element_text(size=ftsize,lineheight=.5,family=txt_font),
          plot.margin=unit(c(.5,5,2,1),"mm")
    )
    
print(p5)
dev.off()

# additional analysis on R3
# df_point_r3 <- NULL
# for (n_point in colnames(df_r)[2:ncol(df_r)]) {
#   df_point_r3 <- rbind(df_point_r3, cbind(df_r$ws, df_r[,n_point], n_point))
# }
# df_point_r3 <- as.data.frame(df_point_r3)
# colnames(df_point_r3) <- c('WSPD', 'Power', 'Group')
# df_point_r3$WSPD <- as.double(df_point_r3$WSPD)
# df_point_r3$Power <- as.double(df_point_r3$Power)
# 
# cols1 <- gg_color_hue(100)
# 
# pdf(file = '/Users/jhao2/Desktop/NWDB/WTK_array/new_loss/TG10scatterplot_r3.pdf', width=9, height= 5, onefile =TRUE)
# 
#  p4 <- 
# ggplot() + geom_point(data = df_point_r3, aes(x = WSPD, y = Power, group = Group, color = Group, size = Group)) +
#   scale_color_manual(breaks=colnames(df_r)[2:ncol(df_r)], values=c('grey', cols1)) +
#   scale_size_manual(breaks=colnames(df_r)[2:ncol(df_r)], values=c(2, rep(.1, 100))) +
#   theme_bw() + labs(y = 'Wind Power [kW]',x = expression("Wind Speed [m/s]")) +
#   theme_bw() +
#   theme(axis.text=element_text(size=ftsize),
#         axis.title=element_text(size=ftsize), # axises control
#         panel.grid = element_line(size=.3, linetype="dashed"),
#         panel.grid.minor = element_line(size=.3, linetype="dashed"),
#         panel.border = element_rect(colour = "black", fill=NA, size=1),
#         axis.text.x = element_text(angle=0, vjust = 0.5),
#         legend.title=element_blank(),
#         legend.position = "none",
#         legend.text=element_text(size=ftsize, family=txt_font, colour = 'grey35'),
#         legend.direction = "vertical",
#         legend.background = element_blank()) +
#   theme(strip.text = element_text(size=ftsize,lineheight=.5,family=txt_font),
#         panel.spacing.x = unit(1, "mm"),
#         panel.spacing.y = unit(1, "mm"))
#  
#  print(p4)
#  dev.off()

 