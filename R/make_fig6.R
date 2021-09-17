# Figure 6
# Errors in derived quantities

make_fig6 = function() {
  source("R/error_prop_6400.R", echo=TRUE)
  error_6400_data = read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/error_rest_6400_subsampled_broadleaf.csv")
  error_est_reg_6400 = est_error_6400_reg(error_6400_data, m = 0.7838, b = 0.8092)
  
  source("R/error_prop_6800.R", echo=TRUE)
  error_6800_data <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/error_rest_6800_subsampled_new.csv")
  error_est_reg_6800 = est_error_6800_reg(error_6800_data, m = 0.4941, b = -1.1436)
  
  # Bind together
  error_est_reg_6400$Licor_Type = "LI-6400XT"
  error_est_reg_6800$Licor_Type = "LI-6800"
  
  error_all = rbind(error_est_reg_6400, error_est_reg_6800)
  error_all = subset(error_all, error < 1)
  
  # Open file
  pdf("figures/fig6.pdf", width = 7, height = 3.5)
  
  # Make label text
  dat_text <- data.frame(
    labels = c("(b) ~~ italic(g[sw])", "(c) ~~ italic(g[tw])", "(d) ~~ italic(g[tc])", "(e) ~~ italic(C[i])"),
    variable = c("g_sw","g_tw","g_tc","C_i"),
    Tair = 0,
    Tleaf = 0,
    error = 0,
    Licor_Type = "LI-6400XT"
  )
  
  # Build plots
  p1 = ggplot(data = error_all, aes(x = variable, y=100*error, fill=Licor_Type)) + 
    scale_fill_manual(values = palette_a) +
    scale_colour_manual(values = palette_a) +
    geom_boxplot() +
    ylab("Relative error (%)") + xlab(NULL) + #ylim(-0.75, 0.75) + 
    scale_x_discrete(labels=c("g_sw" = expression(italic("g"[sw])), 
                              "g_tw" = expression(italic("g"[tw])), 
                              "g_tc" = expression(italic("g"[tc])),
                              "C_i" = expression(italic("C"[i])))) +
    my_theme +
    theme(legend.position = c(0.8, 0.12)) +
    theme(legend.title = element_blank()) +
    theme(legend.background=element_blank()) +
    ylim(c(-50,75)) +
    geom_abline(slope = 0, lty = 2) +
    annotate("text", x = -Inf, y = 0.75*100, hjust = -0.1, label = "(a)") +
    theme(axis.text.x = element_text(face="bold",size=12))
  
  p2 = ggplot(data = error_all, aes(x = Tair-Tleaf, y=100*error, color=Licor_Type)) +
    scale_fill_manual(values = palette_a) +
    scale_colour_manual(values = palette_a) +
    geom_point(size = 0.8) +
    facet_wrap(~ variable, ncol = 2) +
    my_theme + 
    theme(strip.background = element_blank(), strip.text = element_blank()) +
    xlab(expression(paste("Reported air-leaf temperature difference (", degree, "C)  "))) + 
    ylab("Relative error (%)") +
    guides(colour = "none") +
    ylim(c(-0.55*100, 0.75*100)) +
    geom_abline(slope = 0, lty = 2) +
    geom_text(data = dat_text, aes(x = -Inf, y = Inf, label = labels), color = "black", hjust = -0.1, vjust = 1.3, parse=T)
  
  grid.arrange(p1,p2, ncol = 2)
  
  # Close file
  dev.off()
}





make_fig6a = function() {
  source("R/error_prop_6400.R", echo=TRUE)
  error_6400_data = read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/error_rest_6400_subsampled_broadleaf.csv")
  error_est_reg_6400 = est_error_6400_reg_2(error_6400_data, m = 0.7838, b = 0.8092)
  
  source("R/error_prop_6800.R", echo=TRUE)
  error_6800_data <- read.csv("~/Documents/School/UBC - Botany/LICOR gradients/data/error_rest_6800_subsampled_new.csv")
  error_est_reg_6800 = est_error_6800_reg_2(error_6800_data, m = 0.4941, b = -1.1436)
  
  # Bind together
  error_est_reg_6400$Licor_Type = "LI-6400XT"
  error_est_reg_6800$Licor_Type = "LI-6800"
  
  error_all = rbind(error_est_reg_6400, error_est_reg_6800)
  error_all = subset(error_all, error < 1)
  
  # Open file
  pdf("figures/fig6.pdf", width = 7, height = 3.5)
  
  # Make label text
  dat_text <- data.frame(
    labels = c("(b) ~~ italic(g[sw])", "(c) ~~ italic(g[tw])", "(d) ~~ italic(g[tc])", "(e) ~~ italic(C[i])"),
    variable = c("g_sw","g_tw","g_tc","C_i"),
    Tair = 0,
    Tleaf = 0,
    error = 0,
    Licor_Type = "LI-6400XT"
  )
  
  # Build plots
  p1 = ggplot(data = error_all, aes(x = variable, y=100*error, fill=Licor_Type)) + 
    scale_fill_manual(values = palette_a) +
    scale_colour_manual(values = palette_a) +
    geom_boxplot() +
    ylab("Relative error (%)") + xlab(NULL) + #ylim(-0.75, 0.75) + 
    scale_x_discrete(labels=c("g_sw" = expression(italic("g"[sw])), 
                              "g_tw" = expression(italic("g"[tw])), 
                              "g_tc" = expression(italic("g"[tc])),
                              "C_i" = expression(italic("C"[i])))) +
    my_theme +
    theme(legend.position = c(0.8, 0.12)) +
    theme(legend.title = element_blank()) +
    theme(legend.background=element_blank()) +
    ylim(c(-125,50)) +
    geom_abline(slope = 0, lty = 2) +
    annotate("text", x = -Inf, y = 0.75*100, hjust = -0.1, label = "(a)") +
    theme(axis.text.x = element_text(face="bold",size=12))
  
  p2 = ggplot(data = error_all, aes(x = Tair-Tleaf, y=100*error, color=Licor_Type)) +
    scale_fill_manual(values = palette_a) +
    scale_colour_manual(values = palette_a) +
    geom_point(size = 0.8) +
    facet_wrap(~ variable, ncol = 2) +
    my_theme + 
    theme(strip.background = element_blank(), strip.text = element_blank()) +
    xlab(expression(paste("Reported air-leaf temperature difference (", degree, "C)  "))) + 
    ylab("Relative error (%)") +
    guides(colour = "none") +
    ylim(c(-0.55*100, 0.75*100)) +
    geom_abline(slope = 0, lty = 2) +
    geom_text(data = dat_text, aes(x = -Inf, y = Inf, label = labels), color = "black", hjust = -0.1, vjust = 1.3, parse=T)
  
  grid.arrange(p1,p2, ncol = 2)
  
  # Close file
  dev.off()
}
