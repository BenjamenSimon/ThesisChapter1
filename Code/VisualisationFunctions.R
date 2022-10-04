
###############################
### VISUALISATION FUNCTIONS ###
###############################

library(gridExtra)

#~~~~~~~~~~~~~#
# TRACE PLOTS #
#~~~~~~~~~~~~~#

gg_trace_plot_betas <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], b2 = results[, 3], g = results[, 4], d = results[, 5])
  
  num_samples = nrow(df)
  
  df_burn_in <- df %>% 
          mutate(burnin = c(rep("yes", burn_in), rep("no", (num_samples - burn_in))))
  
  beta1_plot = df_burn_in %>% 
                  ggplot(aes(x = samples, y = b1)) +
                  geom_line(size  = 0.2, aes(colour = burnin)) + 
                  scale_colour_manual(values = c("dodgerblue", "#fd8f24")) +
                  geom_hline(aes(yintercept = params_true[1]), size = 0.7, linetype = 2, 
                             colour = '#4f5157') +
                  annotate("text", y = annotate_xy[2], x = annotate_xy[1], ##### HERE IS CHANGE VALUE
                           parse = TRUE, size = 5,
                           label = as.expression(bquote(beta[1]~"="~.(params_true[[1]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
                  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(x_limits[1], x_limits[2])) +
                  labs(
                    x = "Sample index", 
                    y = "Value", 
                    title = expression("Traceplot of parameter samples" ~ beta[1])) + 
                  guides(colour = "none") 
  
  beta2_plot = df_burn_in %>% 
                  ggplot(aes(x = samples, y = b2)) +
                  geom_line(size  = 0.2, aes(colour = burnin)) + 
                  scale_colour_manual(values = c("dodgerblue", "#fd8f24")) +
                  geom_hline(aes(yintercept = params_true[2]), size = 0.7, linetype = 2, 
                             colour = '#4f5157') +
                  annotate("text", y = annotate_xy[4], x = annotate_xy[3], ##### HERE IS CHANGE VALUE
                           parse = TRUE, size = 5,
                           label = as.expression(bquote(beta[2]~"="~.(params_true[[2]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
                  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(x_limits[3], x_limits[4])) +
                  labs(
                    x = "Sample index", 
                    y = "Value", 
                    title = expression("Traceplot of parameter samples" ~ beta[2])) + 
                  guides(colour = "none") 
  
  trace_plots = grid.arrange(beta1_plot, beta2_plot, nrow = 2)

  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
 return(trace_plots)
}


gg_trace_plot_gd <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], b2 = results[, 3], g = results[, 4], d = results[, 5])
  
  num_samples = nrow(df)
  
  df_burn_in <- df %>% 
    mutate(burnin = c(rep("yes", burn_in), rep("no", (num_samples - burn_in))))
  
  g_plot = df_burn_in %>% 
    ggplot(aes(x = samples, y = g)) +
    geom_line(size  = 0.2, aes(colour = burnin)) + 
    scale_colour_manual(values = c("dodgerblue", "#fd8f24")) +
    geom_hline(aes(yintercept = params_true[3]), size = 0.7, linetype = 2, 
               colour = '#4f5157') +
    annotate("text", y = annotate_xy[2], x = annotate_xy[1], ##### HERE IS CHANGE VALUE
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma~"="~.(params_true[[3]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(x_limits[1], x_limits[2])) +
    labs(
      x = "Sample index", 
      y = "Value", 
      title = expression("Traceplot of parameter samples" ~ gamma)) + 
    guides(colour = "none") 
  
  d_plot = df_burn_in %>% 
    ggplot(aes(x = samples, y = d)) +
    geom_line(size  = 0.2, aes(colour = burnin)) + 
    scale_colour_manual(values = c("dodgerblue", "#fd8f24")) +
    geom_hline(aes(yintercept = params_true[4]), size = 0.7, linetype = 2, 
               colour = '#4f5157') +
    annotate("text", y = annotate_xy[4], x = annotate_xy[3], ##### HERE IS CHANGE VALUE
             parse = TRUE, size = 5,
             label = as.expression(bquote(d~"="~.(params_true[[4]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(x_limits[3], x_limits[4])) +
    labs(
      x = "Sample index", 
      y = "Value", 
      title = expression("Traceplot of parameter samples" ~ d)) + 
    guides(colour = "none") 
  
  
  trace_plots = grid.arrange(g_plot, d_plot, nrow = 2)
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  return(trace_plots)
}


gg_trace_plot_bp <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], p = results[, 6], g = results[, 4], d = results[, 5])
  
  num_samples = nrow(df)
  
  df_burn_in <- df %>% 
    mutate(burnin = c(rep("yes", burn_in), rep("no", (num_samples - burn_in))))
  
  beta1_plot = df_burn_in %>% 
    ggplot(aes(x = samples, y = b1)) +
    geom_line(size  = 0.2, aes(colour = burnin)) + 
    scale_colour_manual(values = c("dodgerblue", "#fd8f24")) +
    geom_hline(aes(yintercept = params_true[1]), size = 0.7, linetype = 2, 
               colour = '#4f5157') +
    annotate("text", y = annotate_xy[2], x = annotate_xy[1], ##### HERE IS CHANGE VALUE
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[1]~"="~.(params_true[[1]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(x_limits[1], x_limits[2])) +
    labs(
      x = "Sample index", 
      y = "Value", 
      title = expression("Traceplot of parameter samples" ~ beta[1])) + 
    guides(colour = "none") 
  
  p_plot = df_burn_in %>% 
    ggplot(aes(x = samples, y = p)) +
    geom_line(size  = 0.2, aes(colour = burnin)) + 
    scale_colour_manual(values = c("dodgerblue", "#fd8f24")) +
    geom_hline(aes(yintercept = params_true[5]), size = 0.7, linetype = 2, 
               colour = '#4f5157') +
    annotate("text", y = annotate_xy[4], x = annotate_xy[3], ##### HERE IS CHANGE VALUE
             parse = TRUE, size = 5,
             label = as.expression(bquote(p~"="~.(params_true[[5]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(x_limits[3], x_limits[4])) +
    labs(
      x = "Sample index", 
      y = "Value", 
      title = expression("Traceplot of parameter samples" ~ p)) + 
    guides(colour = "none") 
  
  trace_plots = grid.arrange(beta1_plot, p_plot, nrow = 2)
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  return(trace_plots)
}


gg_trace_plot_homo <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b = results[, 2], g = results[, 3])
  
  num_samples = nrow(df)
  
  df_burn_in <- df %>% 
    mutate(burnin = c(rep("yes", burn_in), rep("no", (num_samples - burn_in))))
  
  beta_plot = df_burn_in %>% 
    ggplot(aes(x = samples, y = b)) +
    geom_line(size  = 0.2, aes(colour = burnin)) + 
    scale_colour_manual(values = c("dodgerblue", "#fd8f24")) +
    geom_hline(aes(yintercept = params_true[1]), size = 0.7, linetype = 2, 
               colour = '#4f5157') +
    annotate("text", y = annotate_xy[2], x = annotate_xy[1], ##### HERE IS CHANGE VALUE
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[1]~"="~.(params_true[[1]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(x_limits[1], x_limits[2])) +
    labs(
      x = "Sample index", 
      y = "Value", 
      title = expression("Traceplot of parameter samples" ~ beta)) + 
    guides(colour = "none") 
  
  g_plot = df_burn_in %>% 
    ggplot(aes(x = samples, y = g)) +
    geom_line(size  = 0.2, aes(colour = burnin)) + 
    scale_colour_manual(values = c("dodgerblue", "#fd8f24")) +
    geom_hline(aes(yintercept = params_true[3]), size = 0.7, linetype = 2, 
               colour = '#4f5157') +
    annotate("text", y = annotate_xy[4], x = annotate_xy[3], ##### HERE IS CHANGE VALUE
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma~"="~.(params_true[[3]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(x_limits[3], x_limits[4])) +
    labs(
      x = "Sample index", 
      y = "Value", 
      title = expression("Traceplot of parameter samples" ~ gamma)) + 
    guides(colour = "none") 
  
  trace_plots = grid.arrange(beta_plot, g_plot, nrow = 2)
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  return(trace_plots)
}



#~~~~~~~~~~~~#
# HISTOGRAMS #
#~~~~~~~~~~~~#

gg_hist_plot <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], b2 = results[, 3], g = results[, 4], d = results[, 5])
  
  num_samples = nrow(df)
  
  df_burn_in <- df %>% 
    mutate(burnin = c(rep("yes", burn_in), rep("no", (num_samples - burn_in))))
  
  beta1_plot = df_burn_in %>% 
                  ggplot(aes(x = b1, y = ..density..)) +
                  geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
                  geom_vline(aes(xintercept = params_true[1]), size = 1.1, linetype = 2, 
                             colour = '#4f5157') +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                                     limits = c(x_limits[1], x_limits[2])) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
                  ) +
                  annotate("text", y = annotate_xy[2], x = annotate_xy[1], ##### HERE IS CHANGE VALUE
                           parse = TRUE, size = 5,
                           label = as.expression(bquote(beta[1]~"="~.(params_true[[1]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
                  labs(
                    x = expression("Samples of parameter" ~ beta[1]), ##### HERE IS CHANGE SYMBOL
                    y = "Density") 
  
  beta2_plot = df_burn_in %>% 
                  ggplot(aes(x = b2, y = ..density..)) +
                  geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
                  geom_vline(aes(xintercept = params_true[2]), size = 1.1, linetype = 2, 
                             colour = '#4f5157') +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                                     limits = c(x_limits[3], x_limits[4])) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
                  ) +
                  annotate("text", y = annotate_xy[4], x = annotate_xy[3], ##### HERE IS CHANGE VALUE
                           parse = TRUE, size = 5,
                           label = as.expression(bquote(beta[2]~"="~.(params_true[[2]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
                  labs(
                    x = expression("Samples of parameter" ~ beta[2]), ##### HERE IS CHANGE SYMBOL
                    y = "Density") 
  
  g_plot = df_burn_in %>% 
                  ggplot(aes(x = g, y = ..density..)) +
                  geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
                  geom_vline(aes(xintercept = params_true[3]), size = 1.1, linetype = 2, 
                             colour = '#4f5157') +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                                     limits = c(x_limits[5], x_limits[6])) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
                  ) +
                  annotate("text", y = annotate_xy[6], x = annotate_xy[5], ##### HERE IS CHANGE VALUE
                           parse = TRUE, size = 5,
                           label = as.expression(bquote(gamma~"="~.(params_true[[3]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
                  labs(
                    x = expression("Samples of parameter" ~ gamma), ##### HERE IS CHANGE SYMBOL
                    y = "Density") 
  
  d_plot = df_burn_in %>% 
                  ggplot(aes(x = d, y = ..density..)) +
                  geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
                  geom_vline(aes(xintercept = params_true[4]), size = 1.1, linetype = 2, 
                             colour = '#4f5157') +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                                     limits = c(x_limits[7], x_limits[8])) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
                  ) +
                  annotate("text", y = annotate_xy[8], x = annotate_xy[7], ##### HERE IS CHANGE VALUE
                           parse = TRUE, size = 5,
                           label = as.expression(bquote(d~"="~.(params_true[[4]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
                  labs(
                    x = expression("Samples of parameter" ~ d), ##### HERE IS CHANGE SYMBOL
                    y = "Density") 
  
  
  hist_plots = grid.arrange(beta1_plot, beta2_plot, g_plot, d_plot, nrow = 2)
  
  return(hist_plots)
}


gg_hist_plot_reparam <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], p = results[, 6], g = results[, 4], d = results[, 5])
  
  num_samples = nrow(df)
  
  df_burn_in <- df %>% 
    mutate(burnin = c(rep("yes", burn_in), rep("no", (num_samples - burn_in))))
  
  beta1_plot = df_burn_in %>% 
                  ggplot(aes(x = b1, y = ..density..)) +
                  geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
                  geom_vline(aes(xintercept = params_true[1]), size = 1.1, linetype = 2, 
                             colour = '#4f5157') +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                                     limits = c(x_limits[1], x_limits[2])) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
                  ) +
                  annotate("text", y = annotate_xy[2], x = annotate_xy[1], ##### HERE IS CHANGE VALUE
                           parse = TRUE, size = 5,
                           label = as.expression(bquote(beta[1]~"="~.(params_true[[1]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
                  labs(
                    x = expression("Samples of parameter" ~ beta[1]), ##### HERE IS CHANGE SYMBOL
                    y = "Density") 
  
  p_plot = df_burn_in %>% 
                  ggplot(aes(x = p, y = ..density..)) +
                  geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
                  geom_vline(aes(xintercept = params_true[5]), size = 1.1, linetype = 2, 
                             colour = '#4f5157') +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                                     limits = c(x_limits[3], x_limits[4])) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
                  ) +
                  annotate("text", y = annotate_xy[4], x = annotate_xy[3], ##### HERE IS CHANGE VALUE
                           parse = TRUE, size = 5,
                           label = as.expression(bquote(p~"="~.(params_true[[5]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
                  labs(
                    x = expression("Samples of parameter" ~ p), ##### HERE IS CHANGE SYMBOL
                    y = "Density") 
                
  g_plot = df_burn_in %>% 
                  ggplot(aes(x = g, y = ..density..)) +
                  geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
                  geom_vline(aes(xintercept = params_true[3]), size = 1.1, linetype = 2, 
                             colour = '#4f5157') +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                                     limits = c(x_limits[5], x_limits[6])) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
                  ) +
                  annotate("text", y = annotate_xy[6], x = annotate_xy[5], ##### HERE IS CHANGE VALUE
                           parse = TRUE, size = 5,
                           label = as.expression(bquote(gamma~"="~.(params_true[[3]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
                  labs(
                    x = expression("Samples of parameter" ~ gamma), ##### HERE IS CHANGE SYMBOL
                    y = "Density") 
                
  d_plot = df_burn_in %>% 
                  ggplot(aes(x = d, y = ..density..)) +
                  geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
                  geom_vline(aes(xintercept = params_true[4]), size = 1.1, linetype = 2, 
                             colour = '#4f5157') +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                                     limits = c(x_limits[7], x_limits[8])) +
                  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
                  ) +
                  annotate("text", y = annotate_xy[8], x = annotate_xy[7], ##### HERE IS CHANGE VALUE
                           parse = TRUE, size = 5,
                           label = as.expression(bquote(d~"="~.(params_true[[4]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
                  labs(
                    x = expression("Samples of parameter" ~ d), ##### HERE IS CHANGE SYMBOL
                    y = "Density") 
  
  
  hist_plots = grid.arrange(beta1_plot, p_plot, g_plot, d_plot, nrow = 2)
  
  return(hist_plots)
}


gg_hist_plot_homo <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b = results[, 2], g = results[, 3])
  
  num_samples = nrow(df)
  
  df_burn_in <- df %>% 
    mutate(burnin = c(rep("yes", burn_in), rep("no", (num_samples - burn_in))))
  
  beta_plot = df_burn_in %>% 
    ggplot(aes(x = b, y = ..density..)) +
    geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
    geom_vline(aes(xintercept = params_true[1]), size = 1.1, linetype = 2, 
               colour = '#4f5157') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                       limits = c(x_limits[1], x_limits[2])) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
    ) +
    annotate("text", y = annotate_xy[2], x = annotate_xy[1], ##### HERE IS CHANGE VALUE
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[1]~"="~.(params_true[[1]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    labs(
      x = expression("Samples of parameter" ~ beta[1]), ##### HERE IS CHANGE SYMBOL
      y = "Density") 
  
  g_plot = df_burn_in %>% 
    ggplot(aes(x = g, y = ..density..)) +
    geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
    geom_vline(aes(xintercept = params_true[3]), size = 1.1, linetype = 2, 
               colour = '#4f5157') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                       limits = c(x_limits[3], x_limits[4])) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
    ) +
    annotate("text", y = annotate_xy[4], x = annotate_xy[3], ##### HERE IS CHANGE VALUE
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma~"="~.(params_true[[3]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    labs(
      x = expression("Samples of parameter" ~ gamma), ##### HERE IS CHANGE SYMBOL
      y = "Density") 
  
  
  hist_plots = grid.arrange(beta_plot, g_plot, nrow = 1)
  
  return(hist_plots)
}
