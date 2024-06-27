
###############################
### VISUALISATION FUNCTIONS ###
###############################

library(gridExtra)
library(ggExtra)

#~~~~~~~~~~~~~~~~~#
# EPIDEMIC CURVES #
#~~~~~~~~~~~~~~~~~#

epidemic_data_reshape <- function(inf.times, rem.times, inf.ids){
  
  ## Create a new data frame of ordered event times ##
  paired_times = data.frame(inf_times = inf.times[inf.ids], rem_times = rem.times[inf.ids])
  paired_ordered_times = paired_times[order(paired_times$inf_times),]
  df_paired_times = data.frame(
    event_times = c(paired_ordered_times[,1], paired_ordered_times[,2]), 
    event_type = as.factor( rep(c(1,2), each = length(inf.ids)) ), 
    id = as.factor( rep( seq( 1, length(inf.ids) ), 2 ))
  )
  df_paired_ordered_times = df_paired_times[order(df_paired_times$event_times),]
  
  
  num_of_events = length(df_paired_ordered_times$event_times)
  ## Create a new data frame of pop size in each state by time
  epi_curves_data = data.frame(t = rep(NA, num_of_events), 
                               S = rep(NA, num_of_events), 
                               I = rep(NA, num_of_events), 
                               R = rep(NA, num_of_events) )
  
  epi_curves_data[1, ] = c(0, 99, 1, 0)
  
  for (i in 2:length(df_paired_ordered_times$event_times)){
    
    epi_curves_data[i, 1] = df_paired_ordered_times$event_times[i]
    
    if (df_paired_ordered_times$event_type[i] == 1){
      epi_curves_data[i, 2:4] = epi_curves_data[i-1, 2:4] + c(-1, +1, 0)
    }
    
    if (df_paired_ordered_times$event_type[i] == 2){
      epi_curves_data[i, 2:4] = epi_curves_data[i-1, 2:4] + c(0, -1, +1)
    }
    
  }
  
  gg_epi_curves_data = data.frame(t = rep(epi_curves_data$t, 3),
                                  pop = c(epi_curves_data$S, epi_curves_data$I, epi_curves_data$R),
                                  state = as.factor(rep(c("S", "I", "R"), each = length(df_paired_ordered_times$event_times))) )%>%
    mutate(state = fct_relevel(state, 
                               "S", "I", "R"))
  
  
  return(gg_epi_curves_data)
}


epidemic_curves_plot <- function(inf.times, rem.times, inf.ids){
  
  gg_epi_curves_data = epidemic_data_reshape(inf.times, rem.times, inf.ids)
  
  epi_curves_plot = ggplot(data=gg_epi_curves_data, aes(x=t, y=pop, colour=state)) +
    geom_step(size = 1.2) + 
    labs( x = "Time", y = "Population" ) + 
    scale_color_manual(name = "State",
                       guide = "legend",
                       values = c("#00BA38", "#F8766D", "#619CFF")) + 
    theme(text = element_text(size = 20)) 
  
  return(epi_curves_plot)
}



#~~~~~~~~~~~~~#
# TRACE PLOTS #
#~~~~~~~~~~~~~#

gg_trace_plot_betas <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], b2 = results[, 3], gamma = results[, 4], d = results[, 5])
  
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


gg_trace_plot_gammad <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], b2 = results[, 3], gamma = results[, 4], d = results[, 5])
  
  num_samples = nrow(df)
  
  df_burn_in <- df %>% 
    mutate(burnin = c(rep("yes", burn_in), rep("no", (num_samples - burn_in))))
  
  gamma_plot = df_burn_in %>% 
    ggplot(aes(x = samples, y = gamma)) +
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
  
  
  trace_plots = grid.arrange(gamma_plot, d_plot, nrow = 2)
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  return(trace_plots)
}


gg_trace_plot_bp <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], p = results[, 6], gamma = results[, 4], d = results[, 5])
  
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
  
  df = data.frame(samples = results[, 1], b = results[, 2], gamma = results[, 3])
  
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
  
  gamma_plot = df_burn_in %>% 
    ggplot(aes(x = samples, y = gamma)) +
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
  
  trace_plots = grid.arrange(beta_plot, gamma_plot, nrow = 2)
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  return(trace_plots)
}


gg_trace_plot_cbhomo <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 3], b = results[, 1], gamma = results[, 2])
  
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
             label = as.expression(bquote(beta~"="~.(params_true[[1]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(x_limits[1], x_limits[2])) +
    labs(
      x = "Sample index", 
      y = "Value", 
      title = expression("Traceplot of parameter samples" ~ beta)) + 
    guides(colour = "none") 
  
  gamma_plot = df_burn_in %>% 
    ggplot(aes(x = samples, y = gamma)) +
    geom_line(size  = 0.2, aes(colour = burnin)) + 
    scale_colour_manual(values = c("dodgerblue", "#fd8f24")) +
    geom_hline(aes(yintercept = params_true[2]), size = 0.7, linetype = 2, 
               colour = '#4f5157') +
    annotate("text", y = annotate_xy[4], x = annotate_xy[3], ##### HERE IS CHANGE VALUE
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma~"="~.(params_true[[2]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(x_limits[3], x_limits[4])) +
    labs(
      x = "Sample index", 
      y = "Value", 
      title = expression("Traceplot of parameter samples" ~ gamma)) + 
    guides(colour = "none") 
  
  trace_plots = grid.arrange(beta_plot, gamma_plot, nrow = 2)
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  return(trace_plots)
}



#~~~~~~~~~~~~#
# HISTOGRAMS #
#~~~~~~~~~~~~#

gg_hist_plot <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], b2 = results[, 3], gamma = results[, 4], d = results[, 5])
  
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
  
  gamma_plot = df_burn_in %>% 
                  ggplot(aes(x = gamma, y = ..density..)) +
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
  
  
  hist_plots = grid.arrange(beta1_plot, beta2_plot, gamma_plot, d_plot, nrow = 2)
  
  return(hist_plots)
}


gg_hist_plot_reparam <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], p = results[, 6], gamma = results[, 4], d = results[, 5])
  
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
                
  gamma_plot = df_burn_in %>% 
                  ggplot(aes(x = gamma, y = ..density..)) +
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
  
  
  hist_plots = grid.arrange(beta1_plot, p_plot, gamma_plot, d_plot, nrow = 2)
  
  return(hist_plots)
}


gg_hist_plot_homo <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b = results[, 2], gamma = results[, 3])
  
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
  
  gamma_plot = df_burn_in %>% 
    ggplot(aes(x = gamma, y = ..density..)) +
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
  
  
  hist_plots = grid.arrange(beta_plot, gamma_plot, nrow = 1)
  
  return(hist_plots)
}


gg_hist_plot_cbhomo <- function(results, params_true, burn_in, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 3], b = results[, 1], gamma = results[, 2])
  
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
             label = as.expression(bquote(beta~"="~.(params_true[[1]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    labs(
      x = expression("Samples of parameter" ~ beta), ##### HERE IS CHANGE SYMBOL
      y = "Density") 
  
  gamma_plot = df_burn_in %>% 
    ggplot(aes(x = gamma, y = ..density..)) +
    geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
    geom_vline(aes(xintercept = params_true[2]), size = 1.1, linetype = 2, 
               colour = '#4f5157') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                       limits = c(x_limits[3], x_limits[4])) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
    ) +
    annotate("text", y = annotate_xy[4], x = annotate_xy[3], ##### HERE IS CHANGE VALUE
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma~"="~.(params_true[[2]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    labs(
      x = expression("Samples of parameter" ~ gamma), ##### HERE IS CHANGE SYMBOL
      y = "Density") 
  
  
  hist_plots = grid.arrange(beta_plot, gamma_plot, nrow = 1)
  
  return(hist_plots)
}



#~~~~~~~~~~~~~~~~~~~~~~#
# HISTOGRAMS w/ PRIORS #
#~~~~~~~~~~~~~~~~~~~~~~#

gg_hist_plot_w_prior <- function(results, params_true, burn_in, priorhp, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], b2 = results[, 3], gamma = results[, 4], d = results[, 5])
  
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
      y = "Density")  +
    stat_function(aes(y = NULL), 
                  fun=dgamma, 
                  args=list(shape=priorhp[1], rate=priorhp[2]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2)
  
  
  
  
  
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
      y = "Density")  +
    stat_function(aes(y = NULL), 
                  fun=dgamma, 
                  args=list(shape=priorhp[3], rate=priorhp[4]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2)
  
  
  
  
  gamma_plot = df_burn_in %>% 
    ggplot(aes(x = gamma, y = ..density..)) +
    geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
    geom_vline(aes(xintercept = params_true[3]), size = 1.1, linetype = 2, 
               colour = '#4f5157') +
    stat_function(aes(y = NULL), 
                  fun=dgamma, 
                  args=list(shape=priorhp[5], rate=priorhp[6]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2) +
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
      y = "Density") +
    stat_function(aes(y = NULL), 
                  fun=dunif, 
                  args=list(min=priorhp[7],  max=priorhp[8]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2)
  
  
  hist_plots = grid.arrange(beta1_plot, beta2_plot, gamma_plot, d_plot, nrow = 2)
  
  return(hist_plots)
}


gg_hist_plot_w_prior_reparam <- function(results, params_true, burn_in, priorhp, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b1 = results[, 2], p = results[, 6], gamma = results[, 4], d = results[, 5])
  
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
      y = "Density")  +
    stat_function(aes(y = NULL), 
                  fun=dgamma, 
                  args=list(shape=priorhp[1], rate=priorhp[2]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2) 
  
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
      y = "Density")  +
    stat_function(aes(y = NULL), 
                  fun=dunif, 
                  args=list(min=priorhp[3],  max=priorhp[4]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2)
  
  gamma_plot = df_burn_in %>% 
    ggplot(aes(x = gamma, y = ..density..)) +
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
      y = "Density") +
    stat_function(aes(y = NULL), 
                  fun=dgamma, 
                  args=list(shape=priorhp[5], rate=priorhp[6]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2)
  
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
      y = "Density") +
    stat_function(aes(y = NULL), 
                  fun=dunif, 
                  args=list(min=priorhp[7],  max=priorhp[8]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2)
  
  
  hist_plots = grid.arrange(beta1_plot, p_plot, gamma_plot, d_plot, nrow = 2)
  
  return(hist_plots)
}


gg_hist_plot_w_prior_homo <- function(results, params_true, burn_in, priorhp, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 1], b = results[, 2], gamma = results[, 3])
  
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
      y = "Density")   +
    stat_function(aes(y = NULL), 
                  fun=dgamma, 
                  args=list(shape=priorhp[1], rate=priorhp[2]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2)
                  
                  
  
  gamma_plot = df_burn_in %>% 
    ggplot(aes(x = gamma, y = ..density..)) +
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
      y = "Density")   +
    stat_function(aes(y = NULL), 
                  fun=dgamma, 
                  args=list(shape=priorhp[1], rate=priorhp[2]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2)
  
  
  hist_plots = grid.arrange(beta_plot, gamma_plot, nrow = 1)
  
  return(hist_plots)
}


gg_hist_plot_w_prior_cbhomo <- function(results, params_true, burn_in, priorhp, annotate_xy, x_limits){
  
  df = data.frame(samples = results[, 3], b = results[, 1], gamma = results[, 2])
  
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
             label = as.expression(bquote(beta~"="~.(params_true[[1]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    labs(
      x = expression("Samples of parameter" ~ beta), ##### HERE IS CHANGE SYMBOL
      y = "Density")  +
    stat_function(aes(y = NULL), 
                  fun=dunif, 
                  args=list(min=priorhp[1], max=priorhp[2]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2) 
  
  gamma_plot = df_burn_in %>% 
    ggplot(aes(x = gamma, y = ..density..)) +
    geom_histogram(fill = "#f5c04a", colour = "grey15", alpha = 0.85) +
    geom_vline(aes(xintercept = params_true[2]), size = 1.1, linetype = 2, 
               colour = '#4f5157') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                       limits = c(x_limits[3], x_limits[4])) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7)
    ) +
    annotate("text", y = annotate_xy[4], x = annotate_xy[3], ##### HERE IS CHANGE VALUE
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma~"="~.(params_true[[2]])))) + ##### HERE IS CHANGE SYMBOL AND VALUE
    labs(
      x = expression("Samples of parameter" ~ gamma), ##### HERE IS CHANGE SYMBOL
      y = "Density")  +
    stat_function(aes(y = NULL), 
                  fun=dunif, 
                  args=list(min=priorhp[3], max=priorhp[4]),
                  colour = "lightblue", geom="area", fill="lightblue", alpha=0.2) 
  
  
  hist_plots = grid.arrange(beta_plot, gamma_plot, nrow = 1)
  
  return(hist_plots)
}


#~~~~~~~~~~~~~~#
# 2D Hex Plots #
#~~~~~~~~~~~~~~#

gg_hex_plot_cbhomo <- function(results, params_true, annotate_xy, x_limits, y_limits){
  
  temp_df = data.frame(samples = results[, 3], beta = results[, 1], gamma = results[, 2])
  
  hexplot_init = ggplot(temp_df, aes(x=beta, y=gamma) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  mean_beta = signif(mean_x, digits = 3)
  mean_gamma = signif(mean_y, digits = 3) 
  
  
  hexplot_init = hexplot_init +
    geom_vline(aes(xintercept = mean_x), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params[1]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params[2]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                       limits = c(x_limits[1], x_limits[2])) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), 
                       limits = c(y_limits[1], y_limits[2]))+ 
    labs(x = expression( ~ beta), 
         y = expression( ~ gamma))+
    annotate("text", y = annotate_xy[2], x = annotate_xy[1],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta^tr~"="~.(params[[1]]))))+
    annotate("text", y = annotate_xy[4], x = annotate_xy[3],
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma^tr~"="~.(params[[2]]))))+
    annotate("text", y = annotate_xy[6], x = annotate_xy[5],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta^est~"="~.(mean_beta[[1]]))))+
    annotate("text", y = annotate_xy[8], x = annotate_xy[7],
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma^est~"="~.(mean_gamma[[1]])))) + 
    theme(text = element_text(size = 20)) 
  
  hex_final <- ggMarginal(hexplot_init, type="histogram", xparams = list(bins=50), yparams = list(bins=50))
  
  return(hex_final)
}


gg_hex_plot_homo <- function(results, params_true, annotate_xy, x_limits, y_limits){
  
  temp_df = data.frame(beta = results[, 2], gamma = results[, 3])
  
  hexplot_init = ggplot(temp_df, aes(x=beta, y=gamma) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  mean_beta = signif(mean_x, digits = 3)
  mean_gamma = signif(mean_y, digits = 3) 
  
  
  hexplot_init = hexplot_init +
    geom_vline(aes(xintercept = mean_x), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params[1]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params[3]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                       limits = c(x_limits[1], x_limits[2])) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), 
                       limits = c(y_limits[1], y_limits[2]))+ 
    labs(x = expression( ~ beta), 
         y = expression( ~ gamma))+
    annotate("text", y = annotate_xy[2], x = annotate_xy[1],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta^tr~"="~.(params[[1]]))))+
    annotate("text", y = annotate_xy[4], x = annotate_xy[3],
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma^tr~"="~.(params[[3]]))))+
    annotate("text", y = annotate_xy[6], x = annotate_xy[5],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta^est~"="~.(mean_beta[[1]]))))+
    annotate("text", y = annotate_xy[8], x = annotate_xy[7],
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma^est~"="~.(mean_gamma[[1]])))) + 
    theme(text = element_text(size = 20)) 
  
  hex_final <- ggMarginal(hexplot_init, type="histogram", xparams = list(bins=50), yparams = list(bins=50))
  
  return(hex_final)
}


gg_hex_plot_beta1_gamma <- function(results, params_true, annotate_xy, x_limits, y_limits){
  
  temp_df = data.frame(beta1 = results[-(1:10000), 2], beta2 = results[-(1:10000), 3], gamma = results[-(1:10000), 4], d = results[-(1:10000), 5], p = results[-(1:10000), 6])
  
  hexplot_init = ggplot(temp_df, aes(x=beta1, y=gamma) ) +
                  geom_point(color = NA, fill = NA) +
                  geom_hex(bins = 50) +
                  scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  mean_beta = signif(mean_x, digits = 3)
  mean_gamma = signif(mean_y, digits = 3) 
  
  
  hexplot_init = hexplot_init +
                    geom_vline(aes(xintercept = mean_x), size = 0.7, linetype = 2, 
                               colour = 'yellow')+
                    geom_hline(aes(yintercept = mean_y), size = 0.7, linetype = 2, 
                               colour = 'yellow') +  
                    geom_vline(aes(xintercept = params[1]), size = 0.7, linetype = 2, 
                               colour = 'red')+
                    geom_hline(aes(yintercept = params[3]), size = 0.7, linetype = 2, 
                               colour = 'red') +
                    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                                       limits = c(x_limits[1], x_limits[2])) +
                    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), 
                                       limits = c(y_limits[1], y_limits[2]))+ 
                    labs(x = expression( ~ beta[1]), 
                         y = expression( ~ gamma))+
                    annotate("text", y = annotate_xy[2], x = annotate_xy[1],
                             parse = TRUE, size = 5,
                             label = as.expression(bquote(beta[1]^tr~"="~.(params[[1]]))))+
                    annotate("text", y = annotate_xy[4], x = annotate_xy[3],
                             parse = TRUE, size = 5,
                             label = as.expression(bquote(gamma^tr~"="~.(params[[3]]))))+
                    annotate("text", y = annotate_xy[6], x = annotate_xy[5],
                             parse = TRUE, size = 5,
                             label = as.expression(bquote(beta[1]^est~"="~.(mean_beta[[1]]))))+
                    annotate("text", y = annotate_xy[8], x = annotate_xy[7],
                             parse = TRUE, size = 5,
                             label = as.expression(bquote(gamma^est~"="~.(mean_gamma[[1]])))) + 
                    theme(text = element_text(size = 20)) 
  
  hex_final <- ggMarginal(hexplot_init, type="histogram", xparams = list(bins=50), yparams = list(bins=50))
  
  return(hex_final)
}


gg_hex_plot_beta2_gamma <- function(results, params_true, annotate_xy, x_limits, y_limits){
  
  temp_df = data.frame(beta1 = results[-(1:10000), 2], beta2 = results[-(1:10000), 3], gamma = results[-(1:10000), 4], d = results[-(1:10000), 5], p = results[-(1:10000), 6])
  
  hexplot_init = ggplot(temp_df, aes(x=beta2, y=gamma) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  mean_beta = signif(mean_x, digits = 3)
  mean_gamma = signif(mean_y, digits = 3) 
  
  
  hexplot_init = hexplot_init +
    geom_vline(aes(xintercept = mean_x), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params[2]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params[3]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                       limits = c(x_limits[1], x_limits[2])) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), 
                       limits = c(y_limits[1], y_limits[2]))+ 
    labs(x = expression( ~ beta[2]), 
         y = expression( ~ gamma))+
    annotate("text", y = annotate_xy[2], x = annotate_xy[1],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[2]^tr~"="~.(params[[2]]))))+
    annotate("text", y = annotate_xy[4], x = annotate_xy[3],
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma^tr~"="~.(params[[3]]))))+
    annotate("text", y = annotate_xy[6], x = annotate_xy[5],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[2]^est~"="~.(mean_beta[[1]]))))+
    annotate("text", y = annotate_xy[8], x = annotate_xy[7],
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma^est~"="~.(mean_gamma[[1]])))) + 
    theme(text = element_text(size = 20)) 
  
  hex_final <- ggMarginal(hexplot_init, type="histogram", xparams = list(bins=50), yparams = list(bins=50))
  
  return(hex_final)
}


gg_hex_plot_beta1_beta2 <- function(results, params_true, annotate_xy, x_limits, y_limits){
  
  temp_df = data.frame(beta1 = results[-(1:10000), 2], beta2 = results[-(1:10000), 3], gamma = results[-(1:10000), 4], d = results[-(1:10000), 5], p = results[-(1:10000), 6])
  
  hexplot_init = ggplot(temp_df, aes(x=beta1, y=beta2) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  mean_beta = signif(mean_x, digits = 3)
  mean_gamma = signif(mean_y, digits = 3) 
  
  
  hexplot_init = hexplot_init +
    geom_vline(aes(xintercept = mean_x), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params[1]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params[2]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                       limits = c(x_limits[1], x_limits[2])) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), 
                       limits = c(y_limits[1], y_limits[2]))+ 
    labs(x = expression( ~ beta[1]), 
         y = expression( ~ beta[2]))+
    annotate("text", y = annotate_xy[2], x = annotate_xy[1],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[1]^tr~"="~.(params[[1]]))))+
    annotate("text", y = annotate_xy[4], x = annotate_xy[3],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[2]^tr~"="~.(params[[2]]))))+
    annotate("text", y = annotate_xy[6], x = annotate_xy[5],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[1]^est~"="~.(mean_beta[[1]]))))+
    annotate("text", y = annotate_xy[8], x = annotate_xy[7],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[2]^est~"="~.(mean_gamma[[1]])))) + 
    theme(text = element_text(size = 20)) 
  
  hex_final <- ggMarginal(hexplot_init, type="histogram", xparams = list(bins=50), yparams = list(bins=50))
  
  return(hex_final)
}


gg_hex_plot_beta1_d <- function(results, params_true, annotate_xy, x_limits, y_limits){
  
  temp_df = data.frame(beta1 = results[-(1:10000), 2], beta2 = results[-(1:10000), 3], gamma = results[-(1:10000), 4], d = results[-(1:10000), 5], p = results[-(1:10000), 6])
  
  hexplot_init = ggplot(temp_df, aes(x=beta1, y=d) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  mean_beta = signif(mean_x, digits = 3)
  mean_gamma = signif(mean_y, digits = 3) 
  
  
  hexplot_init = hexplot_init +
    geom_vline(aes(xintercept = mean_x), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params[1]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params[4]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                       limits = c(x_limits[1], x_limits[2])) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), 
                       limits = c(y_limits[1], y_limits[2]))+ 
    labs(x = expression( ~ beta[1]), 
         y = expression( ~ d))+
    annotate("text", y = annotate_xy[2], x = annotate_xy[1],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[1]^tr~"="~.(params[[1]]))))+
    annotate("text", y = annotate_xy[4], x = annotate_xy[3],
             parse = TRUE, size = 5,
             label = as.expression(bquote(d^tr~"="~.(params[[4]]))))+
    annotate("text", y = annotate_xy[6], x = annotate_xy[5],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[1]^est~"="~.(mean_beta[[1]]))))+
    annotate("text", y = annotate_xy[8], x = annotate_xy[7],
             parse = TRUE, size = 5,
             label = as.expression(bquote(d^est~"="~.(mean_gamma[[1]])))) + 
    theme(text = element_text(size = 20)) 
  
  hex_final <- ggMarginal(hexplot_init, type="histogram", xparams = list(bins=50), yparams = list(bins=50))
  
  return(hex_final)
}


gg_hex_plot_beta2_d <- function(results, params_true, annotate_xy, x_limits, y_limits){
  
  temp_df = data.frame(beta1 = results[-(1:10000), 2], beta2 = results[-(1:10000), 3], gamma = results[-(1:10000), 4], d = results[-(1:10000), 5], p = results[-(1:10000), 6])
  
  hexplot_init = ggplot(temp_df, aes(x=beta2, y=d) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  mean_beta = signif(mean_x, digits = 3)
  mean_gamma = signif(mean_y, digits = 3) 
  
  
  hexplot_init = hexplot_init +
    geom_vline(aes(xintercept = mean_x), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params[2]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params[4]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                       limits = c(x_limits[1], x_limits[2])) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), 
                       limits = c(y_limits[1], y_limits[2]))+ 
    labs(x = expression( ~ beta[2]), 
         y = expression( ~ d))+
    annotate("text", y = annotate_xy[2], x = annotate_xy[1],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[2]^tr~"="~.(params[[2]]))))+
    annotate("text", y = annotate_xy[4], x = annotate_xy[3],
             parse = TRUE, size = 5,
             label = as.expression(bquote(d^tr~"="~.(params[[4]]))))+
    annotate("text", y = annotate_xy[6], x = annotate_xy[5],
             parse = TRUE, size = 5,
             label = as.expression(bquote(beta[2]^est~"="~.(mean_beta[[1]]))))+
    annotate("text", y = annotate_xy[8], x = annotate_xy[7],
             parse = TRUE, size = 5,
             label = as.expression(bquote(d^est~"="~.(mean_gamma[[1]])))) + 
    theme(text = element_text(size = 20)) 
  
  hex_final <- ggMarginal(hexplot_init, type="histogram", xparams = list(bins=50), yparams = list(bins=50))
  
  return(hex_final)
}


gg_hex_plot_d_gamma <- function(results, params_true, annotate_xy, x_limits, y_limits){
  
  temp_df = data.frame(beta1 = results[-(1:10000), 2], beta2 = results[-(1:10000), 3], gamma = results[-(1:10000), 4], d = results[-(1:10000), 5], p = results[-(1:10000), 6])
  
  hexplot_init = ggplot(temp_df, aes(x=d, y=gamma) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  mean_beta = signif(mean_x, digits = 3)
  mean_gamma = signif(mean_y, digits = 3) 
  
  
  hexplot_init = hexplot_init +
    geom_vline(aes(xintercept = mean_x), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params[4]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params[3]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), 
                       limits = c(x_limits[1], x_limits[2])) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), 
                       limits = c(y_limits[1], y_limits[2]))+ 
    labs(x = expression( ~ d), 
         y = expression( ~ gamma))+
    annotate("text", y = annotate_xy[2], x = annotate_xy[1],
             parse = TRUE, size = 5,
             label = as.expression(bquote(d^tr~"="~.(params[[4]]))))+
    annotate("text", y = annotate_xy[4], x = annotate_xy[3],
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma^tr~"="~.(params[[3]]))))+
    annotate("text", y = annotate_xy[6], x = annotate_xy[5],
             parse = TRUE, size = 5,
             label = as.expression(bquote(d^est~"="~.(mean_beta[[1]]))))+
    annotate("text", y = annotate_xy[8], x = annotate_xy[7],
             parse = TRUE, size = 5,
             label = as.expression(bquote(gamma^est~"="~.(mean_gamma[[1]])))) + 
    theme(text = element_text(size = 20)) 
  
  hex_final <- ggMarginal(hexplot_init, type="histogram", xparams = list(bins=50), yparams = list(bins=50))
  
  return(hex_final)
}







#~~~~~~~~~~#
# CONTOURS #
#~~~~~~~~~~#


gg_contour_plot_cbhomo <- function(results, params_true, burn_in, b_limits, d_limits){
  
  # create data frame of results
  
  df = data.frame(samples = results[, 3], b = results[, 1], d = results[, 2])
  
  # Calc number of samples
  
  num_samples = nrow(df)
  
  # Highlight burn in
  
  df_burn_in <- df[-(1:burn_in), ]
  
  
  
  
  # b vs d posterior max
  
  
  hexplot_init = ggplot(df_burn_in, aes(x=b, y=d) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  
  
  
  # b vs d plot
  
  b_d_plot = df_burn_in %>% 
    ggplot(aes(x = b, y = d)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon", h = 0.05) +
    scale_fill_continuous(type = "viridis") +
    geom_vline(aes(xintercept = mean_x1), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y1), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params_true[1]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params_true[2]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(min(df_burn_in$b), max(df_burn_in$b))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(min(df_burn_in$d), max(df_burn_in$d))) +
    coord_cartesian(xlim = c(b_limits[1], b_limits[2]),  ylim = c(d_limits[1], d_limits[2])) +
    labs(
      x = as.expression(bquote(beta)), 
      y = as.expression(bquote(gamma)), 
      title = expression(beta ~ "vs." ~ gamma)) + 
    guides(colour = "none") 
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  
  return(b_d_plot)
}


gg_contour_plot_homo <- function(results, params_true, burn_in, x_limits, y_limits){
  
  # create data frame of results
  
  df = data.frame(b1 = results[, 2], gamma = results[, 3])
  
  # Calc number of samples
  
  num_samples = nrow(df)
  
  # Highlight burn in
  
  df_burn_in <- df[-(1:burn_in), ]
  
  
  
  
  # b1 vs gamma posterior max
  
  
  hexplot_init = ggplot(df_burn_in, aes(x=b1, y=gamma) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  
  
  
  # b1 vs gamma plot
  
  plot = df_burn_in %>% 
    ggplot(aes(x = b1, y = gamma)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon", h = 0.0097) + #0.008
    scale_fill_continuous(type = "viridis") +
    geom_vline(aes(xintercept = mean_x1), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y1), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params_true[1]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params_true[3]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(min(df_burn_in$b1), max(df_burn_in$b1))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(min(df_burn_in$gamma), max(df_burn_in$gamma))) +
    coord_cartesian(xlim = c(x_limits[1], x_limits[2]),  ylim = c(y_limits[1], y_limits[2])) +
    labs(
      x = as.expression(bquote(beta)), 
      y = as.expression(bquote(gamma)), 
      title = expression(beta ~ "vs." ~ gamma)) + 
    guides(colour = "none") 
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  
  return(plot)
}




gg_contour_plot_b1_gamma <- function(results, params_true, burn_in, x_limits, y_limits){
  
  # create data frame of results
  
  df = data.frame(b1 = results[, 2], b2 = results[, 3], gamma = results[, 4], d = results[, 5], p = results[, 6])
  
  # Calc number of samples
  
  num_samples = nrow(df)
  
  # Highlight burn in
  
  df_burn_in <- df[-(1:burn_in), ]
  
  
  
  
  # b1 vs gamma posterior max
  
  
  hexplot_init = ggplot(df_burn_in, aes(x=b1, y=gamma) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  
  
  
  # b1 vs gamma plot
  
  plot = df_burn_in %>% 
    ggplot(aes(x = b1, y = gamma)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon", h = 0.05) +
    scale_fill_continuous(type = "viridis") +
    geom_vline(aes(xintercept = mean_x1), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y1), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params_true[1]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params_true[3]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(min(df_burn_in$b1), max(df_burn_in$b1))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(min(df_burn_in$gamma), max(df_burn_in$gamma))) +
    coord_cartesian(xlim = c(x_limits[1], x_limits[2]),  ylim = c(y_limits[1], y_limits[2])) +
    labs(
      x = as.expression(bquote(beta[1])), 
      y = as.expression(bquote(gamma)), 
      title = expression(beta[1] ~ "vs." ~ gamma)) + 
    guides(colour = "none") 
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  
  return(plot)
}



gg_contour_plot_b1_b2 <- function(results, params_true, burn_in, x_limits, y_limits){
  
  # create data frame of results
  
  df = data.frame(b1 = results[, 2], b2 = results[, 3], gamma = results[, 4], d = results[, 5], p = results[, 6])
  
  # Calc number of samples
  
  num_samples = nrow(df)
  
  # Highlight burn in
  
  df_burn_in <- df[-(1:burn_in), ]
  
  
  
  
  # b1 vs b2 posterior max
  
  
  hexplot_init = ggplot(df_burn_in, aes(x=b1, y=b2) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  
  
  
  # b1 vs gamma plot
  
  plot = df_burn_in %>% 
    ggplot(aes(x = b1, y = b2)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon", h = 0.00265) + # 0.0032
    scale_fill_continuous(type = "viridis") +
    geom_vline(aes(xintercept = mean_x1), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y1), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params_true[1]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params_true[2]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(min(df_burn_in$b1), max(df_burn_in$b1))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(min(df_burn_in$b2), max(df_burn_in$b2))) +
    coord_cartesian(xlim = c(x_limits[1], x_limits[2]),  ylim = c(y_limits[1], y_limits[2])) +
    labs(
      x = as.expression(bquote(beta[1])), 
      y = as.expression(bquote(beta[2])), 
      title = expression(beta[1] ~ "vs." ~ beta[2])) + 
    guides(colour = "none") 
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  
  return(plot)
}



gg_contour_plot_b1_d <- function(results, params_true, burn_in, x_limits, y_limits){
  
  # create data frame of results
  
  df = data.frame(b1 = results[, 2], b2 = results[, 3], gamma = results[, 4], d = results[, 5], p = results[, 6])
  
  # Calc number of samples
  
  num_samples = nrow(df)
  
  # Highlight burn in
  
  df_burn_in <- df[-(1:burn_in), ]
  
  
  
  
  # b1 vs d posterior max
  
  
  hexplot_init = ggplot(df_burn_in, aes(x=b1, y=d) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  
  
  
  # b1 vs d plot
  
  plot = df_burn_in %>% 
    ggplot(aes(x = b1, y = d)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon", h = 0.0035) + #0.0075
    scale_fill_continuous(type = "viridis") +
    geom_vline(aes(xintercept = mean_x1), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y1), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params_true[1]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params_true[4]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(min(df_burn_in$b1), max(df_burn_in$b1))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(min(df_burn_in$d), max(df_burn_in$d))) +
    coord_cartesian(xlim = c(x_limits[1], x_limits[2]),  ylim = c(y_limits[1], y_limits[2])) +
    labs(
      x = as.expression(bquote(beta[1])), 
      y = as.expression(bquote(d)), 
      title = expression(beta[1] ~ "vs." ~ d)) + 
    guides(colour = "none") 
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  
  return(plot)
}



gg_contour_plot_b2_gamma <- function(results, params_true, burn_in, x_limits, y_limits){
  
  # create data frame of results
  
  df = data.frame(b1 = results[, 2], b2 = results[, 3], gamma = results[, 4], d = results[, 5], p = results[, 6])
  
  # Calc number of samples
  
  num_samples = nrow(df)
  
  # Highlight burn in
  
  df_burn_in <- df[-(1:burn_in), ]
  
  
  
  
  # b1 vs gamma posterior max
  
  
  hexplot_init = ggplot(df_burn_in, aes(x=b2, y=gamma) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  
  
  
  # b2 vs gamma plot
  
  plot = df_burn_in %>% 
    ggplot(aes(x = b2, y = gamma)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon", h = 0.00285) + #0.0032
    scale_fill_continuous(type = "viridis") +
    geom_vline(aes(xintercept = mean_x1), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y1), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params_true[2]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params_true[3]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(min(df_burn_in$b2), max(df_burn_in$b2))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(min(df_burn_in$gamma), max(df_burn_in$gamma))) +
    coord_cartesian(xlim = c(x_limits[1], x_limits[2]),  ylim = c(y_limits[1], y_limits[2])) +
    labs(
      x = as.expression(bquote(beta[2])), 
      y = as.expression(bquote(gamma)), 
      title = expression(beta[2] ~ "vs." ~ gamma)) + 
    guides(colour = "none") 
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  
  return(plot)
}



gg_contour_plot_b2_d <- function(results, params_true, burn_in, x_limits, y_limits){
  
  # create data frame of results
  
  df = data.frame(b1 = results[, 2], b2 = results[, 3], gamma = results[, 4], d = results[, 5], p = results[, 6])
  
  # Calc number of samples
  
  num_samples = nrow(df)
  
  # Highlight burn in
  
  df_burn_in <- df[-(1:burn_in), ]
  
  
  
  
  # b1 vs d posterior max
  
  
  hexplot_init = ggplot(df_burn_in, aes(x=b2, y=d) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  
  
  
  # b2 vs d plot
  
  plot = df_burn_in %>% 
    ggplot(aes(x = b2, y = d)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon", h = 0.0025) + #0.0025
    scale_fill_continuous(type = "viridis") +
    geom_vline(aes(xintercept = mean_x1), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y1), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params_true[2]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params_true[4]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(min(df_burn_in$b2), max(df_burn_in$b2))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(min(df_burn_in$d), max(df_burn_in$d))) +
    coord_cartesian(xlim = c(x_limits[1], x_limits[2]),  ylim = c(y_limits[1], y_limits[2])) +
    labs(
      x = as.expression(bquote(beta[2])), 
      y = as.expression(bquote(d)), 
      title = expression(beta[2] ~ "vs." ~ d)) + 
    guides(colour = "none") 
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  
  return(plot)
}



gg_contour_plot_d_gamma <- function(results, params_true, burn_in, x_limits, y_limits){
  
  # create data frame of results
  
  df = data.frame(b1 = results[, 2], b2 = results[, 3], gamma = results[, 4], d = results[, 5], p = results[, 6])
  
  # Calc number of samples
  
  num_samples = nrow(df)
  
  # Highlight burn in
  
  df_burn_in <- df[-(1:burn_in), ]
  
  
  
  
  # b1 vs gamma posterior max
  
  
  hexplot_init = ggplot(df_burn_in, aes(x=d, y=gamma) ) +
    geom_point(color = NA, fill = NA) +
    geom_hex(bins = 50) +
    scale_fill_continuous(type = "viridis")
  
  meta_data = ggplot_build(hexplot_init)$data
  
  mean_x1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$x
  mean_y1 = meta_data[[2]][which.max(meta_data[[2]]$density),]$y
  
  
  
  # d vs gamma plot
  
  plot = df_burn_in %>% 
    ggplot(aes(x = d, y = gamma)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon", h = 0.175) + #0.2
    scale_fill_continuous(type = "viridis") +
    geom_vline(aes(xintercept = mean_x1), size = 0.7, linetype = 2, 
               colour = 'yellow')+
    geom_hline(aes(yintercept = mean_y1), size = 0.7, linetype = 2, 
               colour = 'yellow') +  
    geom_vline(aes(xintercept = params_true[4]), size = 0.7, linetype = 2, 
               colour = 'red')+
    geom_hline(aes(yintercept = params_true[3]), size = 0.7, linetype = 2, 
               colour = 'red') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(min(df_burn_in$d), max(df_burn_in$d))) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 7), limits = c(min(df_burn_in$gamma), max(df_burn_in$gamma))) +
    coord_cartesian(xlim = c(x_limits[1], x_limits[2]),  ylim = c(y_limits[1], y_limits[2])) +
    labs(
      x = as.expression(bquote(d)), 
      y = as.expression(bquote(gamma)), 
      title = expression(d ~ "vs." ~ gamma)) + 
    guides(colour = "none") 
  
  # use ggsave() to save it, e.g.
  # ggsave(filename = "F_augs_trace.png", width = 10, height = 8) ##### HERE IS CHANGE NAME
  
  
  return(plot)
}


