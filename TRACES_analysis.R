# upload file
traceB_HE_file <- readLines("~/Desktop/Università Daniele/Third Year/Computer systems modelling and semantic web/ASSIGNMENT/TraceFilesbigLITTLE/TraceB-HE.txt")
traceB_HH_file <- readLines("~/Desktop/Università Daniele/Third Year/Computer systems modelling and semantic web/ASSIGNMENT/TraceFilesbigLITTLE/TraceB-HH.txt")
traceB_LE_file <- readLines("~/Desktop/Università Daniele/Third Year/Computer systems modelling and semantic web/ASSIGNMENT/TraceFilesbigLITTLE/TraceB-LE.txt")
traceB_LH_file <- readLines("~/Desktop/Università Daniele/Third Year/Computer systems modelling and semantic web/ASSIGNMENT/TraceFilesbigLITTLE/TraceB-LH.txt")
traceB_HE <- as.data.frame(traceB_HE_file)
traceB_HH <- as.data.frame(traceB_HH_file)
traceB_LE <- as.data.frame(traceB_LE_file)
traceB_LH <- as.data.frame(traceB_LH_file)
# Save the dataset cvc to a CSV file in the working directory
write.csv(traceB_HE, "traceB_HE.csv", row.names = FALSE)

#convert values to numerical
traceB_HE[] <- lapply(traceB_HE, function(x) as.numeric(as.character(x)))
traceB_HH[] <- lapply(traceB_HH, function(x) as.numeric(as.character(x)))
traceB_LE[] <- lapply(traceB_LE, function(x) as.numeric(as.character(x)))
traceB_LH[] <- lapply(traceB_LH, function(x) as.numeric(as.character(x)))

#PAXCKAGES FOR STAT SUMMARY
library(psych)

# SUMMARY FO VARIABLE
SUM_B_HE <- describe(traceB_HE)
SUM_B_HH <- describe(traceB_HH)
SUM_B_LE <- describe(traceB_LE)
SUM_B_LH <- describe(traceB_LH)

# coefficient of variation
cv_B_HE <- SUM_B_HE$sd / SUM_B_HE$mean
cv_B_HH <- SUM_B_HH$sd / SUM_B_HH$mean
cv_B_LE <- SUM_B_LE$sd / SUM_B_LE$mean
cv_B_LH <- SUM_B_LH$sd / SUM_B_LH$mean


library(ggplot2)
library(patchwork)
# PDF and CDF for traces
PDF_CDF_plot <- function(data, column_name, title_1) {
  PDF <- ggplot(data, aes_string(x = column_name)) +
    geom_density(aes(y = ..scaled..), fill = "skyblue", alpha = 0.5) +  
    labs(title = title_1, subtitle = "PDF", x = "Values", y = "Scaled Density") + # Scaled PDF 
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 10), # Size of x-axis title
      axis.title.y = element_text(size = 10))  # Size of y-axis title
  
  CDF <- ggplot(data, aes_string(x = column_name)) +
    stat_ecdf(geom = "step", color = "red", size = 1) + #CDF
    labs(subtitle = "CDF", x = "Values", y = "Cumulative Probability") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.x = element_text(size = 10), # Size of x-axis title
      axis.title.y = element_text(size = 10))  # Size of y-axis title
  
  list(PDF = PDF, CDF = CDF)
}

#TRACE B_HE
PDF_CDF_B_HE <- PDF_CDF_plot(traceB_HE, traceB_HE$traceB_HE_file, "traceB-HE")
#TRACE B_HH
PDF_CDF_B_HH <- PDF_CDF_plot(traceB_HH, traceB_HH$traceB_HH_file, "traceB-HH")
#TRACE B_LH
PDF_CDF_B_LH <- PDF_CDF_plot(traceB_LH, traceB_LH$traceB_LH_file, "traceB-LH")
#TRACE B_LE
PDF_CDF_B_LE <- PDF_CDF_plot(traceB_LE, traceB_LE$traceB_LE_file, "traceB-LE")

# grid of PDF AND CDF
plots <- list(PDF_CDF_B_HH$PDF, PDF_CDF_B_HH$CDF, PDF_CDF_B_HE$PDF, PDF_CDF_B_HE$CDF, PDF_CDF_B_LH$PDF, 
              PDF_CDF_B_LH$CDF, PDF_CDF_B_LE$PDF, PDF_CDF_B_LE$CDF)
grid <- wrap_plots(plots, ncol = 4, nrow = 2)
print(grid)

png("plot_high_quality.png", width = 3000, height = 1500, res = 300)
print(grid)  # Replace with your ggplot object
dev.off()


library(ggplot2)
library(patchwork)
library(distributions3) # for erlang distribution
library(Distributacalcul) # for erlang distribution

PDF_CDF_plot <- function(data, column_name, title_1, params) {
  x_values <- seq(max(0, min(data[[column_name]])), max(data[[column_name]]), length.out = 500)
  plots <- list()
  
  # Iterate through all distributions
  for (dist_name in names(params)) {
    dist_params <- params[[dist_name]]
    
    
    theoretical_pdf <- switch(
      dist_name,
      Gamma = dgamma(x_values, shape = dist_params$shape, scale = dist_params$scale),
      Normal = dnorm(x_values, mean = dist_params$mean, sd = dist_params$sd),
      HyperExponential = dist_params$prob * dexp(x_values, rate = dist_params$rate1) + 
        (1 - dist_params$prob) * dexp(x_values, rate = dist_params$rate2),
      HypoExponential = Reduce(`*`, lapply(dist_params$rates, function(rate) dexp(x_values, rate))),
      Erlang = dErlang(x_values, shape = dist_params$shape, rate = dist_params$rate),  # Use dErlang correctly
      Weibull = dweibull(x_values, shape = dist_params$shape, scale = dist_params$scale)
    )
    
    theoretical_cdf <- switch(
      dist_name,
      Gamma = pgamma(x_values, shape = dist_params$shape, scale = dist_params$scale),
      Normal = pnorm(x_values, mean = dist_params$mean, sd = dist_params$sd),
      HyperExponential = dist_params$prob * pexp(x_values, rate = dist_params$rate1) + 
        (1 - dist_params$prob) * pexp(x_values, rate = dist_params$rate2),
      HypoExponential = 1 - Reduce(`*`, lapply(dist_params$rates, function(rate) 1 - pexp(x_values, rate))),
      Erlang = pErlang(x_values, shape = dist_params$shape, rate = dist_params$rate),  # Use pErlang correctly
      Weibull = pweibull(x_values, shape = dist_params$shape, scale = dist_params$scale)
    )
    
    
    # Data frames for theoretical lines
    theoretical_df <- data.frame(x = x_values, pdf = theoretical_pdf, cdf = theoretical_cdf)
    
    # PDF plot
    PDF <- ggplot(data, aes_string(x = column_name)) +
      geom_density(aes(y = ..scaled..), fill = "skyblue", alpha = 0.5) +
      geom_line(data = data.frame(x = x_values, pdf = theoretical_pdf), aes(x = x, y = pdf), color = "red") +
      labs(title = paste(title_1, "- PDF", dist_name), x = "Values", y = "Density") +
      theme_minimal()
    
    # CDF plot
    CDF <- ggplot(data, aes_string(x = column_name)) +
      stat_ecdf(geom = "step", color = "blue", size = 2) +
      geom_line(data = theoretical_df, aes(x = x, y = cdf), color = "red") +
      labs(title = paste(title_1, "- CDF", dist_name), x = "Values", y = "Cumulative Probability") +
      theme_minimal()
    
    # Store plots
    plots[[paste0(dist_name, "_PDF")]] <- PDF
    plots[[paste0(dist_name, "_CDF")]] <- CDF
  }
  
  # Combine all plots into a grid
  combined_plot <- wrap_plots(plots, ncol = 2, nrow = 6)
  return(combined_plot)
}


# Parameters for theoretical distributions TraceB-LH
params_LH <- list(
  Gamma = list(shape = 12.016235, scale = 0.1250389),
  HypoExponential = list(rates = c(1.331118, 0.166390)),
  Erlang = list(shape = 12, rate = 7.99751),
  Normal = list(mean = 1.502497, sd = 0.4334404),
  Weibull = list(shape = 3.911736, scale = 1.659746)
)

# ALL PLOT OF traceB-LH
PDF_CDF_plot_LH <- PDF_CDF_plot(traceB_LH, "traceB_LH_file", "traceB-LH", params_LH)
print(PDF_CDF_plot_LH)

# Parameters for theoretical distributions TraceB-LE
params_LE <- list(
  Gamma = list(shape = 12.257007, scale = 0.394983),
  HypoExponential = list(rates = c(0.416308, 0.052039)),
  Erlang = list(shape = 12, rate = 2.5123115),
  Normal = list(mean = 4.804131, sd = 1.382836),
  Weibull = list(shape = 3.866847, scale = 5.310372)
)

# ALL PLOT OF traceB-LE
PDF_CDF_plot_LE <- PDF_CDF_plot(traceB_LE, "traceB_LE_file", "traceB-LE", params_LE)
print(PDF_CDF_plot_LE)

# Parameters for theoretical distributions TraceB-HH
params_HH <- list(
  Gamma = list(shape = 0.469854, scale = 51.346823),
  HyperExponential = list(rate1 = 0.016557, rate2 = 0.066343, prob = 0.199717),
  Erlang = list(shape = 1, rate = 0.04145),
  Normal = list(mean = 24.12551, sd = 35.19614),
  Weibull = list(shape = 18.058682, scale = 0.663554)
)

# ALL PLOT OF traceB-HH
PDF_CDF_plot_HH <- PDF_CDF_plot(traceB_HH, "traceB_HH_file", "traceB-HH", params_HH)
print(PDF_CDF_plot_HH)

# Parameters for theoretical distributions TraceB-HE
params_HE <- list(
  Gamma = list(shape = 0.463838, scale = 208.1773),
  HyperExponential = list(rate1 = 0.004089, rate2 = 0.016624, prob = 0.197389),
  Erlang = list(shape = 1, rate = 0.010356),
  Normal = list(mean = 96.56053, sd = 141.7805),
  Weibull = list(shape = 71.73806, scale = 0.658926)
)

# ALL PLOT OF traceB-HE
PDF_CDF_plot_HE <- PDF_CDF_plot(traceB_HE, "traceB_HE_file", "traceB-HE", params_HE)
print(PDF_CDF_plot_HE)



# Function to compute theoretical quantiles and generate Q-Q plot
generate_qq_plot <- function(sample_data, dist_type, params) {
  
  # Helper function for HyperExponential distribution
  HyperExponential <- function(p, rate1, rate2, prob) {
    q1 <- qexp(p, rate = rate1)
    q2 <- qexp(p, rate = rate2)
    prob * q1 + (1 - prob) * q2
  }
  
  # Helper function for HypoExponential quantiles
  hypo_exp_quantiles <- function(p, rates) {
    # Approximate HypoExponential quantiles using a numerical approach
    n <- length(rates)
    quantiles <- sapply(p, function(pp) {
      f <- function(x) {
        1 - prod(1 - pexp(x, rate = rates)) - pp
      }
      uniroot(f, lower = 0, upper = 10^5)$root
    })
    return(quantiles)
  }
  
  # Number of data points
  n <- length(sample_data)
  
  # Compute theoretical quantiles based on the distribution type
  if (dist_type == "Gamma") {
    theoretical_quantiles <- qgamma(ppoints(n), shape = params$shape, scale = params$scale)
  } else if (dist_type == "Normal") {
    theoretical_quantiles <- qnorm(ppoints(n), mean = params$mean, sd = params$sd)
  } else if (dist_type == "Weibull") {
    theoretical_quantiles <- qweibull(ppoints(n), shape = params$shape, scale = params$scale)
  } else if (dist_type == "Erlang") {
    theoretical_quantiles <- quantile(Erlang(k = params$shape, lambda = params$rate), ppoints(n))
  } else if (dist_type == "HyperExponential") {
    theoretical_quantiles <- HyperExponential(ppoints(n), 
                                              rate1 = params$rate1, 
                                              rate2 = params$rate2, 
                                              prob = params$prob)
  } else if (dist_type == "HypoExponential") {
    theoretical_quantiles <- hypo_exp_quantiles(ppoints(n), rates = params$rates)
  } else {
    stop("Unsupported distribution type.")
  }
  
  # Create the Q-Q plot
  ggplot(data = data.frame(Sample = sample_data, Theoretical = theoretical_quantiles), 
         aes(x = Theoretical, y = Sample)) +
    geom_point(color = "blue") +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(
      x = "Theoretical Quantiles",
      y = "Sample Quantiles",
      title = paste("Q-Q Plot for", dist_type, "Distribution")
    ) +
    theme_minimal()
}


# Function to generate plots for a given dataset and return them in a list
generate_plots <- function(data, params, prefix, PDF_CDF_plots = NULL) {
  dist_types <- names(params)
  plots <- list()
  
  for (i in seq_along(dist_types)) {
    dist_name <- dist_types[i]
    qq_plot <- generate_qq_plot(sort(data), dist_type = dist_name, params = params[[dist_name]])
    
    # Combine PDF, CDF, and Q-Q plots (if PDF_CDF_plots is provided)
    if (!is.null(PDF_CDF_plots)) {
      combined_plot <- PDF_CDF_plots[[2 * i - 1]] + PDF_CDF_plots[[2 * i]] + qq_plot
      plots[[paste(prefix, dist_name, "combined", sep = "_")]] <- combined_plot
    }
    plots[[paste(prefix, dist_name, "qq", sep = "_")]] <- qq_plot
  }
  
  return(plots)
}

# Generate plots and assign them to variables
plots_LH <- generate_plots(traceB_LH$traceB_LH_file, params_LH, "LH", PDF_CDF_plot_LH)
plots_LE <- generate_plots(traceB_LE$traceB_LE_file, params_LE, "LE", PDF_CDF_plot_LE)
plots_HH <- generate_plots(traceB_HH$traceB_HH_file, params_HH, "HH", PDF_CDF_plot_HH) 
plots_HE <- generate_plots(traceB_HE$traceB_HE_file, params_HE, "HE", PDF_CDF_plot_HE) 
plots_HH$HH_Gamma_combined
plots_HH$HH_HyperExponential_combined
plots_HH$HH_Erlang_combined
plots_HH$HH_Normal_combined
plots_HH$HH_Weibull_combined




PDF_CDF_plot_2 <- function(data, column_name, title_1, params) {
  # Generate x_values for theoretical distributions
  x_values <- seq(max(0, min(data[[column_name]], na.rm = TRUE)), max(data[[column_name]], na.rm = TRUE), length.out = 500)
  
  # Initialize data frames for PDFs and CDFs
  densities <- data.frame(x = x_values)
  cdfs <- data.frame(x = x_values)
  
  # Iterate through all distributions
  for (dist_name in names(params)) {
    dist_params <- params[[dist_name]]
    
    # Compute theoretical PDF and CDF
    theoretical_pdf <- switch(
      dist_name,
      Gamma = dgamma(x_values, shape = dist_params$shape, scale = dist_params$scale),
      Normal = dnorm(x_values, mean = dist_params$mean, sd = dist_params$sd),
      HyperExponential = dist_params$prob * dexp(x_values, rate = dist_params$rate1) +
        (1 - dist_params$prob) * dexp(x_values, rate = dist_params$rate2),
      HypoExponential = Reduce(`*`, lapply(dist_params$rates, function(rate) dexp(x_values, rate))),
      Erlang = dgamma(x_values, shape = dist_params$shape, rate = dist_params$rate),
      Weibull = dweibull(x_values, shape = dist_params$shape, scale = dist_params$scale),
      rep(NA, length(x_values)) # Fallback if dist_name is not matched
    )
    
    theoretical_cdf <- switch(
      dist_name,
      Gamma = pgamma(x_values, shape = dist_params$shape, scale = dist_params$scale),
      Normal = pnorm(x_values, mean = dist_params$mean, sd = dist_params$sd),
      HyperExponential = dist_params$prob * pexp(x_values, rate = dist_params$rate1) +
        (1 - dist_params$prob) * pexp(x_values, rate = dist_params$rate2),
      HypoExponential = 1 - Reduce(`*`, lapply(dist_params$rates, function(rate) 1 - pexp(x_values, rate))),
      Erlang = pgamma(x_values, shape = dist_params$shape, rate = dist_params$rate),
      Weibull = pweibull(x_values, shape = dist_params$shape, scale = dist_params$scale),
      rep(NA, length(x_values)) # Fallback if dist_name is not matched
    )
    
    # Add results to data frames
    densities[[dist_name]] <- theoretical_pdf
    cdfs[[dist_name]] <- theoretical_cdf
  }
  
  # PDF Plot
  PDF <- ggplot(data, aes_string(x = column_name)) +
    geom_density(aes(y = ..scaled..), fill = "skyblue", alpha = 0.5) +
    geom_line(data = tidyr::pivot_longer(densities, -x, names_to = "Distribution", values_to = "Density"),
              aes(x = x, y = Density, color = Distribution), size = 1) +
    scale_color_manual(values = c("Gamma" = "red", "HyperExponential" = "green", 
                                  "HypoExponential" = "orange", "Erlang" = "purple",
                                  "Normal" = "blue", "Weibull" = "brown")) +
    labs(title = title_1, subtitle = "PDF", x = "Values", y = "Scaled Density", color = "Distributions") +
    theme_minimal()
  
  # CDF Plot
  CDF <- ggplot(data, aes_string(x = column_name)) +
    stat_ecdf(geom = "step", color = "black", size = 2) +
    geom_line(data = tidyr::pivot_longer(cdfs, -x, names_to = "Distribution", values_to = "CDF"),
              aes(x = x, y = CDF, color = Distribution), size = 1) +
    scale_color_manual(values = c("Gamma" = "red", "HyperExponential" = "green", 
                                  "HypoExponential" = "orange", "Erlang" = "purple",
                                  "Normal" = "blue", "Weibull" = "brown")) +
    labs(subtitle = "CDF", x = "Values", y = "Cumulative Probability", color = "Distributions") +
    theme_minimal()
  
  list(PDF = PDF, CDF = CDF)
}

# plot for traceB_LH
PDF_CDF_plot2_LH <- PDF_CDF_plot_2(traceB_LH, "traceB_LH_file", "traceB-LH", params_LH)
grid_LH <- PDF_CDF_plot2_LH$PDF + PDF_CDF_plot2_LH$CDF
print(grid_LH)

# PLOT for traceB-LE
PDF_CDF_plot2_LE <- PDF_CDF_plot_2(traceB_LE, "traceB_LE_file", "traceB-LE", params_LE)
grid_LE <- PDF_CDF_plot2_LE$PDF + PDF_CDF_plot2_LE$CDF
print(grid_LE)

# PLOT FOR traceB-HH
PDF_CDF_plot2_HH <- PDF_CDF_plot_2(traceB_HH, "traceB_HH_file", "traceB-HH", params_HH)
grid_HH <- PDF_CDF_plot2_HH$PDF + PDF_CDF_plot2_HH$CDF
print(grid_HH)

# PLOT FOR traceB-HE
PDF_CDF_plot2_HE <- PDF_CDF_plot_2(traceB_HE, "traceB_HE_file", "traceB-HE", params_HE)
grid_HE <- PDF_CDF_plot2_HE$PDF + PDF_CDF_plot2_HE$CDF
print(grid_HE)



QQ_plot_multiple <- function(data, column_name, title_1, params) {
  # Extract sample data
  sample_data <- sort(data[[column_name]])
  
  # Initialize a data frame for theoretical quantiles
  qq_data <- data.frame(Sample = sample_data)
  
  # Compute theoretical quantiles for each distribution
  for (dist_name in names(params)) {
    dist_params <- params[[dist_name]]
    
    # Compute theoretical quantiles based on the distribution type
    theoretical_quantiles <- switch(
      dist_name,
      Gamma = qgamma(ppoints(length(sample_data)), shape = dist_params$shape, scale = dist_params$scale),
      Normal = qnorm(ppoints(length(sample_data)), mean = dist_params$mean, sd = dist_params$sd),
      Weibull = qweibull(ppoints(length(sample_data)), shape = dist_params$shape, scale = dist_params$scale),
      HyperExponential = {
        prob <- dist_params$prob
        q1 <- qexp(ppoints(length(sample_data)), rate = dist_params$rate1)
        q2 <- qexp(ppoints(length(sample_data)), rate = dist_params$rate2)
        prob * q1 + (1 - prob) * q2
      },
      HypoExponential = {
        hypo_exp_quantiles <- function(p, rates) {
          sapply(p, function(pp) {
            uniroot(function(x) 1 - prod(1 - pexp(x, rate = rates)) - pp, lower = 0, upper = 1e5)$root
          })
        }
        hypo_exp_quantiles(ppoints(length(sample_data)), rates = dist_params$rates)
      },
      Erlang = qgamma(ppoints(length(sample_data)), shape = dist_params$shape, rate = dist_params$rate),
      stop(paste("Unsupported distribution:", dist_name))
    )
    
    # Add theoretical quantiles to the data frame
    qq_data[[dist_name]] <- theoretical_quantiles
  }
  
  # Reshape data for ggplot
  qq_long <- tidyr::pivot_longer(qq_data, -Sample, names_to = "Distribution", values_to = "Theoretical")
  
  # Generate Q-Q plot
  QQ_plot <- ggplot(qq_long, aes(x = Theoretical, y = Sample, color = Distribution)) +
    geom_point(size = 1, alpha = 0.85) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
    scale_color_manual(values = c("Gamma" = "red", "HyperExponential" = "green", 
                                  "HypoExponential" = "orange", "Erlang" = "purple",
                                  "Normal" = "blue", "Weibull" = "brown")) +
    labs(
      title = title_1,
      subtitle = "Q-Q Plot Comparing Multiple Distributions",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles",
      color = "Distributions"
    ) +
    theme_minimal()
  
  return(QQ_plot)
}
# Generate Q-Q plot for traceB_LH
QQ_plot_LH <- QQ_plot_multiple(traceB_LH, "traceB_LH_file", "traceB-LH", params_LH)
print(QQ_plot_LH)

# Generate Q-Q plot for traceB_LE
QQ_plot_LE <- QQ_plot_multiple(traceB_LE, "traceB_LE_file", "traceB-LE", params_LE)
print(QQ_plot_LE)

# Generate Q-Q plot for traceB_HH
QQ_plot_HH <- QQ_plot_multiple(traceB_HH, "traceB_HH_file", "traceB-HH", params_HH)
print(QQ_plot_HH)

# Generate Q-Q plot for traceB_HE
QQ_plot_HE <- QQ_plot_multiple(traceB_HE, "traceB_HE_file", "traceB-HE", params_HE)
print(QQ_plot_HE)

print(grid_LH + QQ_plot_LH)
print(grid_LE + QQ_plot_LE)
print(grid_HH + QQ_plot_HH)
print(grid_HE + QQ_plot_HE)
