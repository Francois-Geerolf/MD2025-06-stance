# build.R

# Define the range of figures to include
figure_numbers <- 2:8

# Loop over each figure script and source it
for (i in figure_numbers) {
  script_name <- sprintf("R/figure%d.R", i)
  message("▶️ Running: ", script_name)
  source(script_name)
}
