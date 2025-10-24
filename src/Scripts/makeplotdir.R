# Create plot directories with proper permissions
create_plot_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  Sys.chmod(path, mode = "0755", use_umask = FALSE)
}

create_plot_dir("plots/baseline")
create_plot_dir("plots/arranged")

create_plot_dir("plots/cd/soft-flare/diet")
create_plot_dir("plots/cd/hard-flare/diet")
create_plot_dir("plots/uc/soft-flare/diet")
create_plot_dir("plots/uc/hard-flare/diet")

# Sensitivity analysis subdirectories
create_plot_dir("plots/cd/soft-flare/diet/sensitivity")
create_plot_dir("plots/cd/hard-flare/diet/sensitivity")
create_plot_dir("plots/uc/soft-flare/diet/sensitivity")
create_plot_dir("plots/uc/hard-flare/diet/sensitivity")

create_plot_dir("plots/ibd/soft-flare/diet/")
create_plot_dir("plots/ibd/hard-flare/diet/")

# IBD sensitivity analysis subdirectories
create_plot_dir("plots/ibd/soft-flare/diet/sensitivity")
create_plot_dir("plots/ibd/hard-flare/diet/sensitivity")


create_plot_dir("plots/cd/soft-flare/demographics")
create_plot_dir("plots/cd/hard-flare/demographics")
create_plot_dir("plots/uc/soft-flare/demographics")
create_plot_dir("plots/uc/hard-flare/demographics")

create_plot_dir("plots/cd/soft-flare/biochem")
create_plot_dir("plots/cd/hard-flare/biochem")
create_plot_dir("plots/uc/soft-flare/biochem")
create_plot_dir("plots/uc/hard-flare/biochem")


create_plot_dir("plots/cd/soft-flare/demographics")
create_plot_dir("plots/cd/hard-flare/demographics")
create_plot_dir("plots/uc/soft-flare/demographics")
create_plot_dir("plots/uc/hard-flare/demographics")

create_plot_dir("plots/cd/soft-flare/controlled")
create_plot_dir("plots/cd/hard-flare/controlled")
create_plot_dir("plots/uc/soft-flare/controlled")
create_plot_dir("plots/uc/hard-flare/controlled")

create_plot_dir("plots/cd/soft-flare/ibd")
create_plot_dir("plots/cd/hard-flare/ibd")
create_plot_dir("plots/uc/soft-flare/ibd")
create_plot_dir("plots/uc/hard-flare/ibd")

create_plot_dir("docx")
