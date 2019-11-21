source("./scripts/plots.R")

generate_plots <- function(){

	####### Iterate through plots for each project
	projects <- c("nova","neutron","cinder")
	for (project in projects) {
		add_label = FALSE
		if (project == "neutron") {
			add_label=TRUE
		}
		generate_dispersion_plot(project)	
		generate_stmt_experience_plots(project,add_label)
		generate_test_experience_plots(project,add_label)
		generate_age_plots(project,add_label)
		generate_churn_plots(project,add_label)		
	}
	
}

generate_plots()



