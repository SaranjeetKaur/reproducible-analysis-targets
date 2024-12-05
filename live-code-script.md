Process: remember to add commits for each phase, to log the process

Analysis without using {targets}

- Load packages

```r
install.packages(
  c(
    "broom",
    "conflicted",
    "crew",
    "janitor",
    "palmerpenguins",
    "purrr",
    "quarto",
    "tarchetypes",
    "targets",
    "tidyverse",
    "usethis",
    "visNetwork"
  )
)
```

- Get path to the `"penguins_raw.csv"` file

```r
penguins_csv_file <- palmerpenguins::path_to_file("penguins_raw.csv")
penguins_csv_file
```

- Read CSV file into R using `readr` package which comes with `tidyverse`

```r
penguins_data_raw <- readr::read_csv(penguins_csv_file)
```

- Keyboard shortcut for `View()` - hover over the object and press `F2`

- Clean up the data

```r
colnames(penguins_data_raw)
colnames(penguins_data_raw) <- janitor::make_clean_names(colnames(penguins_data_raw))
colnames(penguins_data_raw)

penguins_data <- penguins_data_raw |>
  # subset to only the columns needed for analysis
  dplyr::select(
    species,
    culmen_length_mm,
    culmen_depth_mm
  ) |>
  # Delete rows with missing data
  ggplot2::remove_missing(na.rm = TRUE)
```

- Note: The technical term “culmen” is used to refer to the bill.

- Plot the data

```r
penguins_data |>
  ggplot2::ggplot(ggplot2::aes(x = culmen_length_mm,
                               y = culmen_depth_mm,
                               color = species)) +
  ggplot2::geom_point() +
  ggplot2::labs(
    title = "Penguin culmen length and depth",
    x = "Culmen length (mm)",
    y = "Culmen depth (mm)"
  ) +
  ggplot2::theme_minimal()
```

- Build a linear model
- Hypothesis: culmen depth decreases with culmen length. 
- Test the hypothesis using a linear model of culmen depth dependent on culmen length.

```r
combined_model = lm(
  culmen_depth_mm ~ culmen_length_mm,
  data = penguins_data
  )
broom::glance(combined_model)
```

- Is this really an appropriate model?
- There are three species of penguins in the dataset.
- It is possible that the relationship between the culmen depth and length varies by species.
- Let's test some alternative models:
  - Say, a model that adds a parameter for species, or 
  - A model that adds an interaction effect between species and culmen length.

```r
species_model = lm(
  culmen_depth_mm ~ culmen_length_mm + species,
  data = penguins_data
  )

interaction_model = lm(
  culmen_depth_mm ~ culmen_length_mm * species,
  data = penguins_data
  )
```

- Now let's get the model summaries

```r
combined_summary <- broom::glance(combined_model)
species_summary <- broom::glance(species_model)
interaction_summary <- broom::glance(interaction_model)
```

- Let's also generate predictions from the models using `broom::augment()`.

```r
combined_predictions <- broom::augment(combined_model)
species_predictions <- broom::augment(species_model)
interaction_predictions <- broom::augment(interaction_model)
```

- Notice:
 - We did some repetitive steps both when fitting the models and when summarising them.
 - We have to call `broom::augment` and `broom::glance()` respectively, each time for each model. 
 - Each prediction value (`combined_predictions`, etc.) and summary value (`combined_summary`, etc.) is explicitly named and typed out manually. 
 - Hence, high chances to make a typo and end up with the wrong model being predicted and summarised.
 
- Let's come back to this in a bit!
- So far, we have cleaned the data, plotted it, and fit some models without using {targets}.
- What questions do you have?

Analysis using {targets}

- Every `targets` project must include a special file, called `_targets.R`
- This file should be in the main project folder (the “project root”). 
- It includes the specification of the workflow: directions for R to run your analysis.
- By using the `_targets.R` file, you won’t have to remember to run specific scripts in a certain order. 
- Instead, R will do it for you!
- Let's start by creating the `_targets.R` file

```r
targets::tar_script()
```

- The default `_targets.R` file includes three main parts:
  - Loading packages with `library()`
  - Defining a custom function with `function()`
  - Defining a list with `list()`.
- The list, is the most important part of the `_targets.R` file. 
- It defines the steps in the workflow. 
- The `_targets.R` file must always end with this list.
- Each item in the list is a call of the `targets::tar_target()` function. 
  - The first argument of `targets::tar_target()` is name of the target to build, 
  - The second argument is the command used to build it. 
  - The name of the target is unquoted.
- Let's clean up the default `_targets.R` file and populate it with our workflow.

```r
list(
  targets::tar_target(penguins_csv_file,
             palmerpenguins::path_to_file("penguins_raw.csv")),
  targets::tar_target(penguins_data_raw,
             readr::read_csv(penguins_csv_file)
             )
  )
```

- Let's run the workflow using `targets::tar_make()`.
- Let's use `show_col_types = FALSE` to quiet the column specification message.
- Run `targets::tar_make()` again.
- To visualise the state of the workflow so far, we can use `targets::tar_visnetwork()`.

- Let's create data cleaning function (include the colname cleaning steps) in the `_targets.R` file.

```r
clean_penguins_data <- function(penguins_data_raw) {
  colnames(penguins_data_raw) <- janitor::make_clean_names(colnames(penguins_data_raw))
  penguins_data_raw |>
    # subset to only the columns needed for analysis
    dplyr::select(
      species,
      culmen_length_mm,
      culmen_depth_mm
    ) |>
    # Delete rows with missing data
    ggplot2::remove_missing(na.rm = TRUE)
}
```

- Now let's add this function to the list in the `_targets.R` file.

```r
clean_penguins_data <- function(penguins_data_raw) {
  colnames(penguins_data_raw) <- janitor::make_clean_names(colnames(penguins_data_raw))
  penguins_data_raw |>
    # subset to only the columns needed for analysis
    dplyr::select(
      species,
      culmen_length_mm,
      culmen_depth_mm
    ) |>
    # Delete rows with missing data
    ggplot2::remove_missing(na.rm = TRUE)
}

list(
  targets::tar_target(penguins_csv_file,
             palmerpenguins::path_to_file("penguins_raw.csv")),
  targets::tar_target(penguins_data_raw,
             readr::read_csv(penguins_csv_file,
                             show_col_types = FALSE)),
  targets::tar_target(penguins_data,
                      clean_penguins_data(penguins_data_raw))
  )
```

- Let's run the workflow using `targets::tar_make()`.
- Notice how the earlier two steps are skipped and only the new step in the workflow  is dispatched.
- Let's visualise the workflow using `targets::tar_visnetwork()`.

- What questions do you have?

- Let's add the plotting step to the workflow, include it in the list.

```r
gg_penguins <- function(penguins_data){
  penguins_data |>
    ggplot2::ggplot(ggplot2::aes(x = culmen_length_mm,
                                 y = culmen_depth_mm,
                                 color = species)) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "Penguin culmen length and depth",
      x = "Culmen length (mm)",
      y = "Culmen depth (mm)"
    ) +
    ggplot2::theme_minimal()
}

list(
  targets::tar_target(penguins_csv_file,
             palmerpenguins::path_to_file("penguins_raw.csv")),
  targets::tar_target(penguins_data_raw,
             readr::read_csv(penguins_csv_file,
                             show_col_types = FALSE)),
  targets::tar_target(penguins_data,
                      clean_penguins_data(penguins_data_raw)),
  targets::tar_target(plot_penguins,
                      gg_penguins(penguins_data))
  )
```

- Let's run the workflow using `targets::tar_make()` and visualise it using `targets::tar_visnetwork()`.
- Now if you try to look for the object `plot_penguins`, you will get an error that the object cannot be found.
- This is because the object is only available in the workflow, not in the global environment.
- So where are the results of the workflow stored?
- Actually, `targets` runs the workflow in a separate R session that we can’t interact with. 
- This is for reproducibility!
- So the objects built by the workflow only depend only on the code in your project, and not on any commands you may have interactively given to R.
- `targets` has two such functions: `targets::tar_load()` and `targets::tar_read()`.
- `targets::tar_read()`: to immediately inspect an object without loading it into the current session.
- `targets::tar_load()`: to load objects into the current session and do things with them. 

```r
targets::tar_read(plot_penguins) # Should show the plot
plot_penguins # Should throw an object not found error
```

```r
targets::tar_load(plot_penguins) # Should show the plot and be available in the current session
plot_penguins # Should now be available and also loaded in the current session environment
```

- Now let's what happens when we change/modify one part of the workflow.
- Say, we decide that the species names should be shorter. 
- Right have the common name and the scientific name.
- Let's only keep the first part of the common name by modifying the `clean_penquins_data` function in the `_targets.R` file.

```r
clean_penguins_data <- function(penguins_data_raw) {
  colnames(penguins_data_raw) <- janitor::make_clean_names(colnames(penguins_data_raw))
  penguins_data_raw |>
    # subset to only the columns needed for analysis
    dplyr::select(
      species,
      culmen_length_mm,
      culmen_depth_mm
    ) |>
    # Delete rows with missing data
    ggplot2::remove_missing(na.rm = TRUE)
    # Split "species" apart on spaces, and only keep the first word
    tidyr::separate(species, into = "species", extra = "drop")
}
```

- Now let's try to visualise the workflow using `targets::tar_visnetwork()`.
- Notice the dark green targets - they are the ones that are up to date.
- The light blue colour indicates that target is outdated.
- Since `plot_penguins` depends on `penguins_data`, it is also light blue.
- Some other ways of checking the workflow status:
  - `targets::tar_outdated()`: lists only the outdated targets, i.e., targets that will be built during the next run, 
  or depend on such a target. If everything is up to date, it will return a zero-length character vector (character(0)).
  - `targets::tar_progress()`: shows the current status of the workflow as a dataframe.
- Let's run the workflow again using `targets::tar_make()` so that it is up to date.
- Notice both `penguins_data` and `plot_penguins` are dispatched now. 
- You will also see that the visualisation of the workflow is now all up to date.

- What questions do you have?

- Is there a simpler way to write workflow plans, besides the default way with `tar_target()`?
- Because the default way can be a bit verbose.
- Yes, there is an alternative provided by the {tarchetypes} package (also created by Will Landau).
- It provides shortcuts for writing the pipelines.
- Let's use one of them, `tarchetypes::tar_plan()`. 
- We will use it instead of `list()` in the `_targets.R` file.
- Syntax: `target_name = target_command`.
- Let’s edit our workflow using `tarchetypes::tar_plan()`.

```r
# replace the list with
tarchetypes::tar_plan(
  penguins_csv_file = palmerpenguins::path_to_file("penguins_raw.csv"),
  penguins_data_raw = readr::read_csv(penguins_csv_file,
                             show_col_types = FALSE),
  penguins_data = clean_penguins_data(penguins_data_raw),
  plot_penguins = gg_penguins(penguins_data)
  )
```

- Isn't that easier to read?
- Note, you can still use `targets::tar_target()` from within `tarchetypes::tar_plan()`.
- We will be coming back to this.

- For now, let's organise things a bit.
- As of now, everything is in a single `_targets.R` file.
- But we can organise the code better.
- Let's create a directory called `R` to store the functions in it (in a file named `functions.R`) using `usethis::use_r("functions")`.
- Let’s put the `clean_penguin_data()` and `gg_penguins()` functions in there now.
- Modify the `_targets.R` script to call these scripts with source `source("R/functions.R")`.
- The `_targets.R` script looks much more organised now and it is easier to tell what happens in each step.

- What questions do you have?

- Let's add the model fitting steps to the workflow.
- First let us add the `combined_model` to the workflow.

```r
source("R/functions.R")

tarchetypes::tar_plan(
  # Load raw data
  penguins_csv_file = palmerpenguins::path_to_file("penguins_raw.csv"),
  penguins_data_raw = readr::read_csv(penguins_csv_file,
                             show_col_types = FALSE),
  # Clean data
  penguins_data = clean_penguins_data(penguins_data_raw),
  # Plot data
  plot_penguins = gg_penguins(penguins_data),
  # Build model
  combined_model = lm(
    culmen_depth_mm ~ culmen_length_mm,
    data = penguins_data
  )
)
```
- Run the workflow using `targets::tar_make()`.
- Let's load `combined_model` and use the `broom::glance()` function to get the summary of the model.

```r
targets::tar_load(combined_model)
broom::glance(combined_model)
```

- Let's also add the alternative models and their summaries to the workflow.

```r
source("R/functions.R")

tarchetypes::tar_plan(
  # Load raw data
  penguins_csv_file = palmerpenguins::path_to_file("penguins_raw.csv"),
  penguins_data_raw = readr::read_csv(penguins_csv_file,
                                      show_col_types = FALSE),
  # Clean data
  penguins_data = clean_penguins_data(penguins_data_raw),
  # Plot data
  plot_penguins = gg_penguins(penguins_data),
  # Build model
  combined_model = lm(
    culmen_depth_mm ~ culmen_length_mm,
    data = penguins_data
  ),
  species_model = lm(
    culmen_depth_mm ~ culmen_length_mm + species,
    data = penguins_data
  ),
  interaction_model = lm(
    culmen_depth_mm ~ culmen_length_mm * species,
    data = penguins_data
  ),
  # Model summaries
  combined_summary = broom::glance(combined_model),
  species_summary = broom::glance(species_model),
  interaction_summary = broom::glance(interaction_model)
)
```

- Run the workflow using `targets::tar_make()`.
- This works. 
- But remember, we were talking about this approach being repetitive and manual, hence, may lead to typos?
- This can be overcome using the branching feature of `targets`.
- It is a way to define many targets from a single line of code.
- This saves you typing and reduces the risk of typos.
- Now we will try dynamic branching in the workflow.
  - It will provide a single specification to make the targets (the “pattern”).
  - Which will generate multiple targets from it (“branches”).
  - Being “Dynamic” implies that the branches (from the pattern) do not have to be defined in advance instead they are a dynamic result of the code.
- Use the following to branch the model building process.
- Define the models in a list (instead of one target per model). 
- This helps dynamic branching in looping: a function is applied to each element of a list. 


```r
# Build models
models = list(
  combined_model = lm(
    culmen_depth_mm ~ culmen_length_mm, data = penguins_data),
  species_model = lm(
    culmen_depth_mm ~ culmen_length_mm + species, data = penguins_data),
  interaction_model = lm(
    culmen_depth_mm ~ culmen_length_mm * species, data = penguins_data)
)
```

- Also change the model summarises.

```r
targets::tar_target(
    model_summaries,
    broom::glance(models[[1]]),
    pattern = map(models)
  )
```

- `model_summaries` is the target name.
- `broom::glance(models[[1]])` is the command to build the target.
- It is applied to each element of `models`.
- [[1]]: removes one layer of nesting
- pattern: indicates that this target should be built using dynamic branching. 
- map: applies the command to each element of the input list (models) sequentially.


- Let's reflect on the output of `targets::tar_make()`.
- Series of smaller targets (branches) that are each named like `model_summaries_alphanumeric_hash`.
- One overall `model_summaries target`. 
- This is effect of branching.
- Each smaller target is a “branch” from the overall target. 
- targets does not know how many branches there will be, it is naming them using a alphanumeric hash.
- Branches are built one at a time, then combined into the overall target.

- Take a look at the output of `model_summaries` using `targets::tar_read(model_summaries)`.

- Output is a single dataframe.
- However, not sure which row belongs to which model.
- They might not be in the same order as the models list.
- Let's make it more evident by adding the model type to the output of each branch.
- Write a function in the `functions.R` file to add the model type to the output of `broom::glance()`.

```r
glance_with_mod_name <- function(model_in_list) {
  model_name <- names(model_in_list)
  model <- model_in_list[[1]]
  broom::glance(model) |>
    dplyr::mutate(model_name = model_name)
}
```

- Update the `model_summaries` target in the workflow to use this function.

```r
# Model summaries
targets::tar_target(
  model_summaries,
  glance_with_mod_name(models),
  pattern = map(models)
)
```

- Now let's run the workflow using `targets::tar_make()` (also view `targets::tar_visnetwork()`).
- The output of `targets::tar_read(model_summaries)` will have a column `model_name` that indicates which model each row corresponds to.

- Add one more target: prediction of culmen depth based on each model. 
- Using `broom::augment()` function.
- Add the following to `functions.R` file.

```r
augment_with_mod_name <- function(model_in_list) {
  model_name <- names(model_in_list)
  model <- model_in_list[[1]]
  broom::augment(model) |>
    dplyr::mutate(model_name = model_name)
}
```

- Add the target to the workflow.

```r
# Get model predictions
targets::tar_target(
  model_predictions,
  augment_with_mod_name(models),
  pattern = map(models)
)
```

- Run the workflow using `targets::tar_make()`.
- Visualise the workflow using `targets::tar_visnetwork()`.
