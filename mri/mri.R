library(trelliscope)
library(dplyr)
library(readr)

d <- read_csv("mri/_data/data.csv") |>
  mutate(
    educ = recode_factor(educ,
      "1" = "1. less than high school grad.",
      "2" = "2. high school grad.",
      "3" = "3. some college",
      "4" = "4. college grad.",
      "5" = "5. beyond college"
    ),
    cdr = recode_factor(
      cdr,
      "0" = "nondemented",
      "0.5" = "very mild dementia",
      "1" = "mild dementia",
      "2" = "moderate dementia",
      .missing = "nondemented"
    ),
    orientation = case_when(
      grepl("_sag_", img) ~ "sagittal",
      grepl("_cor_", img) ~ "coronal",
      grepl("_tra_", img) ~ "transverse"
    ),
    img = panel_local(paste0("mri/_data/img/", img))
  )

d <- set_var_labels(d,
  subjid = "Subject identifier",
  mr_num = "MRI identifier",
  img_type = "String indicating information about the image type",
  sex = "Subject's sex",
  hand = "Subject's handedness",
  age = "Subject's age",
  educ = "Subject's education level",
  ses = "Subject's socioeconomic status - 1 (highest) to 5 (lowest)",
  mmse = "Mini-Mental State Examination score - 0 (worst) to 30 (best)",
  cdr = "Clinical Dementia Rating",
  etiv = "Estimated total intracranial volume (cm3)",
  nwbv = "Normalized whole brain volume - % of all voxels in the atlas-masked image that are labeled as gray or white matter by the automated tissue segmentation process",
  asf = "Atlas scaling factor - transforms native-space brain and skull to the atlas target",
  delay = "Delay between scan and clinical visit (days)",
  orientation = "Orientation of image",
  img = "MRI image of brain"
)

dt <- as_trelliscope_df(d,
  name = "Brain MRI Images",
  description = "MRI images from the open access oasis-brains.org project",
  path = "_public/mri"
) |>
  set_default_labels(c("subjid", "mmse", "cdr")) |>
  set_default_sort("mmse") |>
  set_default_layout(ncol = 5, sidebar = TRUE,
    visible_filters = c("orientation", "age", "cdr"))

view_trelliscope(dt)
