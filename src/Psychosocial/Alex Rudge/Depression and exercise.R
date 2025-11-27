library(tidyverse)
library(magrittr)

# Does exercise correlate with depression?


# Run HADS and exercise 
data_depression <- hads %>%
  dplyr::filter(hads_type == 'depression_hads')

data_exercise <- exercise

data <- data_depression %>%
  dplyr::select(
    ParticipantNo,
    age,
    Sex,
    hads_score,
    score_group) %>%
  dplyr::inner_join(
    data_exercise %>%
      dplyr::select(
        ParticipantNo,
        MinimumExercise
      ),
    by = 'ParticipantNo'
  )


# Plot
data %>%
  ggplot(aes(x = hads_score, colour = MinimumExercise)) +
  geom_density()

# Statistical test
kruskal_test(hads_score ~ MinimumExercise, data = data)

data %>%
  dplyr::group_by(score_group, MinimumExercise) %>%
  dplyr::count() %>%
  dplyr::group_by(score_group) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = score_group, y = p, fill = MinimumExercise)) +
  geom_col(position = 'dodge')
