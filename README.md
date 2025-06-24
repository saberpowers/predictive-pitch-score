
# Pitch trajectory density estimation for predicting future outcomes

Scott Powers and Vicente Iglesias

2023 Saberseminar
[slides](https://drive.google.com/file/d/1MI_olwoJ6AJ1BWEVaXiWwGh63Dt7vdqj),
[video](https://www.youtube.com/watch?v=IgPB4TlTass&list=PL40KH8fsrt-sX1lSf659bl1u341F76ue3)

## Installing the predpitchscore R package

```R
devtools::install_github(
  repo = "saberpowers/predictive-pitch-score",
  subdir = "package/predpitchscore"
)
```

## Folder Structure

```
├── articles                            # LaTeX code for papers and slides
│   └── saberseminar
├── package                             # R package
│   └── predpitchscore
└── scripts                             # R code for reproducing results
    ├── sandbox
    ├── download_data.R
    ├── estimate_pitch_distribution.R
    ├── fit_descriptive_models.R
    ├── generate_predicitions.R
    └── validate_predictions.R
```

## Abstract

In 2023, FanGraphs added PitchingBot and Pitching+ pitch modeling to their leaderboards, narrowing the gap between public pitching analysis and the work done within teams. These two models share an objective: Given the trajectory of a pitch, predict the outcome of that pitch. Interestingly, the most important variables for predicting the outcome of the pitch (plate x and z location) are also the lowest-stability variables derived from the trajectory of the pitch. "Stuff" variables (velocity, break and release point) stabilize extremely quickly but explain less of the outcome than plate location. Both public pitch outcome models acknowledge this, with PitchingBot separating Stuff from Command and Pitching+ separating Stuff+ from Location+.

However, this distinction between "stuff" and not-"stuff" leaves something to be desired. The FanGraphs primers on these models suggest using the stuff-based metrics for small samples and using the overall grades for larger samples. But there is no line that separates small samples from large samples. One could take a weighted average of stuff-based grade and overall grade, with more weight shifting to the overall grade as the sample increases. We suggest a more direct approach that models the relationship between pitch trajectory and future outcomes, rather than present outcomes. After all, the purpose of pitch modeling is to improve predictions of how the pitcher will perform in the future.

We split this problem into two sub-problems. The first sub-problem, already solved by PitchingBot and Pitching+, is to model the outcome of a pitch based on its trajectory. The second sub-problem is to estimate a probability distribution for a pitcher's future pitch trajectories, based on a sample of observed pitches. This distribution estimation problem is challenging because several contextual covariates can influence the nine-dimensional pitch distribution, such as count and batter handedness. For example, a pitcher is trying to do something very different in a 3-0 count vs. in an 0-2 count. We formulate and estimate a hierarchical Bayesian model that allows for information sharing across contexts for each pitcher as well as information sharing across pitchers.
