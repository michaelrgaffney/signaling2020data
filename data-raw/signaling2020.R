## code to prepare `signaling2020` dataset goes here
library(tidyverse)

col_names <- names(read_csv("data-raw/2020USSampleRaw.csv", n_max = 0))
dUS <- read_csv("data-raw/2020USSampleRaw.csv", col_names = col_names, skip = 7) %>%
  mutate(
    vignette = FL_129_DO,
    vignette = str_replace_all(vignette, "Vignette", ""),
    vignette = str_replace_all(vignette, "Page1", ""),
    vignette = str_replace_all(vignette, "IntroSex", "RomanticPartner"),
    sample = "American"
  ) %>%
  mutate(
    signal = paste0(FL_38_DO, FL_32_DO, FL_241_DO),
    signal = str_replace_all(signal, "NA", ""),
    signal = str_replace_all(signal, ":Partner", ""),
    signal = str_replace_all(signal, ":Basketball", ""),
    signal = str_replace_all(signal, ":Sister", ""),
    signal = str_replace(signal, "CryingandBehaviorChange", "CryingwithBehaviorChange")) %>%
  mutate(
    T1Belief = paste0(`Sister T1 Belief_1`, `Basketball T1 Belief_1`, `Partner T1 Belief_1`),
    T1Belief = as.numeric(str_replace_all(T1Belief, "NA", "")),
    T2Belief = paste0(`Sister T2 Belief_1`, `Basketball T2 Belief_1`, `Partner T2 Belief_1`),
    T2Belief = as.numeric(str_replace_all(T2Belief, "NA", "")),
    T1Action = paste0(`Sister T1 Action_1`, `Basketball  T1Action_1`, `Partner T1 Action_1`),
    T1Action = as.numeric(str_replace_all(T1Action, "NA", "")),
    T2Action = paste0(`Sister T2 Action_1`, `Basketball T2Action_1`, `Partner T2 Action_1`),
    T2Action = as.numeric(str_replace_all(T2Action, "NA", "")),
    T3Action = paste0(`Sister T3 Action_1`, `Basketball T3 Action_1`, `Partner T3 Action_1`),
    T3Action = as.numeric(str_replace_all(T3Action, "NA", "")),
    T4AttentionCheck = paste0(`Sister T4 Action_1`, `Basketball T4_1`, `Partner T4 Action_1`),
    T4AttentionCheck = as.numeric(str_replace_all(T4AttentionCheck, "NA", "")),
    T4AttentionCheckFail = ifelse(T4AttentionCheck == '0', 0, 1)
  ) %>%
  mutate( # time measurements
    TotalTime = `Duration (in seconds)`,
    VignetteTimeSister = (sumrow = `Sister P1 Time_Page Submit` + `Sister P2 Time_Page Submit` + `Sister P3 Time_Page Submit`),
    VignetteTime = paste0(`Basketball Time_Page Submit`, VignetteTimeSister, `Male Rom Time_Page Submit`, `Female Rom Time_Page Submit`),
    VignetteTime = as.numeric(str_replace_all(VignetteTime, "NA", ""))
  ) %>%
  mutate( # time 1 emotions
    T1AngryB = ifelse(str_detect(`Basketball T1Emotion`, 'Angry'), 1, 0),
    T1SadB = ifelse(str_detect(`Basketball T1Emotion`, 'Sad'), 1, 0),
    T1SuicidalB = ifelse(str_detect(`Basketball T1Emotion`, 'Suicidal'), 1, 0),
    T1MentallyIllB = ifelse(str_detect(`Basketball T1Emotion`, 'Mentally ill'), 1, 0),
    T1DepressedB = ifelse(str_detect(`Basketball T1Emotion`, 'Depressed'), 1, 0),
    T1GuiltyB = ifelse(str_detect(`Basketball T1Emotion`, 'Guilty'), 1, 0),
    T1CalmB = ifelse(str_detect(`Basketball T1Emotion`, 'Calm'), 1, 0),
    T1NeutralB = ifelse(str_detect(`Basketball T1Emotion`, 'Neutral'), 1, 0),
    T1ScaredB = ifelse(str_detect(`Basketball T1Emotion`, 'Scared'), 1, 0),
    T1TiredB = ifelse(str_detect(`Basketball T1Emotion`, 'Tired'), 1, 0),
    T1DistressedB = ifelse(str_detect(`Basketball T1Emotion`, 'Distressed'), 1, 0),
    T1DeviousB = ifelse(str_detect(`Basketball T1Emotion`, 'Devious'), 1, 0),
    T1JealousB = ifelse(str_detect(`Basketball T1Emotion`, 'Jealous'), 1, 0),
    T1ConfidentB = ifelse(str_detect(`Basketball T1Emotion`, 'Confident'), 1, 0),
    T1TraumatizedB = ifelse(str_detect(`Basketball T1Emotion`, 'Traumatized'), 1, 0),
    T1ViolatedB = ifelse(str_detect(`Basketball T1Emotion`, 'Violated'), 1, 0),
    T1NoneOfAboveB = ifelse(str_detect(`Basketball T1Emotion`, 'None'), 1, 0),

    T1AngryS = ifelse(str_detect(`Sister T1 Emotions`, 'Angry'), 1, 0),
    T1SadS = ifelse(str_detect(`Sister T1 Emotions`, 'Sad'), 1, 0),
    T1SuicidalS = ifelse(str_detect(`Sister T1 Emotions`, 'Suicidal'), 1, 0),
    T1MentallyIllS = ifelse(str_detect(`Sister T1 Emotions`, 'Mentally ill'), 1, 0),
    T1DepressedS = ifelse(str_detect(`Sister T1 Emotions`, 'Depressed'), 1, 0),
    T1GuiltyS = ifelse(str_detect(`Sister T1 Emotions`, 'Guilty'), 1, 0),
    T1CalmS = ifelse(str_detect(`Sister T1 Emotions`, 'Calm'), 1, 0),
    T1NeutralS = ifelse(str_detect(`Sister T1 Emotions`, 'Neutral'), 1, 0),
    T1ScaredS = ifelse(str_detect(`Sister T1 Emotions`, 'Scared'), 1, 0),
    T1TiredS = ifelse(str_detect(`Sister T1 Emotions`, 'Tired'), 1, 0),
    T1DistressedS = ifelse(str_detect(`Sister T1 Emotions`, 'Distressed'), 1, 0),
    T1DeviousS = ifelse(str_detect(`Sister T1 Emotions`, 'Devious'), 1, 0),
    T1JealousS = ifelse(str_detect(`Sister T1 Emotions`, 'Jealous'), 1, 0),
    T1ConfidentS = ifelse(str_detect(`Sister T1 Emotions`, 'Confident'), 1, 0),
    T1TraumatizedS = ifelse(str_detect(`Sister T1 Emotions`, 'Traumatized'), 1, 0),
    T1ViolatedS = ifelse(str_detect(`Sister T1 Emotions`, 'Violated'), 1, 0),
    T1NoneOfAboveS = ifelse(str_detect(`Sister T1 Emotions`, 'None'), 1, 0),

    T1AngryP = ifelse(str_detect(`Partner T1 Emotion`, 'Angry'), 1, 0),
    T1SadP = ifelse(str_detect(`Partner T1 Emotion`, 'Sad'), 1, 0),
    T1SuicidalP = ifelse(str_detect(`Partner T1 Emotion`, 'Suicidal'), 1, 0),
    T1MentallyIllP = ifelse(str_detect(`Partner T1 Emotion`, 'Mentally ill'), 1, 0),
    T1DepressedP = ifelse(str_detect(`Partner T1 Emotion`, 'Depressed'), 1, 0),
    T1GuiltyP = ifelse(str_detect(`Partner T1 Emotion`, 'Guilty'), 1, 0),
    T1CalmP = ifelse(str_detect(`Partner T1 Emotion`, 'Calm'), 1, 0),
    T1NeutralP = ifelse(str_detect(`Partner T1 Emotion`, 'Neutral'), 1, 0),
    T1ScaredP = ifelse(str_detect(`Partner T1 Emotion`, 'Scared'), 1, 0),
    T1TiredP = ifelse(str_detect(`Partner T1 Emotion`, 'Tired'), 1, 0),
    T1DistressedP = ifelse(str_detect(`Partner T1 Emotion`, 'Distressed'), 1, 0),
    T1DeviousP = ifelse(str_detect(`Partner T1 Emotion`, 'Devious'), 1, 0),
    T1JealousP = ifelse(str_detect(`Partner T1 Emotion`, 'Jealous'), 1, 0),
    T1ConfidentP = ifelse(str_detect(`Partner T1 Emotion`, 'Confident'), 1, 0),
    T1TraumatizedP = ifelse(str_detect(`Partner T1 Emotion`, 'Traumatized'), 1, 0),
    T1ViolatedP = ifelse(str_detect(`Partner T1 Emotion`, 'Violated'), 1, 0),
    T1NoneOfAboveP = ifelse(str_detect(`Partner T1 Emotion`, 'None'), 1, 0),

    T1Angry = paste0(T1AngryB, T1AngryS, T1AngryP),
    T1Angry = as.numeric(str_replace_all(T1Angry, "NA", "")),
    T1Sad = paste0(T1SadB, T1SadS, T1SadP),
    T1Sad = as.numeric(str_replace_all(T1Sad, "NA", "")),
    T1Suicidal = paste0(T1SuicidalB, T1SuicidalS, T1SuicidalP),
    T1Suicidal = as.numeric(str_replace_all(T1Suicidal, "NA", "")),
    T1MentallyIll = paste0(T1MentallyIllB, T1MentallyIllS, T1MentallyIllP),
    T1MentallyIll = as.numeric(str_replace_all(T1MentallyIll, "NA", "")),
    T1Depressed = paste0(T1DepressedB, T1DepressedS, T1DepressedP),
    T1Depressed = as.numeric(str_replace_all(T1Depressed, "NA", "")),
    T1Guilty = paste0(T1GuiltyB, T1GuiltyS, T1GuiltyP),
    T1Guilty = as.numeric(str_replace_all(T1Guilty, "NA", "")),
    T1Calm = paste0(T1CalmB, T1CalmS, T1CalmP),
    T1Calm = as.numeric(str_replace_all(T1Calm, "NA", "")),
    T1Neutral = paste0(T1NeutralB, T1NeutralS, T1NeutralP),
    T1Neutral = as.numeric(str_replace_all(T1Neutral, "NA", "")),
    T1Scared = paste0(T1ScaredB, T1ScaredS, T1ScaredP),
    T1Scared = as.numeric(str_replace_all(T1Scared, "NA", "")),
    T1Tired = paste0(T1TiredB, T1TiredS, T1TiredP),
    T1Tired = as.numeric(str_replace_all(T1Tired, "NA", "")),
    T1Distressed = paste0(T1DistressedB, T1DistressedS, T1DistressedP),
    T1Distressed = as.numeric(str_replace_all(T1Distressed, "NA", "")),
    T1Devious = paste0(T1DeviousB, T1DeviousS, T1DeviousP),
    T1Devious = as.numeric(str_replace_all(T1Devious, "NA", "")),
    T1Jealous = paste0(T1JealousB, T1JealousS, T1JealousP),
    T1Jealous = as.numeric(str_replace_all(T1Jealous, "NA", "")),
    T1Confident = paste0(T1ConfidentB, T1ConfidentS, T1ConfidentP),
    T1Confident = as.numeric(str_replace_all(T1Confident, "NA", "")),
    T1Traumatized = paste0(T1TraumatizedB, T1TraumatizedS, T1TraumatizedP),
    T1Traumatized = as.numeric(str_replace_all(T1Traumatized, "NA", "")),
    T1Violated = paste0(T1ViolatedB, T1ViolatedS, T1ViolatedP),
    T1Violated = as.numeric(str_replace_all(T1Violated, "NA", "")),
    T1NoneOfAbove = paste0(T1NoneOfAboveB, T1NoneOfAboveS, T1NoneOfAboveP),
    T1NoneOfAbove = as.numeric(str_replace_all(T1NoneOfAbove, "NA", ""))
  ) %>%
  mutate( # time 2 emotions
    T2AngryB = ifelse(str_detect(`Basketball T2Emotion`, 'Angry'), 1, 0),
    T2SadB = ifelse(str_detect(`Basketball T2Emotion`, 'Sad'), 1, 0),
    T2SuicidalB = ifelse(str_detect(`Basketball T2Emotion`, 'Suicidal'), 1, 0),
    T2MentallyIllB = ifelse(str_detect(`Basketball T2Emotion`, 'Mentally ill'), 1, 0),
    T2DepressedB = ifelse(str_detect(`Basketball T2Emotion`, 'Depressed'), 1, 0),
    T2GuiltyB = ifelse(str_detect(`Basketball T2Emotion`, 'Guilty'), 1, 0),
    T2CalmB = ifelse(str_detect(`Basketball T2Emotion`, 'Calm'), 1, 0),
    T2NeutralB = ifelse(str_detect(`Basketball T2Emotion`, 'Neutral'), 1, 0),
    T2ScaredB = ifelse(str_detect(`Basketball T2Emotion`, 'Scared'), 1, 0),
    T2TiredB = ifelse(str_detect(`Basketball T2Emotion`, 'Tired'), 1, 0),
    T2DistressedB = ifelse(str_detect(`Basketball T2Emotion`, 'Distressed'), 1, 0),
    T2DeviousB = ifelse(str_detect(`Basketball T2Emotion`, 'Devious'), 1, 0),
    T2JealousB = ifelse(str_detect(`Basketball T2Emotion`, 'Jealous'), 1, 0),
    T2ConfidentB = ifelse(str_detect(`Basketball T2Emotion`, 'Confident'), 1, 0),
    T2TraumatizedB = ifelse(str_detect(`Basketball T2Emotion`, 'Traumatized'), 1, 0),
    T2ViolatedB = ifelse(str_detect(`Basketball T2Emotion`, 'Violated'), 1, 0),
    T2NoneOfAboveB = ifelse(str_detect(`Basketball T2Emotion`, 'None'), 1, 0),

    T2AngryS = ifelse(str_detect(`Sister T2 Emotions`, 'Angry'), 1, 0),
    T2SadS = ifelse(str_detect(`Sister T2 Emotions`, 'Sad'), 1, 0),
    T2SuicidalS = ifelse(str_detect(`Sister T2 Emotions`, 'Suicidal'), 1, 0),
    T2MentallyIllS = ifelse(str_detect(`Sister T2 Emotions`, 'Mentally ill'), 1, 0),
    T2DepressedS = ifelse(str_detect(`Sister T2 Emotions`, 'Depressed'), 1, 0),
    T2GuiltyS = ifelse(str_detect(`Sister T2 Emotions`, 'Guilty'), 1, 0),
    T2CalmS = ifelse(str_detect(`Sister T2 Emotions`, 'Calm'), 1, 0),
    T2NeutralS = ifelse(str_detect(`Sister T2 Emotions`, 'Neutral'), 1, 0),
    T2ScaredS = ifelse(str_detect(`Sister T2 Emotions`, 'Scared'), 1, 0),
    T2TiredS = ifelse(str_detect(`Sister T2 Emotions`, 'Tired'), 1, 0),
    T2DistressedS = ifelse(str_detect(`Sister T2 Emotions`, 'Distressed'), 1, 0),
    T2DeviousS = ifelse(str_detect(`Sister T2 Emotions`, 'Devious'), 1, 0),
    T2JealousS = ifelse(str_detect(`Sister T2 Emotions`, 'Jealous'), 1, 0),
    T2ConfidentS = ifelse(str_detect(`Sister T2 Emotions`, 'Confident'), 1, 0),
    T2TraumatizedS = ifelse(str_detect(`Sister T2 Emotions`, 'Traumatized'), 1, 0),
    T2ViolatedS = ifelse(str_detect(`Sister T2 Emotions`, 'Violated'), 1, 0),
    T2NoneOfAboveS = ifelse(str_detect(`Sister T2 Emotions`, 'None'), 1, 0),

    T2AngryP = ifelse(str_detect(`Partner T2 Emotions`, 'Angry'), 1, 0),
    T2SadP = ifelse(str_detect(`Partner T2 Emotions`, 'Sad'), 1, 0),
    T2SuicidalP = ifelse(str_detect(`Partner T2 Emotions`, 'Suicidal'), 1, 0),
    T2MentallyIllP = ifelse(str_detect(`Partner T2 Emotions`, 'Mentally ill'), 1, 0),
    T2DepressedP = ifelse(str_detect(`Partner T2 Emotions`, 'Depressed'), 1, 0),
    T2GuiltyP = ifelse(str_detect(`Partner T2 Emotions`, 'Guilty'), 1, 0),
    T2CalmP = ifelse(str_detect(`Partner T2 Emotions`, 'Calm'), 1, 0),
    T2NeutralP = ifelse(str_detect(`Partner T2 Emotions`, 'Neutral'), 1, 0),
    T2ScaredP = ifelse(str_detect(`Partner T2 Emotions`, 'Scared'), 1, 0),
    T2TiredP = ifelse(str_detect(`Partner T2 Emotions`, 'Tired'), 1, 0),
    T2DistressedP = ifelse(str_detect(`Partner T2 Emotions`, 'Distressed'), 1, 0),
    T2DeviousP = ifelse(str_detect(`Partner T2 Emotions`, 'Devious'), 1, 0),
    T2JealousP = ifelse(str_detect(`Partner T2 Emotions`, 'Jealous'), 1, 0),
    T2ConfidentP = ifelse(str_detect(`Partner T2 Emotions`, 'Confident'), 1, 0),
    T2TraumatizedP = ifelse(str_detect(`Partner T2 Emotions`, 'Traumatized'), 1, 0),
    T2ViolatedP = ifelse(str_detect(`Partner T2 Emotions`, 'Violated'), 1, 0),
    T2NoneOfAboveP = ifelse(str_detect(`Partner T2 Emotions`, 'None'), 1, 0),

    T2Angry = paste0(T2AngryB, T2AngryS, T2AngryP),
    T2Angry = as.numeric(str_replace_all(T2Angry, "NA", "")),
    T2Sad = paste0(T2SadB, T2SadS, T2SadP),
    T2Sad = as.numeric(str_replace_all(T2Sad, "NA", "")),
    T2Suicidal = paste0(T2SuicidalB, T2SuicidalS, T2SuicidalP),
    T2Suicidal = as.numeric(str_replace_all(T2Suicidal, "NA", "")),
    T2MentallyIll = paste0(T2MentallyIllB, T2MentallyIllS, T2MentallyIllP),
    T2MentallyIll = as.numeric(str_replace_all(T2MentallyIll, "NA", "")),
    T2Depressed = paste0(T2DepressedB, T2DepressedS, T2DepressedP),
    T2Depressed = as.numeric(str_replace_all(T2Depressed, "NA", "")),
    T2Guilty = paste0(T2GuiltyB, T2GuiltyS, T2GuiltyP),
    T2Guilty = as.numeric(str_replace_all(T2Guilty, "NA", "")),
    T2Calm = paste0(T2CalmB, T2CalmS, T2CalmP),
    T2Calm = as.numeric(str_replace_all(T2Calm, "NA", "")),
    T2Neutral = paste0(T2NeutralB, T2NeutralS, T2NeutralP),
    T2Neutral = as.numeric(str_replace_all(T2Neutral, "NA", "")),
    T2Scared = paste0(T2ScaredB, T2ScaredS, T2ScaredP),
    T2Scared = as.numeric(str_replace_all(T2Scared, "NA", "")),
    T2Tired = paste0(T2TiredB, T2TiredS, T2TiredP),
    T2Tired = as.numeric(str_replace_all(T2Tired, "NA", "")),
    T2Distressed = paste0(T2DistressedB, T2DistressedS, T2DistressedP),
    T2Distressed = as.numeric(str_replace_all(T2Distressed, "NA", "")),
    T2Devious = paste0(T2DeviousB, T2DeviousS, T2DeviousP),
    T2Devious = as.numeric(str_replace_all(T2Devious, "NA", "")),
    T2Jealous = paste0(T2JealousB, T2JealousS, T2JealousP),
    T2Jealous = as.numeric(str_replace_all(T2Jealous, "NA", "")),
    T2Confident = paste0(T2ConfidentB, T2ConfidentS, T2ConfidentP),
    T2Confident = as.numeric(str_replace_all(T2Confident, "NA", "")),
    T2Traumatized = paste0(T2TraumatizedB, T2TraumatizedS, T2TraumatizedP),
    T2Traumatized = as.numeric(str_replace_all(T2Traumatized, "NA", "")),
    T2Violated = paste0(T2ViolatedB, T2ViolatedS, T2ViolatedP),
    T2Violated = as.numeric(str_replace_all(T2Violated, "NA", "")),
    T2NoneOfAbove = paste0(T2NoneOfAboveB, T2NoneOfAboveS, T2NoneOfAboveP),
    T2NoneOfAbove = as.numeric(str_replace_all(T2NoneOfAbove, "NA", ""))
  ) %>%
  mutate( # NAs for vars only in Indian Sample
    T1Divide = NA_real_,
    T2Divide = NA_real_,
    SignalTime = NA_real_,
    CurrencyType = "USD"
  )


US <- select(`MTurk ID`, IPAddress, Progress, sample, vignette, signal, T1Belief, T1Action, T1Divide,
             T2Belief, T2Action, T2Divide, T3Action, T1Angry, T1Sad, T1Suicidal, T1MentallyIll,
             T1Depressed, T1Guilty, T1Calm, T1Neutral, T1Scared, T1Tired, T1Distressed, T1Devious,
             T1Jealous, T1Confident, T1Traumatized, T1Violated, T1NoneOfAbove, T2Angry, T2Sad,
             T2Suicidal, T2MentallyIll,T2Depressed, T2Guilty, T2Calm, T2Neutral, T2Scared, T2Tired,
             T2Distressed, T2Devious,T2Jealous, T2Confident, T2Traumatized, T2Violated,
             T2NoneOfAbove, Age, Sex, Sons, Daughters, `Rel Status`, Ed, Income, CurrencyType, Feedback,
             TotalTime, VignetteTime, SignalTime, T4AttentionCheck, T4AttentionCheckFail, .data = dUS)

col_names2 <- names(read_csv("data-raw/2020IndianSampleRaw.csv", n_max = 0))
dIndia <- read_csv("data-raw/2020IndianSampleRaw.csv", col_names = col_names2, skip = 6) %>%
  mutate(
    vignette = "ThwartedMarriage",
    sample = "Indian")%>%
  mutate(
    signal = FL_14_DO,
    signal = str_replace(signal, ":ThwartedMarriage", ""),
    signal = str_replace(signal, "Crying\\+Change", "CryingwithBehaviorChange")) %>%
      mutate(
        T1Belief = `Marriage Belief T1_4`,
        T1Divide = `Marriage Divide T1_4`,
        T1Action = `Marriage Action T1_4`,
        T2Belief = `Marriage Belief T2_4`,
        T2Divide = `Marriage Divide T2_4`,
        T2Action = `Marriage Action T2_4`,
        T3Action = `Marriage Action T3_1`,
        T4AttentionCheck = `T4 Check_1`,
        T4AttentionCheckFail = ifelse(T4AttentionCheck == '0', 0, 1)
      ) %>%
  mutate(
    VignetteTime = (sumrow = `Time P1_Page Submit` + `Time P2_Page Submit` + `Time P3_Page Submit`
                    + `Time P4_Page Submit` + `Time P5_Page Submit`),
    VignetteTime = as.numeric(VignetteTime),
    SignalTime = paste0( `TM Verbal Time_Page Submit`, `TM Crying Time_Page Submit`,
                  `TM CryingChange Time_Page Submit`, `TM Depresion Time_Page Submit`,
                  `TM Suicide Time_Page Submit`),
    SignalTime = as.numeric(str_replace_all(SignalTime, "NA", "")),
    TotalTime = `Duration (in seconds)`
  ) %>%
  mutate(
    T1Angry = ifelse(str_detect(`Feel T1`, 'Angry'), 1, 0),
    T1Sad = ifelse(str_detect(`Feel T1`, 'Sad'), 1, 0),
    T1Suicidal = ifelse(str_detect(`Feel T1`, 'Suicidal'), 1, 0),
    T1MentallyIll = ifelse(str_detect(`Feel T1`, 'Mentally ill'), 1, 0),
    T1Depressed = ifelse(str_detect(`Feel T1`, 'Depressed'), 1, 0),
    T1Guilty = ifelse(str_detect(`Feel T1`, 'Guilty'), 1, 0),
    T1Calm = ifelse(str_detect(`Feel T1`, 'Calm'), 1, 0),
    T1Neutral = ifelse(str_detect(`Feel T1`, 'Neutral'), 1, 0),
    T1Scared = ifelse(str_detect(`Feel T1`, 'Scared'), 1, 0),
    T1Tired = ifelse(str_detect(`Feel T1`, 'Tired'), 1, 0),
    T1Distressed = ifelse(str_detect(`Feel T1`, 'Distressed'), 1, 0),
    T1Devious = ifelse(str_detect(`Feel T1`, 'Devious'), 1, 0),
    T1Jealous = ifelse(str_detect(`Feel T1`, 'Jealous'), 1, 0),
    T1Confident= ifelse(str_detect(`Feel T1`, 'Confident'), 1, 0),
    T1Traumatized = ifelse(str_detect(`Feel T1`, 'Traumatized'), 1, 0),
    T1Violated = ifelse(str_detect(`Feel T1`, 'Violated'), 1, 0),
    T1NoneOfAbove = ifelse(str_detect(`Feel T1`, 'None'), 1, 0),
  ) %>%
  mutate(
    T2Angry = ifelse(str_detect(`Feel T2`, 'Angry'), 1, 0),
    T2Sad = ifelse(str_detect(`Feel T2`, 'Sad'), 1, 0),
    T2Suicidal = ifelse(str_detect(`Feel T2`, 'Suicidal'), 1, 0),
    T2MentallyIll = ifelse(str_detect(`Feel T2`, 'Mentally ill'), 1, 0),
    T2Depressed = ifelse(str_detect(`Feel T2`, 'Depressed'), 1, 0),
    T2Guilty = ifelse(str_detect(`Feel T2`, 'Guilty'), 1, 0),
    T2Calm = ifelse(str_detect(`Feel T2`, 'Calm'), 1, 0),
    T2Neutral = ifelse(str_detect(`Feel T2`, 'Neutral'), 1, 0),
    T2Scared = ifelse(str_detect(`Feel T2`, 'Scared'), 1, 0),
    T2Tired = ifelse(str_detect(`Feel T2`, 'Tired'), 1, 0),
    T2Distressed = ifelse(str_detect(`Feel T2`, 'Distressed'), 1, 0),
    T2Devious = ifelse(str_detect(`Feel T2`, 'Devious'), 1, 0),
    T2Jealous = ifelse(str_detect(`Feel T2`, 'Jealous'), 1, 0),
    T2Confident= ifelse(str_detect(`Feel T2`, 'Confident'), 1, 0),
    T2Traumatized = ifelse(str_detect(`Feel T2`, 'Traumatized'), 1, 0),
    T2Violated = ifelse(str_detect(`Feel T2`, 'Violated'), 1, 0),
    T2NoneOfAbove = ifelse(str_detect(`Feel T2`, 'None'), 1, 0),
  ) %>%
  mutate( #typo fix
    Daughters = Daugthers,
    `Rel Status` = RelStat,
    `MTurk ID` = as.character(MTurkCode),
    CurrencyType = "Rupees"
  )

India <- select(`MTurk ID`, IPAddress, Progress, sample, vignette, signal, T1Belief, T1Action, T1Divide,
             T2Belief, T2Action, T2Divide, T3Action, T1Angry, T1Sad, T1Suicidal, T1MentallyIll,
             T1Depressed, T1Guilty, T1Calm, T1Neutral, T1Scared, T1Tired, T1Distressed, T1Devious,
             T1Jealous, T1Confident, T1Traumatized, T1Violated, T1NoneOfAbove, T2Angry, T2Sad,
             T2Suicidal, T2MentallyIll,T2Depressed, T2Guilty, T2Calm, T2Neutral, T2Scared, T2Tired,
             T2Distressed, T2Devious,T2Jealous, T2Confident, T2Traumatized, T2Violated,
             T2NoneOfAbove, Age, Sex, Sons, Daughters, `Rel Status`, Ed, Income, CurrencyType, Feedback,
             TotalTime, VignetteTime, SignalTime, T4AttentionCheck, T4AttentionCheckFail, .data = dIndia)


signal_recode_dict <-
  c(
    'Crying' = 'Crying',
    'CryingwithBehaviorChange' = 'Mild depression',
    'Depression' = 'Depression',
    'SuicideAttempt' = 'Suicide attempt',
    'VerbalRequest' = 'Verbal request'
  )

vignette_recode_dict <-
  c(
    'Basketball' = 'Basketball coach',
    'RomanticPartner' = 'Romantic partner',
    'Sister' = 'Brother-in-law',
    'ThwartedMarriage' = 'Thwarted marriage'
  )

signaling2020 <- bind_rows(US,India) %>%
  rename(
    MTurkID = `MTurk ID`,
    RelStatus = `Rel Status`
  ) %>%
  mutate(
    CompleteSurvey = !is.na(MTurkID),
    signal = signal_recode_dict[signal],
    vignette = vignette_recode_dict[vignette]
  )


usethis::use_data(signaling2020, overwrite = TRUE)

