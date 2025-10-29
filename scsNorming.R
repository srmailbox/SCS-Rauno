library(readxl)

# First step is to read in the data
rawDataFile = 'ACUReadingInitiative.scsRaw.RDat'
if(file.exists(rawDataFile)) load(rawDataFile) else {
  scsRaw = read.csv('ACU Reading Initiative - Export - Export (2).csv'
                    , na.strings = c("Absent ", "absent ", "Absent", "absent"
                                     , "-", "NULL"))
  save(scsRaw, file=rawDataFile)
}

newDataFiles = list('DLS Revesby_edited_deid.xlsx', 'Trinity_edited_deid.xlsx', 'Marcellin Randwick 2024_edited_deid.xlsx')
names(newDataFiles)=c('DLS Revesby', 'Trinity', 'Marcellin Randwick')
newRaw=lapply(newDataFiles, read_excel, sheet='Data Entry', na="999") %>% 
  bind_rows(.id="School") %>% 
  rename_with(~gsub(" ", ".", .x, fixed=T)) %>% 
  filter(!is.na(SIS.ID)) %>% 
  select(-Match, -contains("Incorrect"))


# (Math 3, English 2, and Science 2)
scsOrig = scsRaw %>% rename(Test.1.Num.Incorrect= Test.1.Num.Correct.1) %>% select(-matches("PAT"), -matches("NAPLAN")) %>% 
  mutate(Course = str_extract(Class, "ENG|SCI|MAT|REL|HIS|GEO")
         , Course = ifelse(Course=="HIS", "GEO", Course)
         , Grade = str_split(Class, Course, simplify = T)[1]
         , Classroom = str_split(Class, Course, simplify = T)[2])

scs = rbind(
  scsOrig %>% select(School, SIS.ID, Course, Grade, Classroom, ends_with("Correct")) %>% 
    mutate(Time = factor("Late", levels=c("Early", "Late"))
           , Score = ifelse(Course=="ENG", Test.2.Num.Correct
                            , ifelse(Course=="MAT", Test.3.Num.Correct,
                                     ifelse(Course=="SCI", Test.2.Num.Correct, NA)))) %>% 
    filter(!is.na(Score)) %>% select(-ends_with("Correct"))
  , newRaw %>% pivot_longer(ends_with("Correct"), names_to="Course", values_to="Score") %>% 
    mutate(Course = substr(Course, 1,3), Grade=7, Time="Early", Classroom=1) %>% 
    filter(!is.na(Score))
) %>% 
  # pivot_wider(id_cols = c(School, SIS.ID, Grade, Classroom, Time)
  #             , names_from=Course, values_from=Score) %>% 
  data.frame

### Get quantiles

scsQuant.Time = merge(aggregate(Score~Course+Time, scs
                               , FUN = quantile, probs=c(.2), na.rm=T)
                     , aggregate(Score~Course+Time, scs
                                 , FUN = quantile, probs=c(.3), na.rm=T)
                     , by=c("Course", "Time"), suffixes=c(".2.t", ".3.t")
)

scsQuant.noTime = merge(aggregate(Score~Course, scs
                                  , FUN = quantile, probs=c(.2), na.rm=T)
                        , aggregate(Score~Course, scs
                                    , FUN = quantile, probs=c(.3), na.rm=T)
                        , by=c("Course"), suffixes=c(".2", ".3")
)

scsQuant.mean = merge(aggregate(Score.2.t~Course, scsQuant.Time, mean)
                      , aggregate(Score.3.t~Course, scsQuant.Time, mean)
                      ) %>% rename(Score.2.mean=Score.2.t, Score.3.mean=Score.3.t)

### Merge these with the data

scsNormed = merge(scs, scsQuant.Time, all.x=T) %>% 
  merge(scsQuant.noTime, all.x=T) %>% 
  merge(scsQuant.mean, all.x=T) %>% 
  rename_with(~gsub("Score\\.", "Quant\\.", .)) %>% 
  mutate(cat.Time = ifelse(Score<=Quant.2.t, "concern", ifelse(Score<Quant.3.t, "monitor", "ok"))
         , cat.Pooled = ifelse(Score<=Quant.2, "concern", ifelse(Score<Quant.3, "monitor", "ok"))
         , cat.Mean = ifelse(Score<=Quant.2.mean, "concern", ifelse(Score<Quant.3.mean, "monitor", "ok"))
         ) %>% 
  pivot_wider(id_cols = c(School, SIS.ID, Classroom, Time)
              , names_from=Course, values_from=c(Score, cat.Time, cat.Pooled, cat.Mean))

write.csv(scsNormed, file="scsNormed.csv", row.names = F)

write.csv(scsQuant.Time, file="scsNorms.CourseTime.csv", row.names = F)
write.csv(scsQuant.noTime, file="scsNorms.Pooled.csv", row.names = F)
write.csv(scsQuant.mean, file="scsNorms.Mean.csv", row.names = F)
