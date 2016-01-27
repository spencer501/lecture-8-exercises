  # Code demonstrated throughout the slides
  
  # Select helper functions and select_ -------------------------------
  # Make a students data.frame
  students <- data.frame(
    names=c('Mason', 'Tabi', 'Bryce'),
    math_exam1 = c(91, 82, 93), 
    math_exam2 = c(88, 79, 77),
    spanish_exam1 = c(79, 88, 92), 
    spanish_exam2 = c(99, 92, 92)
  )
  
  # Select students + math grades
  math_grades <- select(students, names, math_exam1, math_exam2)
  
  # Better yet, using "contains"
  math_grades <- select(students, names, contains("math"))
  
  # See also: starts_with, ends_with, matches
  
  
  # Use select_ to select using strings
  math_grades <- select_(students, 'names', 'math_exam1', 'math_exam2')
  
  # Subject of interest (why this matters):
  exam_of_interest <- 'math_exam1'
  exam_grades <- select_(students, 'names', exam_of_interest)
  sorter <- paste0('desc(', exam_of_interest, ')')
  sorted_test <- arrange_(students, sorter)
  
  # Summarise -------------------------------
  
  # Compute values of interest
  summarise(students, 
              mean_math1 = mean(math_exam1), 
              mean_math2 = mean(math_exam2),
              mean_math_scores=mean((math_exam1 + math_exam2) / 2)
            )

# Reshaping (we won't focus too much on this today) -------------------------------

varying <- c('math_exam1', 'spanish_exam1', 'math_exam2', 'spanish_exam2')
students_long <- reshape(students, 
        timevar='exam', 
        idvar='names', 
        v.names = 'score',
        times=varying, 
        varying=varying,
        direction="long") %>% arrange(names, score)


# Groupby and summarise ------------------------------- 

# Which student has the highest average?
best_avg <- students_long %>% 
            group_by(names) %>% 
            summarise(avg = mean(score)) %>%
            filter(avg == max(avg))

# Which exam was most difficult?
hardest_exam <- students_long %>% 
                group_by(exam) %>%
                summarise(avg = mean(score)) %>% 
                filter(avg == min(avg))

# Student with highest math grade 
best_math_grade <- students_long %>% 
                  filter(grepl('math', exam)) %>% 
                  group_by(names) %>% 
                  summarise(math = mean(score)) %>% 
                  filter(math == max(math))
                  
# Worst spanish score
hardest_spanish_test <- students_long %>% 
                        filter(grepl('spanish', exam)) %>% 
                        group_by(exam) %>%
                        summarise(avg = mean(score)) %>% 
                        filter(avg == min(avg))

# Left joins -------------------------------

# Student majors
majors <- data.frame(
  student_id=c(1, 2, 3),
  major=c('sociology', 'math', 'biology')
)

# Student contact info
contact_info <- data.frame(
  student_id=c(1,2), 
  cell=c('382-842-5873', '593-254-5834')
)

# Join data.frames

joined <- left_join(majors, contact_info, by='student_id')

# Order matters!
joined <- left_join(contact_info, majors, by='student_id')