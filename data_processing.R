header_part_1 <- "<br><div class=\"watupro-relay-problem-header"
header_part_2 <- "\"><h1 class=\"watupro-relay-problem-header-h1\">"
header_part_3 <- ". feladat</h1><p class=\"watupro-relay-problem-header-p\">"
header_part_4 <- ". próba, "
header_part_5 <- " pontért</p></div>"
problem_part_1 <- "<div class=\"watupro-relay-problem-body\">"
problem_part_2 <- "</div>"

process_data <- function(data, name_of_data) {
  data <- data %>% 
  slice(rep(1:n(), each = 3)) %>% 
  mutate(text = gsub("##", "<br><br>", text)) %>%
  mutate(text = gsub("/\\[", "[latex]", text)) %>% 
  mutate(text = gsub("/\\]", "[/latex]", text)) %>% 
  mutate(Order = row_number()) %>% 
  mutate(attempt = case_when(Order %% 3 == 1 ~ 1,
                             Order %% 3 == 2 ~ 2,
                             Order %% 3 == 0 ~ 3)) %>% 
  mutate(Hints = case_when(attempt == 1 ~ "",
                           attempt == 2 ~ hint1,
                           attempt == 3 ~ hint2)) %>% 
  mutate(point_of_attempt = point + 1 - attempt) %>% 
  mutate(image_url = case_when(image == 1 ~ paste('<p><img class="watupro-relay-problem-img" src="https://online.durerinfo.hu/wp-content/uploads/fsor_kepek/', 
                                                  name_of_data, '_', question, '.png" alt=\'\' /></p>', sep =""), 
                               is.na(image) ~ "")) %>% 
  mutate(previous = case_when(attempt == 1 ~ "",
                              attempt == 2 ~ "-wrong",
                              attempt == 3 ~ "-wrong")) %>% 
  mutate(Question = paste(header_part_1, previous, 
                          header_part_2, question, 
                          header_part_3, attempt, 
                          header_part_4, point_of_attempt, 
                          header_part_5, 
                          problem_part_1, text, 
                          problem_part_2, 
                          image_url,
                          sep ="")) %>%
  mutate(exclude = case_when(attempt == 1 ~ 0,
                             (attempt == 2 |  attempt == 3) ~ 1)) %>% 
  mutate(type = "textarea", category = "", explanation = "", required = 1, correct_condition = "any", fill_gap = "0.00/0.00", 
         sorting = "", max_selection = 0, inactive = 0, survey = 0, feedback = "", open_mode = "exact", tags = "||", 
         open_display = "text", compact_format = 0, round = 0, important = 0, difficulty = "", penalty = "0.00",
         multiple_gaps = "", answer_columns = 0, randomize = 0, is_correct = 1) %>% 
  select(Question, type, Order, category, explanation, required, correct_condition, fill_gap, sorting, max_selection, inactive, 
         survey, feedback, open_mode, tags, open_display, exclude, Hints, compact_format, round, important, difficulty, penalty,
         multiple_gaps, answer_columns, randomize, answer, is_correct, point_of_attempt)

col_names = c("Question",	"Answer Type",	"Order",	"Category",	"Explanation/Feedback",	
              "Required?",	"Correct answer condition",	"Fill the gap/sorting points",	
              "Sorting Answers",	"Max selections",	"Is Inactive?",	"Is survey?",	"Elaborate answer feedback",	
              "Open end mode",	"tags",	"Open-end question display style",	
              "Exclude from showing on the final screen? (0 or 1)",	"Hints",	
              "Display in compact format? (0 or 1)",	"Round the points to the closest decimal? (0 or 1)",	
              "Is this an important question? (0 or 100)",	"Difficulty level",	"Penalty for non-answering",	
              "Multiple gaps as drop-downs",	"Answer columns",	"Don't randomize answers",	
              "Answer",	"Is Correct?",	"Points") 

setnames(data, col_names)

}