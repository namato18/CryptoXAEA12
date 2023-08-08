library(httr)
library(jsonlite)

apiKey = ""
prompt = "Hello, how many countries are there in the world?"

response <- POST(
  url = "https://api.openai.com/v1/chat/completions", 
  add_headers(Authorization = paste("Bearer", apiKey)),
  content_type_json(),
  encode = "json",
  body = list(
    model = "gpt-3.5-turbo",
    temperature = 1,
    messages = list(list(
      role = "user", 
      content = prompt
    ))
  )
)

request_char = rawToChar(response$content)
df.response = jsonlite::fromJSON(request_char, flatten = TRUE)
text.response = df.response$choices$message.content
print(text.response)
