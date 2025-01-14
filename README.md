# gptbot
This is my code that gives me my personal AI assistant, that I call Atlas. It is much cheaper than paying $20 a month in subs as payment is by token.

Create a token with openai top use this bot, and insert your key. 

Can be deployed to shiny server for mobile access.

It has basic authentication if you'd like to share the bot with friends or family.

# Set environment variable for the OpenAI API key
Sys.setenv(OPENAI_API_KEY = "sk-YOURKEY")

Set up a user database or comment out these lines:
# Simple user database 
users <- data.frame(
  username = c("user1", "user2","user3","user4"),
  password = c("pw1", "pw2","pw3","pw4"), 
  stringsAsFactors = FALSE
)
