"JHU - Data Science Capstone"
========================================================
author: Rene Recktenwald
date: 2021-02-22 
autosize: true


Getting Started
========================================================

- Students were given a corpus containing blogs, news articles, and tweets
- I started by cleaning the data: removing special characters, converting everything to lower case and splitting each piece of text into individual words
- Then I built n-Grams for *n* = 2, 3, 4.

How the model works
========================================================

- My model is a very simple back-off model.
- It looks at the last 3 words of the user's input, then it checks if it has at least one 4-Gram with starting with them. If so: predict the most frequent one.
- If we do not find a suitable 4-Gram, then we consider 3-Grams next and repeat. 
- If even 2-Grams fail, we predict the word "the".

How the application works
========================================================

- Simply type a sentence in the input field.
- Similar to applications like SwiftKey, I did not want the user to have to press a "Submit" button, so I just show the predicted word directly.
- The application also shows what the model sees, i.e. I apply the same cleaning function to the user input, that I used for the corpus.

Future Work
========================================================
There are various improvements that can easily be made. We list some of them here

- Use a more sophisticated back-off model, like the one due to Katz.
- Use more values of *n*.
- Do more careful preprocessing. Currently "I'm cooking while he's cleaning" would get processed into "im cooking while hes cleaning", but it would be better to process it into "i am going to cook while he is cleaning".
- Use a spellchecker first, to fix typos in the corpus before creating the model.
