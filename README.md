Natural language processing on the first 2016 presidential debate
==========

In this project I used R and the [Google Cloud Natural Language API](https://cloud.google.com/natural-language/) to analyze the transcript of the first 2016 presidential debate.

I measured the sentiment of Clinton’s and Trump’s responses, and examined how emotional their words were throughout the debate. I also looked at each candidate’s most commonly used adjectives. Building off the work of [Alvin Chang at Vox](http://www.vox.com/debates/2016/9/27/13070616/debate-clinton-trump-not-answers/in/12771101), I was also able to examine how the speech patterns of Clinton and Trump each changed when directly responding to and when skirting the questions.

My analysis is outlined in the blog post [here](http://fouriestseries.tumblr.com/).



File descriptions
----------

Each of the numbered R script runs independently of the others. All of the data generated from `01_transcript_cleaning.R` and `02_annotate_text.R` is included in the [data](https://github.com/BrianWeinstein/presidential-debate-nlp/tree/master/data) directory, so there's no need to regenerate those datasets.

- `01_transcript_cleaning.R` scrapes and cleans the annotated debate transcript from [Alvin Chang at Vox](http://www.vox.com/debates/2016/9/27/13070616/debate-clinton-trump-not-answers/in/12771101).
- `02_annotate_text.R` sends each line of the transcript to the Google Cloud Natural Language API, fetches the results, and does some light data cleaning.
- `03_sentiment.R` analyzes the sentiment of each candidate's responses.
- `04_compare_candidates.R` compares the speech patterns of Clinton and Trump.
- `05_compare_answers.R` compares the speech patterns of each candidate when directly responding to and when skiring the questions.
- `colors.R` defines the chart colors.
- `stopwords_ref.R` defines a list of stopwords, provided by the [tm](https://cran.r-project.org/web/packages/tm/index.html) R package.
- `logistic_helper.R` is a giant function that performs cross-validated logistic regression with lasso and returns a model, coefficients, performance metrics, etc.
- `google_nlp_api.R` holds helper functions to send POST requests to the Google Cloud Natural Language API (more on that below).


Data
----------

Each of the numbered R script runs independently of the others. All of the data generated from `01_transcript_cleaning.R` and `02_annotate_text.R` is in the [data](https://github.com/BrianWeinstein/presidential-debate-nlp/tree/master/data) directory. It includes the Vox transcript and all of the responses from the Google NLP API (sentiment, entity, and sentences, and tokens).


Using the [Google Cloud Natural Language API](https://cloud.google.com/natural-language/)
----------

`google_nlp_api.R` contains helper functions to send POST requests to the [Google Cloud Natural Language API](https://cloud.google.com/natural-language/) (more on that below). The API is in V1 beta, so these might stop working at any time.

To use the API, you'll need to [create a Google Cloud project and enable billing](https://cloud.google.com/natural-language/docs/getting-started), and get an [API key](https://developers.google.com/places/web-service/get-api-key).

You can create a file in the main directory (named `keys.R`) that defines your API key for later use.
```
google.api.key <- "YOUR_API_KEY"
```
