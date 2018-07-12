Mashable: What makes online news popular?

November 21, 2017
In the age of social media, the rate of content consumption and production is
constantly and rapidly increasing. Therefore, it makes sense to analyze what influences
the ‘sharability’ of digital content, or its propensity to be shared throughout social media.
Since content sharing throughout social media tends to have a snowball effect, and
traffic is profit for online content companies such as Mashable, Vox, Buzzfeed, and
Distractify, who have built their brands on top of creating content that is exceedingly
sharable, it would serve well to know exactly what factors tend to propel an article
forward on social media.
 In this project, we analyzed slightly under two years’ worth of data from
Mashable.com news articles to predict the number of shares a given article would
receive. Factors analyzed included predictors such as keyword analysis, day of the week
published, article topic, closeness to other popular article topics, number of
images/videos contained within the article, and number of articles linked/referenced
within the article and their previous popularity, among others. The aim of this analysis is
to determine the set of factors that are most directly tied to the number of shares an
article will receive, so future content could be analyzed to determine its potential for
spread via social media before it is even launched.
 Using the SVM Regression, Random Forest, Regression Tree, and Bootstrap
Tree methods, we performed regression analysis and ultimately determined that it is
difficult to predict the exact number of shares an article would receive using these
factors. The models tended to be reasonably accurate in predicting articles around the
median (1500), but struggled with those that were more popular or less popular. Among
the models used, the SVR model had the best performance on the data. Factors that
were consistently found to be important in predicting the number of shares included
being published on a Saturday/weekend, the average keyword’s average and maximum
number of shares (extracted from the pages’ metadata), and the article’s closeness in
subject and content to certain previous popular topics. 
