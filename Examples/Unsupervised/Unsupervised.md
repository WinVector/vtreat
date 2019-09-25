Unsupervised.Rmd
================

# Using vtreat with Unsupervised Problems and Non-Y-aware data treatment

Nina Zumel and John Mount September 2019

Note this is a description of the [`R` version of
`vtreat`](https://github.com/WinVector/vtreat), the same example for the
[`Python` version of `vtreat`](https://github.com/WinVector/pyvtreat)
can be found
[here](https://github.com/WinVector/pyvtreat/blob/master/Examples/Unsupervised/Unsupervised.md).

## Preliminaries

\#%% md

Load modules/packages.

\#%%

import pkg\_resources import pandas import numpy import numpy.random
import seaborn import matplotlib.pyplot as plt import vtreat import
vtreat.util import wvpy.util

\#%% md

Generate example data.

  - `y` is a noisy sinusoidal plus linear function of the variable `x`
  - Input `xc` is a categorical variable that represents a
    discretization of `y`, along with some `NaN`s
  - Input `x2` is a pure noise variable with no relationship to the
    output
  - Input `x3` is a constant variable

\#%%

def make\_data(nrows): d = pandas.DataFrame({‘x’:\[0.1\*i for i in
range(500)\]}) d\[‘y’\] = numpy.sin(d\[‘x’\]) + 0.01*d\[‘x’\] +
0.1*numpy.random.normal(size=d.shape\[0\]) d\[‘xc’\] = \[‘level\_’ +
str(5\*numpy.round(yi/5, 1)) for yi in d\[‘y’\]\] d\[‘x2’\] =
numpy.random.normal(size=d.shape\[0\]) d\[‘x3’\] = 1
d.loc\[d\[‘xc’\]==‘level\_-1.0’, ‘xc’\] = numpy.nan \# introduce a
nan level return d

d = make\_data(500)

d.head()

\#%% md

### Some quick data exploration

Check how many levels `xc` has, and their disribution (including `NaN`)

\#%%

d\[‘xc’\].unique()

\#%%

d\[‘xc’\].value\_counts(dropna=False)

\#%%
md

## Build a transform appropriate for unsupervised (or non-y-aware) problems.

The `vtreat` package is primarily intended for data treatment prior to
supervised learning, as detailed in the
[Classification](https://github.com/WinVector/pyvtreat/blob/master/Examples/Classification/Classification.ipynb)
and
[Regression](https://github.com/WinVector/pyvtreat/blob/master/Examples/Regression/Regression.ipynb)
examples. In these situations, `vtreat` specifically uses the
relationship between the inputs and the outcomes in the training data to
create certain types of synthetic variables. We call these more complex
synthetic variables *y-aware variables*.

However, you may also want to use `vtreat` for basic data treatment for
unsupervised problems, when there is no outcome variable. Or, you may
not want to create any y-aware variables when preparing the data for
supervised modeling. For these applications, `vtreat` is a convenient
alternative to: `pandas.get_dummies()` or
`sklearn.preprocessing.OneHotEncoder()`.

In any case, we still want training data where all the input variables
are numeric and have no missing values or `NaN`s.

First create the data treatment transform object, in this case a
treatment for an unsupervised problem.

\#%%

transform = vtreat.UnsupervisedTreatment( cols\_to\_copy = \[‘y’\], \#
columns to “carry along” but not treat as input variables )

\#%% md

Use the training data `d` to fit the transform and the return a treated
training set: completely numeric, with no missing values.

\#%%

d\_prepared = transform.fit\_transform(d)

\#%% md

Now examine the score frame, which gives information about each new
variable, including its type and which original variable it is derived
from. Some of the columns of the score frame (`y_aware`, `PearsonR`,
`significance` and `recommended`) are not relevant to the unsupervised
case; those columns are used by the Regression and Classification
transforms.

\#%%

transform.score\_frame\_

\#%% md

Notice that the variable `xc` has been converted to multiple variables:

  - an indicator variable for each possible level, including `NA` or
    missing (`xc_lev_level_*`)
  - a variable indicating when `xc` was `NaN` in the original data
    (`xc_is_bad`)
  - a variable that returns how prevalent this particular value of `xc`
    is in the training data (`xc_prevalence_code`)

Any or all of these new variables are available for downstream modeling.

Also note that the variable `x3` did not show up in the score frame, as
it had no range (didn’t vary), so the unsupervised treatment dropped it.

\#%% md

Let’s look at the top of `d_prepared`, which includes all the new
variables, plus `y` (and excluding `x3`).

\#%%

d\_prepared.head()

\#%% md

## Using the Prepared Data to Model

Of course, what we really want to do with the prepared training data is
to model.

### K-means clustering

Let’s start with an unsupervised analysis: clustering.

\#%%

# don’t use y to cluster

not\_variables = \[‘y’\] model\_vars = \[v for v in d\_prepared.columns
if v not in set(not\_variables)\]

import sklearn.cluster

d\_prepared\[‘clusterID’\] = sklearn.cluster.KMeans(n\_clusters =
5).fit\_predict(d\_prepared\[model\_vars\]) d\_prepared.clusterID

# colorbrewer Dark2 palette

mypalette = \[‘\#1b9e77’, ‘\#d95f02’, ‘\#7570b3’, ‘\#e7298a’,
‘\#66a61e’\] ax = seaborn.scatterplot(x = “x”, y = “y”,
hue=“clusterID”, data = d\_prepared, palette=mypalette, legend=False)
ax.set\_title(“y as a function of x, points colored by (unsupervised)
clusterID”) plt.show()

\#%% md

### Supervised modeling with non-y-aware variables

Since in this case we have an outcome variable, `y`, we can try fitting
a linear regression model to `d_prepared`.

\#%%

import sklearn.linear\_model import seaborn import sklearn.metrics
import matplotlib.pyplot

not\_variables = \[‘y’, ‘prediction’, ‘clusterID’\] model\_vars = \[v
for v in d\_prepared.columns if v not in set(not\_variables)\] fitter =
sklearn.linear\_model.LinearRegression()
fitter.fit(d\_prepared\[model\_vars\], d\_prepared\[‘y’\])
print(fitter.intercept\_) {model\_vars\[i\]: fitter.coef\_\[i\] for i in
range(len(model\_vars))}

\#%%

# now predict

d\_prepared\[‘prediction’\] = fitter.predict(d\_prepared\[model\_vars\])

# get R-squared

r2 = sklearn.metrics.r2\_score(y\_true=d\_prepared.y,
y\_pred=d\_prepared.prediction)

title = ‘Prediction vs. outcome (training data); R-sqr =
{:04.2f}’.format(r2)

# compare the predictions to the outcome (on the training data)

ax = seaborn.scatterplot(x=‘prediction’, y=‘y’, data=d\_prepared)
matplotlib.pyplot.plot(d\_prepared.prediction, d\_prepared.prediction,
color=“darkgray”) ax.set\_title(title) plt.show()

\#%% md

Now apply the model to new data.

\#%%

# create the new data

dtest = make\_data(450)

# prepare the new data with vtreat

dtest\_prepared = transform.transform(dtest)

# apply the model to the prepared data

dtest\_prepared\[‘prediction’\] =
fitter.predict(dtest\_prepared\[model\_vars\])

# get R-squared

r2 = sklearn.metrics.r2\_score(y\_true=dtest\_prepared.y,
y\_pred=dtest\_prepared.prediction)

title = ‘Prediction vs. outcome (test data); R-sqr =
{:04.2f}’.format(r2)

# compare the predictions to the outcome (on the training data)

ax = seaborn.scatterplot(x=‘prediction’, y=‘y’, data=dtest\_prepared)
matplotlib.pyplot.plot(dtest\_prepared.prediction,
dtest\_prepared.prediction, color=“darkgray”) ax.set\_title(title)
plt.show()

\#%% md

## Parameters for `UnsupervisedTreatment`

We’ve tried to set the defaults for all parameters so that `vtreat` is
usable out of the box for most applications. Notice that the parameter
object for unsupervised treatment defines a different set of parameters
than the parameter object for supervised treatments
(`vtreat.vtreat_parameters`).

\#%%

vtreat.unsupervised\_parameters()

\#%% md

**coders**: The types of synthetic variables that `vtreat` will
(potentially) produce. See *Types of prepared variables* below.

**indicator\_min\_fraction**: By default, `UnsupervisedTreatment`
creates indicators for all possible levels (`indicator_min_fraction=0`).
If `indicator_min_fraction` \> 0, then indicator variables (type
`indicator_code`) are only produced for levels that are present at least
`indicator_min_fraction` of the time. A consequence of this is that
1/`indicator_min_fraction` is the maximum number of indicators that will
be produced for a given categorical variable. See the Example below.

**user\_transforms**: For passing in user-defined transforms for custom
data preparation. Won’t be needed in most situations, but see
[here](https://github.com/WinVector/pyvtreat/blob/master/Examples/UserCoders/UserCoders.ipynb)
for an example of applying a GAM transform to input variables.

**sparse\_indicators**: When True, use a (Pandas) sparse representation
for indicator variables. This representation is compatible with
`sklearn`; however, it may not be compatible with other modeling
packages. When False, use a dense representation.

### Example: Restrict the number of indicator variables

\#%%

# calculate the prevalence of each level by hand

d\[‘xc’\].value\_counts(dropna=False)/d.shape\[0\]

\#%%

transform\_common = vtreat.UnsupervisedTreatment( cols\_to\_copy =
\[‘y’\], \# columns to “carry along” but not treat as input
variables params = vtreat.unsupervised\_parameters({
‘indicator\_min\_fraction’: 0.2 \# only make indicators for levels
that show up more than 20% of the time }) )

transform\_common.fit\_transform(d) \# fit the transform
transform\_common.score\_frame\_ \# examine the score frame

\#%% md

In this case, the unsupervised treatment only created levels for the two
most common levels, which are both present more than 20% of the time.

In unsupervised situations, this may only be desirable when there are an
unworkably large number of possible levels (for example, when using ZIP
code as a variable). It is more useful in conjuction with the y-aware
variables produced by `NumericOutputTreatment`,
`BinomialOutcomeTreatment`, or `MultinomialOutcomeTreatment`.

## Types of prepared variables

**clean\_copy**: Produced from numerical variables: a clean numerical
variable with no `NaNs` or missing values

**indicator\_code**: Produced from categorical variables, one for each
level: for each level of the variable, indicates if that level was “on”

**prevalence\_code**: Produced from categorical variables: indicates how
often each level of the variable was “on”

**missing\_indicator**: Produced for both numerical and categorical
variables: an indicator variable that marks when the original variable
was missing or `NaN`

### Example: Produce only a subset of variable types

In this example, suppose you only want to use indicators and continuous
variables in your model; in other words, you only want to use variables
of types (`clean_copy`, `missing_indicator`, and `indicator_code`), and
no `prevalence_code` variables.

\#%%

transform\_thin = vtreat.UnsupervisedTreatment( cols\_to\_copy =
\[‘y’\], \# columns to “carry along” but not treat as input
variables params = vtreat.unsupervised\_parameters({ ‘coders’:
{‘clean\_copy’, ‘missing\_indicator’, ‘indicator\_code’, } }) )

transform\_thin.fit\_transform(d) \# fit the transform
transform\_thin.score\_frame\_
