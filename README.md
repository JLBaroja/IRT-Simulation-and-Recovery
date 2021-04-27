# IRT Simulation and Recovery

This project analizes some Item Response Theory models by simulating data and recovering the parameter values that generated them.

Why? What for?

Beacause simulating is the best way of understanding what a model does and why it does it, and recovering is the best way to evaluate whether inference paradigms/techniques/software/scripts perform the way they should.

### Notes to myself

Still not sure how to structure the whole thing: initially I was thinking of a folder for data and another for inference, but now I'm considering opening a folder for each model. I guess some mixture of both could work, as the main goals are both to understand each model separately, but also to evaluate different inference (model selection) techniques working with a single dataset.

Storing simulations in '.RData' files for now, although I'm working on some format in plain text to make immediate exporting to other languages.

Still unhappy with the indexes: in most polytomous models, categories are represented by $j\in \{0,1,...,m-1\}$, which generates a mess translating to appropiate array positions in R. Maybe Python's starting-index-is-0 would make the whole thing easier...

Seems first recovery to the Graded Response Model is acceptable, although script works only for three categories: have to solve the beta-order problem: b1<b2<...<b{m-1}
