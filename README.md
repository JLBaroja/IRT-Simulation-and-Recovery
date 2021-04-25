# IRT Simulation and Recovery

This projects analizes some Item Response Theory models by simulating data and recovering the parameter values that generated them.

Why? What for?
Beacause simulating is the best way of understanding what a model does and why does it do it, and recovering parameters is the best way to evaluate whether inference paradigms/techniques/software/scripts perform the way they should.

## Notes to myself

Still not sure how to structure the whole thing: initially I was thinking of a folder for data and another for inference, but now I'm considering opening a folder for each model. I guess some mixture of both could work, as the main goals are both to understand each model separately, but also to evaluate different inference (model selection) techniques working with a single dataset.

Storing simulations in '.RData' files for now, although I'm working on some format in plain text to make immediate exporting to other languages.
