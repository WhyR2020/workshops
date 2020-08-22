# Reproductible data analysis with `drake`

Authors: Jakub Kwiecień

### Description

One of the challenges in complex analytical projects is management of analysis' pipeline. After few iterations of running the code, checking results and improving the scripts everything gets messy: codebase has swollen, number of artifacts has grown, same as number of (intermediate) results. In addition to that, rerunning the whole pipeline takes hours or maybe even weeks and you are not sure which parts you can skip after changing this strict inequality to a non-strict one in a config file. Fortunately, `drake` takes this problems away. It keeps track of all the inputs, analysis steps, outputs and relations between them and utilize this information to recompute only the steps that got outdated since the last run. This leads to enhanced reproducibility, maintainability and development speed and, of course, a happy coder. During the workshop you will learn how to set up drake workflow, how to maintain it and few tips about working with drake.