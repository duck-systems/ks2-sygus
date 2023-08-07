# ks2 SyGuS
A solver plugin for the ks2 synthesizer suite using standalone SyGuS solvers.

## Supported SemGuS Problems
Only SemGuS problems that can be represented as SyGuS problems are supported. In practice, this means that:
* each CHC much be of the form `X.Sem(t0(t1,...), in, out) <= Y.Sem(t1, in, y1) ^ out = f(y1,...)`
* constraints must be (for now) limited to PBE - more to come later
* CHCs may only have a single output variable
* only one set of semantics in examples
* only one CHC per production
* CHCs must be total; that is, no guard clauses

*more details pending*
