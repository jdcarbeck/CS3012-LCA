# LowestCommonAncestor
Module to allow for the creation of a ordered binary tree from a list of Integers.
Any elements in the tree can be searched and the lowest common ancestor can be found.

project can be run with the command `./run.sh`

##Limitations
This software does provide a library to create DAGs and to determine the LCA between
them. But the implementation lacks key implementation to provide best possible run
time. Thus this program would need to be improved in order to be used for large sets
of data.
The input is also assumed to be a list of edges describing a DAG and does not provide
the functionality to build DAGs from any set of edges as at the moment there is no
checking of cycles.

## First Haskell Attempt
This is my first attempt to write a Haskell program. The process was tedious as I spent
much time learning the language before being able to implement it. The testing was the
best implementation that I could do without fully being able to understand the documentation
considering that im only a novice when it comes to Haskell
