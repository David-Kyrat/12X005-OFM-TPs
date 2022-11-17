# Homework 3

In this homework, you will implement the algorithm to build the coverability graph of a Petri net.

The submission deadline is set to November the 18th 2022, at 23:59 Geneva local time.

## Coverability Graph

In the file `Homework/Analysis.fs`, fill out the missing implementation for the function `CoverabilityGraph.make`.
This function takes a model and an inital marking as inputs and it returns the coverability graph for the model.

Do not hesitate to take inspiration from the implementation of the algorithm to compute the marking graph of a Petri net which was provided to you in the previous homework.

A helper function called `setOmegas` is also provided to you in the `Analysis` module.
This function takes a marking and the set of its predecessors as input and, if there is a predecessor in the set that is smaller than or equal to the input marking, it returns a new version of this marking with token counts set to `ω` where necessary. 

## Solution structure

The solution for this homework is organised as follows:

- The implementation of Petri nets in F# is located in the file `PetriNet.fs` of the `Homework` project.
- The implementation of the coverability graph for a model is located in the file `Analysis.fs` of the `Homework` project.
- The tests for the coverability graph are located in the file `Analysis.Tests.fs` of the `Homework.Test` project.
