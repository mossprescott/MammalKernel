# MammalKernel

Abstract syntax and interpreter for a simple, functional, un-typed meta-language with quotation
and pattern-matching.

This repository collects low-level code and definitions, along with some simple tests. It's meant to
form a foundation upon which to build a useful meta-programming system.

Unfortunately the low-level nature of everything makes it difficult to present simple examples.

## Overview

[Eval](Sources/MammalKernel/Eval.swift) implements an evaluator for simple expressions, with hooks
to handle quotations and pattern matching.

[Node](Sources/MammalKernel/Node.swift) defines a very flexible AST which can represent any type of
program or document, i.e. a *language*. For each language, appropriate node types and attributes
will be defined somewhere.

[Kernel](Sources/MammalKernel/Kernel.swift) specifies a minimal language (the Mammal `kernel` language).
A `Node` which expresses a program in the the kernel language can be evaluated to produce a result
`Node`.

[Reduce](Sources/MammalKernel/Reduce.swift) provides mechanisms for transforming programs from one
one language to another. *Reductions* translate an input (source) `Node` to an output `Node`.
Typically, these programs are themselves written in (or reduced to) the kernel language, which is
what makes this a meta-programming system.

## Also useful

[Zipper](Sources/MammalKernel/Zipper.swift) tracks a location within an AST, with operations to
navigate from node to node, and modify the node at the current location.

## Background

https://github.com/mossprescott/lorax is an implementation of an earlier incarnation of this system,
including a viewer/editor, along with a write-up of the goals.
