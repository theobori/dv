# Dependencies visualizer

[![build-then-test](https://github.com/theobori/dv/actions/workflows/build-then-test.yml/badge.svg)](https://github.com/theobori/dv/actions/workflows/build-then-test.yml)

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

dv is a KISS Emacs package which contains a tool for visualizing Emacs package dependencies, it's based on the dot program. The aim is to have a graph showing all the dependencies involved, which can be Emacs packages, files or folders.

## Getting started

To use the project you need [Emacs](https://www.gnu.org/software/emacs/) with a version higher or equal than `30.1`, [Graphviz](https://graphviz.org) and [GNU Make](https://www.gnu.org/software/make/) if you want to build and install it manually.

## Installation

To install it manually, download the code from this [GitHub repository](https://github.com/theobori/dv) and then load it. To do this, you can use the following command lines.

```bash
make install
```

Then you can evaluate the following ELisp expression.

```emacs-lisp
(add-to-list 'load-path (file-name-concat user-emacs-directory "manual-packages" "dv"))
```

## How it works

Emacs package metadata retrieval is similar to the `describe-package` function, in that the package cache is first consulted before automatically installing the package from the package archive if it isn't there. In practical terms, Emacs package metadata, at runtime, is represented through `package-desc` objects.

### Node relationships

Several node relationships are possible with the current implementation.

- `package` nodes can only have ”package" child nodes.
- `filepath` nodes can have both `filepath` and `package` child nodes.
- `dirpath` nodes cannot have parent nodes and can only have `filepath` child nodes.

Circular imports are allowed and supported.

## Example

Below is an example of an SVG image produced by the `dv-dirpath` Emacs command.

![example](/assets/example.svg)

## Contribute

If you want to help the project, you can follow the guidelines in [CONTRIBUTING.md](./CONTRIBUTING.md).
