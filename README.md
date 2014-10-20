erlang-mas
======
[![Build Status](https://secure.travis-ci.org/ParaPhraseAGH/erlang-mas.svg?branch=master "Build Status")](http://travis-ci.org/ParaPhraseAGH/erlang-mas)

The ParaPhrase project aims to produce a new structured design and implementation process for heterogeneous parallel architectures, where developers exploit a variety of parallel patterns to develop component based applications that can be mapped to the available hardware resources, and which may then be dynamically re-mapped to meet application needs and hardware availability.

By using the massive and efficient parallelism enabled by ParaPhrase technologies, this work allows to achieve significant speedups of individual agents as well as build much bigger multi-agent systems.

## MAS framework

This repository contains a framework for creating Multi-Agent Systems. It can be used as a dependency in other projects, where certain callbacks have to be implemented in order for the system to work. A good example of using this framework is the [EMAS algorithm](https://github.com/ParaPhraseAGH/erlang-emas).

## Dependencies

To build the project on your machine you need:

* [Erlang (R17 or later)](http://www.erlang.org/)
* [Git](http://git-scm.com/)

## How to build the project

First you need to clone the repository:

    > git clone https://github.com/ParaPhraseAGH/erlang-mas.git
    > cd erlang/
    
To build the project you should use the Makefile:

    > make deps
    
Which will download and compile all dependencies and the project itself.

## How to run the project

The project can't be run by itself - it should be used as a dependency in an other project.
