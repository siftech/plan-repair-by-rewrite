# Plan Repair by Rewrite
### _Robert P. Goldman (SIFT, LLC)_

This is a system that implements the plan repair algorithm of Hoeller, et al. as described in the paper ["HTN Plan Repair via Model Transformation"](http://link.springer.com/10.1007/978-3-030-58285-2_7).

## Loading the library

The plan-repair-by-rewrite (`plan-repair-rewrite`) system is written in Common Lisp.  The easiest way to install and use it will be to install Quicklisp, and put this library (or a symbolic link to it) in your `~/quicklisp/local-projects/` directory.

You will also need to clone the repository for the [PDDL-tools library](https://github.com/rpgoldman/pddl-tools) because the version of that library included in Quicklisp is too old for use with the plan repair system.

Once these filesystem changes are made, start Common Lisp (we suggest either Allegro Common Lisp or SBCL; we have not tested with any other implementations) and, assuming Quicklisp is configured correctly, you should be able to do:

    (ql:quickload "plan-repair-rewrite")

## Using the library

The key data structure is the `execution-record`.  We do not yet have a user-friendly way to construct this from data files (we will do this as soon as possible).  The two exported, top-level functions are `repair-domain` and `repair-problem`.  These are all in the `plan-repair-rewrite` (or `prr`) package.

The file [replan-script.lisp](replan-script.lisp) shows how to use the library.  `setup-input-files` loads up the domain, problem and original plan, and creates an execution record, `*erecord*`.

This function is invoked by `build-repair-files`, which sets up the input files and then builds the repair domain and problem and writes them to `"repair.domain.hddl"` and `"repair-problem.hddl"`.  Those files can be used in any HDDL planner, e.g., PANDA, SHOP3, etc.

## Future plans

We have not yet, but we expect to add the ability build an executable that will generate the plan repair files from the command line, and also a Docker image that will run that executable.
    

## LICENSE

SIFT Proprietary, all rights reserved.


Copyright (c) 2023 Robert P. Goldman and SIFT, LLC.


