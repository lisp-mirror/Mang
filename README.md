# Installation
## via Quicklisp
You will need SBCL and quicklisp already installed, and also need to know how to
use quicklisp.

On the command line do:

```
$ cd ~/quicklisp/local-projects/
$ git clone git@gitlab.com:Aksej/Mang.git
$ echo "mang/mang.simplest.asd\n" >> ./system-index.txt
$ cd [where/do/you/want/mang/to/live]
$ sbcl
```

Then in SBCL:

```lisp
* (ql:quickload "mang.simplest")
* (mang.simplest::compile-simplest-mang)
```

This will create the `simplest-mang` executable. Make it executable via

```
$ chmod +x ./simplest-mang
```



# Usage
## Executable
The executable has these command line options:

```
-f, ----features path/to/features.mang (Required)                      
     Phonological feature description file
-l, ----language-base path/to/language/base.mang (Required)            
     Language description file
-d, ----language-dictionary path/to/language/dictionary.mang (Required)
     Language dictionary file
-D, ----diachrony path/to/language/diachrony.mang                      
     Diachrony file
-c, ----computer-readable                                              
     Print the dictionary in a format that is uniquely parseable by Mang
```


## Feature Definition
A feature definition file starts with

```
# features
```

For an example of a complete feature definition file see
`test/features.mang`. This file does not contain any examples of binary
features.

### Privative Features
Privative features are either present or absent in a feature set. You can define
privative features like this:

```
privative : [feature1], [feature2], [feature3], ...
```


### Binary Features
Binary features can be true, false, or unset in a given feature set. Define
binary features like this:

```
binary : [feature1], [feature2], [feature3], ...
```


### Valued Features
Valued features have a defined set of possible values which they can
have. Valued features can also be absent from a feature set. They are defined
via:

```
valued : [feature] := [value1], [value2], [value3], ...
```



# Feature Sets



## Language Definition


## Dictionary


## Diachrony
