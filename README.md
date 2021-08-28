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
The executable has these command line options:

```
-f, --features path/to/features.mang (Required)
     Phonological feature description file
-l, --language-base path/to/language/base.mang (Required)
     Language description file
-d, --language-dictionary path/to/language/dictionary.mang (Required)
     Language dictionary file
-D, --diachrony path/to/language/diachrony.mang
     Diachrony file
-c, --computer-readable
     Print the dictionary in a format that is uniquely parseable by Mang
```


# Features
A feature definition file starts with

```
# features
```

For an example of a complete feature definition file see
`test/features.mang`. That file does not contain any examples of binary
features.


## Definitions
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


## Feature Sets
Feature sets are used in multiple contexts. They are delimited with square
brackets `[]`. In them different feature states can be specified.


### Privative
To denote that a privative feature is present in a feature set, just put its
name into the feature set:

```
[bilabial, round]
```

denotes a rounded bilabial phoneme.

To denote a privative feature's absence, put a `~` before the name of the
feature:

```
[bilabial, ~round]
```

denotes an unrounded bilabial phoneme.


### Binary
To denote a binary feature being true, write it into the feature set with a
preceding `+`:

```
[+high]
```

To denote it being false, prefix it with a `-`:

```
[-high]
```


### Valued
A valued feature is written with it's name, followed by an `=`, followed by the
value the feature has:

```
[manner=stop]
```


# Language Definition
## Glyphs
### Disallowed Unicode Symbols

### Guaranteed Single Glyph


## Categories

## Phonotactics
### Syllable Based

### Cluster Based


## Learning Markov Chains


# Dictionary
## Glosses

## Parts of Speech

## Known Entries

## Unknown Entries


# Diachrony
## Sound Changes

## Semantic Shift
### Combining Words

### Dropping Words


## Augment
### Glyphs

### Categories


## Replace
### Glyphs

### Categories
