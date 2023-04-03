# `bkmk`

## Table of Contents
* [Manual](#manual)
  * [Usage](#usage)
  * [Verbs](#verbs)
* [Requirements](#requirements)

## Manual

### Usage
`bkmk <verb>`\
`<verb>` is followed by its respective arguments.  If `bkmk` is invoked
without a verb specified, the behaviour is identical to `bkmk view`.

### Verbs

* `add <book name> <page number>`\
  This adds a bookmark and assigns it a number for ease of later reference. 
  A bookmark may not necessarily be assigned a chronologically sequential
  number.
  ```
  $ bkmk add 'Song of Solomon' 69
  Added: ‘Song of Solomon’ pp.69
  ```
* `update <book number> <new page number>`\
  This updates the page number of the bookmark referred to by `<book number>`.
  ```
  $ bkmk update 0 70
  ‘Song of Solomon’: 69 ⇒ 70
  ```
* `view`\
  This displays a table of the stored bookmarks and is the behaviour implied
  in the absence of a verb.
  ```
  ┌─────┬─────────────────┬──────────────┐
  │   # │ Name            │ Page number  │
  ├─────┼─────────────────┼──────────────┤
  │   0 │ Song of Solomon │ 69           │
  └─────┴─────────────────┴──────────────┘
  ```
* `remove <book number>`\
  This removes the bookmark referred to by `<book number>`.
  ```
  $ bkmk remove 0
  Removed: ‘Song of Solomon’
  ```
* `help`\
  This displays a quick help for the user.

Also, the first letter of each verb is its alias — i.e. a verb can be referred
to by its first letter instead of the full word.

## Requirements
| Type        | Name           | Comment                               |
| ----------- | -------------- | :-----------------------------------: |
| Interpreter | CPython ≥3.9.2 | `bkmk` was tested with CPython  3.9.2 |
| Library     | colorama       | —                                     |
