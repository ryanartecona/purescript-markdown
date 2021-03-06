# purescript-markdown

A Purescript library for parsing SlamData's dialect of Markdown, called *SlamDown*.

**Note**: *As mentioned on Twitter, SlamData is offering a $1k bounty to the first Github user who submits a pull request that we merge into this repository which satisfies the requirements of the library (detailed below). This is a fun way to learn Purescript, contribute an MIT-licensed library to the Purescript community, and earn a little cash.*

Direct comments / questions to [@jdegoes](http://twitter.com/jdegoes) on Twitter.

## Usage

*This is a proposed API and is subject to change.*

```purescript
-- parsing
case parseMd "# foo" of 
  Header1 (Text "foo") -> trace "matched!"
  _                    -> trace "did not match!"

-- rendering
(trace <<< renderMd <<< parseMd) "# foo"
```

### API

*This is a proposed API and is subject to change.*

```purescript
parseMd :: String -> SlamDown

class RenderMarkdown a
  renderMd :: SlamDown -> a

instance renderMarkdownString :: RenderMarkdown String

data SlamDown = ...

instance eqSlamDown :: Eq SlamDown

instance ordSlamDown :: Ord SlamDown

instance showSlamDown :: Show SlamDown

instance semigroupSlamDown :: Semigroup SlamDown

instance monoidSlamDown :: Monoid SlamDown
```

### Tests

*This is a proposed test suite and is subject to change.*

The tests use [purescript-strongcheck](http://github.com/purescript-contrib/purescript-strongcheck) to verify that an arbitrary `SlamDown` document can be rendered as a `String` and then parsed to a `SlamDown` equal to the original.

## Features

In general, SlamDown is a subset of [CommonMark](http://spec.commonmark.org/0.10/), supporting the following features:

* Leaf Blocks
  * Horizontal rules
  * ATX headers
  * Setext headers
  * Indented code blocks
  * Fenced code blocks
  * Linked reference definitions
  * Paragraphs
  * Blank line
* Container Blocks
  * Block quotes
  * List items
  * Lists
* Inlines
  * Backslash escapes
  * Entities
  * Code span
  * Emphasis and strong emphasis
  * Links
  * Images
  * Autolinks
  * Hard line breaks
  * Soft line breaks
  * Strings

Notably, HTML is not supported. Possibly, a safe subset of HTML could be added at some point in time.

The parser produces an ADT which describes the structure of the document.

## Extensions to CommonMark

SlamDown extends CommonMark in several ways:

 * **Evaluated code blocks** &mdash; These code blocks are evaluated by the Markdown application and results of the evaluation are inserted into the document.
 * **Form Elements** &mdash; Form elements may be named, given default values, and embedded into a document.

### Code Evaluation

Fenced code blocks may be evaluated by prefixing them with an exclamation point character (`!`). The result of evaluating the code is then inserted into the document at that location.

For example, in a document supporting evaluation of Javascript, the inline code block !`1 + 2` would be evaluated and the resulting number (`3`) would be inserted into the document at that location.

Code evaluation may be used for inline or block-level fenced code. 

If an info-string is specified, the evaluation must use the specified language or error. If no info-string is specified, the default language understood by the Markdown application is used.

**Note**: This library does not provide any support for evaluation of code, and the code snippets are treated as completely opaque, but the documentation does define *semantics* for how these blocks interact with other elements and with the rendering of the document.

### Form Elements

**Suggestions for better and / or less ambiguous syntax appreciated.**

Form fields allow the collection of named, weakly-typed user-input. All fields may have default values, and while it's possible to hard-code all static data and default values for all fields, it is also possible to use this feature in conjunction with code evaluation, so that data and default values are generated dynamically by evaluating code.

Although the suggested syntax has been modified to be more consistent (with respect to default values) and extended to include other types (e.g. dates and times), original credit to [Yevgeniy Brikman](http://brikis98.blogspot.com/2011/07/proposal-extend-markdown-syntax-to.html) for the idea of allowing forms in Markdown.

#### Text Input

```
name = ________ (default)

name = ________ (!`...`)
```

If code evaluation is used to produce the default, then the snippet must evaluate to textual content.

#### Radio Buttons

```
sex = (x) male () female

sex = (!`...`) !`...`
```

If code evaluation is used to produce the values, then the first snippet must evaluate to a label, and the second snippet must evaluate to a list of labels.

#### Checkboxes

```
phones = [] Android [x] iPhone [x] Blackberry

phones = [!`..`] !`...`
```

If code evaluation is used to produce the values, then the first snippet must evaluate to a list of booleans, and the second snippet must evaluate to a list of labels, and the two lists must have the same length.

#### Dropdowns

```
city = {BOS, SFO, NYC} (NYC)

city = {!`...`} (!`...`)
```

If code evaluation is used to produce the set of choices, the snippet must evaluate to a list of labels. If code evaluation is used to produce the default choice, the snippet must evaluate to a label.

### Date

```
start = __ - __ - ____ (06-06-2015)

start = __ - __ - ____ (!`...`)
```

If code evaluation is used to produce the default, the snippet must evaluate to a date.

### Time

```
start = __ : __ (10:32 PM)

start = __ : __ (!`...`)
```

If code evaluation is used to produce the default, the snippet must evaluate to a time.

### DateTime

```
start = __ - __ - ____ __ : __ (06-06-2015 12:00 PM)

start = __ - __ - ____ __ : __ (!`...`)
```

If code evaluation is used to produce the default, the snippet must evaluate to a date / time.

#### Required Fields

```
zip code* = ________
```

