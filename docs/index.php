<?

include_once('geshi/geshi.php');

$geshi = new GeSHi('', 'scheme');
$geshi->enable_classes();
$geshi->set_overall_class('block');
$geshi->set_header_type(GESHI_HEADER_PRE_VALID);
$geshi->enable_line_numbers(GESHI_NORMAL_LINE_NUMBERS);
$geshi->set_overall_style('font-family: monospace; font-size: 10pt;' .
                          'background-color: #f0f0f0;' .
                          'border: 1px solid black; margin: 1em;');
$geshi->set_line_style('font-size: normal; font-weight: normal;' .
                       'vertical-align: top;');

$geshi_inline = new GeSHi('', 'scheme');
$geshi_inline->enable_classes();
$geshi_inline->set_overall_class('inline');
$geshi_inline->set_header_type(GESHI_HEADER_NONE);
$geshi_inline->set_overall_style('font-family: monospace; font-size: 10pt;');

$keywords = array('peg-parser', 'peg-trace');

$geshi->add_keyword_group(2, 'color: #A00000;', false, $keywords);
$geshi_inline->add_keyword_group(2, 'color: #A00000;', false, $keywords);

function show_code($code) {
	global $geshi;
	$geshi->set_source($code);
	echo $geshi->parse_code();
}

function inline_code($code) {
	global $geshi_inline;
	$geshi_inline->set_source($code);
	echo '<span class="scheme inline">';
	echo $geshi_inline->parse_code();
	echo '</span>';
}

?>
<html>
<head>
<title>Parsing Expression Grammar Parsing Expression Generator (PEGPEG)</title>
<style type="text/css">
<? echo $geshi->get_stylesheet(false);
   echo $geshi_inline->get_stylesheet(false); ?>
body { font-family: sans-serif; width: 700px; background-color: #fcfcfc; font-size: small; }
p { margin: 0; margin-left: 1em; margin-bottom: 1em; text-align: justify; }
.header { font-size: large; font-weight: bold; text-align: center; }
.section { font-size: large; font-weight: bold; padding: 0 0 1em 0; }
.section .scheme.inline { font-size: large; }
</style>
</head>
<body style="">
<div class="header" style="font-size: large">Parsing Expression Grammar Parsing Expression Generator (PEGPEG)</div>
<div class="header" style="font-size: small; padding-bottom: 1em">Scott A. Dial</div>

<div class="section">1. PEG Parser: <? inline_code('(peg-parser [binding ...] nonterminal ...)') ?></div>

<div>The <? inline_code("(peg-parser)") ?> syntatic form produces a procedure
of one argument, a thunk which is to yield the input one value at a time. The
result of the application of the procedure is either the semantic value or
a <? inline_code("peg-parse-error") ?> record describing why the input from
the generator was rejected.</div>

<? show_code('; L = {a^n b^n c^n : n>= 1}
(define abc-parser
  (peg-parser
    [(a A) (b B)]
    (S
      [((& a (! "b")) (+ "a") b (! "c")) #t])
    (A
      [("a" (? a) "b") #t])
    (B
      [("b" (? b) "c") #t])))'); ?>

<div class="section">2. PEG Expressions</div>

<div class="section">2.1. Nonterminals (symbols)</div>

<p>
Any symbol appearing in a PEG expression is treated as a nonterminal reference.
A symbol's root prefix is matched against the bindings form to determine the
nonterminal it represents. A root prefix can be any valid scheme identifier but
must not end with any number of digits. Within a PEG expression, you may append
any number of digits to an identifier declared in the bindings to obtain a
unique identifier. The value of the semantic action applied for that
nonterminal will be bound to that unique identifier.
</p>

<? show_code('(peg-parser
  [(foo F)]
  (S
    [(foo0 foo1)
     (list foo0 foo1)]
  (F
    ["foo" \'foo])))'); ?>

<p>
In the example above, both of the symbols "<? inline_code('foo0') ?>" and
"<? inline_code('foo1') ?>" references the binding
"<? inline_code('(foo F)') ?>", which binds all symbols of the form
"<? inline_code('foo[0-9]+') ?>" to the nonterminal named
"<? inline_code('F') ?>".
</p>

<div class="section">2.2. Terminals (values)</div>

<p>
Any scheme value can be matched as a value (a value is matched if the value
from the input stream generator is "<? inline_code('eqv?') ?>" or
"<? inline_code('char=?') ?>" to the terminal), however the PEGPEG expression
grammar has been optimized for the purposes of matching character-based
inputs. Specifically, strings are automatically expanded into a sequence of
character matches (e.g., "<? inline_code('"abc"') ?>" is matched as
"<? inline_code('(#\a #\b #\c)') ?>"). In order to match values that have been
protected, one must escape the expression grammar via unquoting (i.e.,
"<? inline_code('(unquote "abc")') ?>" or "<? inline_code(',"abc"') ?>").
</p>

<div class="section">2.11. Ranges: <? inline_code('(a - b)') ?></div>

<p>
The "<? inline_code('(a - b)') ?>" expression will match a range of values,
inclusively. For example, "<? inline_code('(#\0 - #\9)') ?>" will match any
digit character. For consistency, the range expression supports the automatic
expansion of strings (only for strings of length 1); the expression
"<? inline_code('("0" - "9")') ?>" is treated identically to the previous
example. Integer ranges can also be matched, to support the use case of
tokens (e.g., "<? inline_code('(0 - 9)') ?>").
</p>

<div class="section">2.12. Any: <? inline_code('%') ?></div>

<p>
The "<? inline_code('%') ?>" symbol will match any single value from the
input stream. In the common case that you want to know what value was matched,
one can use assignment to capture the input stream that matched. The
expression "<? inline_code('(c <- %)') ?>" will match any character and assign
the list of matching values (of length 1) from the input stream. The
"<? inline_code('%') ?>" will not match if there are no more values available
from the input stream. The expression "<? inline_code('(! %)') ?>" will only
match at the end of input.
</p>

<div class="section">2.3. Sequence: <? inline_code('(p0 p1 ...)') ?></div>

<p>The "<? inline_code('(p0 p1 ...)') ?>" expression will match the
subexpressions in-order.</p>

<div class="section">2.4. Ordered-choice: <? inline_code('(/ p0 p1 ...)') ?></div>

<p>The "<? inline_code('(/ p0 p1 ...)') ?>" expression will attempt to match only
one of the the subexpressions by trying each in-order. The expression fails only
if all of the subexpressions fail.</p>

<div class="section">2.5. Zero-or-More: <? inline_code('(* p0 p1 ...)') ?></div>

<p>
The "<? inline_code('(* p0 p1 ...)') ?>" expression will match the sequence
"<? inline_code('(p0 p1 ...)') ?>" zero or more times. If there are any
variables bound by the subexpressions, then those variables will be bound a
list of the values for each successful match. For instance,
"<? inline_code('(* some-nt)') ?>" will bind "<? inline_code('some-nt') ?>" to
the list of values returned by the nonterminal's semantic actions. In the
case of zero matches, "<? inline_code('some-nt') ?>" will be bound to the 
"<? inline_code('peg-unmatched') ?>" special value, which can be test for by
calling the procedure "<? inline_code('peg-unmatched?') ?>".
</p>

<p>
It's important to note that if there are any ordered-choices that bind
variables, a bit of indeterminism sneaks in. For instance, expressing "match
zero or more 'A's and 'B's could be expressed as:
"<? inline_code('(* (/ As Bs))') ?>". However, it is important to remember that
the variable "<? inline_code('As') ?>" will be bound to either
"<? inline_code('peg-unmatched') ?>" or the list of matches for
"<? inline_code('As') ?>"; instances of "<? inline_code('peg-unmatched') ?>"
will not be inserted into the list when "<? inline_code('Bs') ?>" matches. All
ordering information between the choices of "<? inline_code('As') ?>" and
"<? inline_code('Bs') ?>" will have been lost. In the case that this ordering
information is important, then you should factor out the subexpression as a
new nonterminal that handles it explicitly.
</p>

<div class="section">2.6. One-or-More: <? inline_code('(+ p0 p1 ...)') ?></div>

<p>
The "<? inline_code('(+ p0 p1 ...)') ?>" expression will match the sequence
"<? inline_code('(p0 p1 ...)') ?>" one or more times. The binding behavior is
identical to the zero-or-more operator.
</p>

<div class="section">2.7. Optional: <? inline_code('(? p0 p1 ...)') ?></div>

<p>
The "<? inline_code('(? p0 p1 ...)') ?>" expression will try to match the
sequence "<? inline_code('(p0 p1 ...)') ?>" but succeed even if it does not
match or consume any input.
</p>

<div class="section">2.8. And-predicate: <? inline_code('(& p0 p1 ...)') ?></div>

<p>
The "<? inline_code('(& p0 p1 ...)') ?>" expression will try to match the
sequence "<? inline_code('(p0 p1 ...)') ?>" but will not consume any input.
The and-predicate can be used as to lookahead in the input stream before
consuming any input to ensure the sequence matches.
</p>

<? show_code('(peg-parser
  [(nl Newline)]
  (Start
    [(* (/ nl0 %))
     (if (peg-unmatched? nl0)
       0
       (length nl))])
  (Newline
    [((& #\return (! #\newline)) #\return) \'r]
    [#\return #\newline \'rn]
    [#\newline \'n]))'); ?>

<p>
In the example above, the first case in <? inline_code('Newline') ?> 
("<? inline_code('[((& #\return (! #\newline)) #\return) #t]') ?>") ensures
that the return character must not be followed by a newline character before
consuming the return character. Although we have looked at the newline
character, it will not be consumed.
</p>

<div class="section">2.9. Not-predicate: <? inline_code('(! p0 p1 ...)') ?></div>

<p>
The "<? inline_code('(! p0 p1 ...)') ?>" expression will try to match the
sequence "<? inline_code('(p0 p1 ...)') ?>" but will not consume any input.
The not-predicate can be used as to lookahead in the input stream before
consuming any input to ensure the sequence does not match.
</p>

<div class="section">2.10. Assignment: <? inline_code('(s <- p0 p1 ...)') ?></div>

<p>
The expression "<? inline_code('(s <- p0 p1 ...)') ?>" binds the sequence of
values from the input stream that match the expression
"<? inline_code('(p0 p1 ...)') ?>" to "<? inline_code('s') ?>". For instance, the
expression "<? inline_code('(foobar <- (/ "foo" "bar")') ?>" will either bind
"<? inline_code('foobar') ?>" to "<? inline_code('\'(#\f #\o #\o)') ?>" or
"<? inline_code('\'(#\b #\a #\r)') ?>". If the subexpression consumes no input,
then the identifier will be bound to an empty list.
</p>

<p>
A common use-case for explicit assignment is for binding the match in a
character range or "any character" match. In other words, the expression
"<? inline_code('(c <- (/ ("a" - "z") ("A" - "Z")))') ?>" matches any alphabetic
character and the variable <? inline_code('c') ?> takes the list of matching
characters for the subexpression, a list of one character.
</p>

<div class="section">3. PEG Actions</div>

<div class="section">4. PEG Errors</div>

<div class="section">5. PEG Debugging</div>

In order to debug your parser, you should call
"<? inline_code('(peg-trace #t)') ?>" before the definition of your parser.
The enabling of tracing affects the code generated for your parser. With
tracing disabled, the code is simpler and faster. With tracing enabled, you
will be shown an extremely verbose trace of the matching successes and
failures in a recursive line-chart format. If there are no errors while
parsing, you will also be shown a similiar recursive line-chart showing the
final evaluation sequence of the semantic actions.

</body>
</html>
