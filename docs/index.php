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

$keywords = array('peg-parser');

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
p { margin: 0; margin-left: 1em; margin-bottom: 1em; }
.header { font-size: large; font-weight: bold; text-align: center; }
.section { font-size: large; font-weight: bold; padding: 0 0 1em 0; }
.section .scheme.inline { font-size: large; }
</style>
</head>
<body style="">
<div class="header" style="font-size: large">Parsing Expression Grammar Parsing Expression Generator (PEGPEG)</div>
<div class="header" style="font-size: small">Scott A. Dial</div>

<div class="section">1. PEG Parser: <?inline_code('(peg-parser [binding ...] nonterminal ...)')?></div>

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

<div class="section">2.2. Terminals (values)</div>

<p>
Any scheme value can be matched as a value (a value is matched if the value
from the input stream generator is <? inline_code('eqv?') ?> to the terminal),
however the PEGPEG expression grammar has been optimized for the purposes of
matching character-based inputs. Specifically, strings are automatically
expanded into a sequence of character matches (e.g., <? inline_code('"abc"') ?> 
is matched as <? inline_code('(#\a #\b #\c)') ?>). In order to match values
that have been protected, one must escape the expression grammar via unquoting
(i.e., <? inline_code('(unquote "abc")') ?> or <? inline_code(',"abc"') ?>).
</p>

<div class="section">2.11. Ranges: <? inline_code('(a - b)') ?></div>

<p>The <? inline_code('(a - b)') ?> expression will match a range of values,
inclusively. For example, <? inline_code('(#\0 - #\9)') ?> will match any
digit character. For consistency, the range expression supports the automatic
expansion of strings (only for strings of length 1); the expression
<? inline_code('("0" - "9")') ?> is treated identically to the previous
example. Integer ranges can also be matched, to support the use case of
tokens (e.g., <? inline_code('(0 - 9)') ?>).

<div class="section">2.12. Any: <? inline_code('@') ?></div>

<p>The <? inline_code('@') ?> symbol will match any single value from the
input stream. In the common case that you want to know what value was matched,
one can use assignment to capture the input stream that matched. The
expression <? inline_code('(c <- @)') ?> will match any character and assign
the list of matching values (of length 1) from the input stream. The
<? inline_code('@') ?> will not match if there are no more values available
from the input stream. The expression <? inline_code('(! @)') ?> will only
"match" when the end of input.
</p>

<div class="section">2.3. Sequence: <? inline_code('(p0 p1 ...)') ?></div>

<div class="section">2.4. Zero-or-More: <? inline_code('(* p0 p1 ...)') ?></div>

<div class="section">2.5. One-or-More: <? inline_code('(+ p0 p1 ...)') ?></div>

<div class="section">2.6. Optional: <? inline_code('(? p0 p1 ...)') ?></div>

<div class="section">2.7. And-predicate: <? inline_code('(& p0 p1 ...)') ?></div>

<div class="section">2.8. Not-predicate: <? inline_code('(! p0 p1 ...)') ?></div>

<div class="section">2.9. Ordered-choice: <? inline_code('(/ p0 p1 ...)') ?></div>

<div class="section">2.10. Assignment: <? inline_code('(s <- p0 p1 ...)') ?></div>

<p>
The expression <? inline_code('(s <- p0 p1 ...)') ?> binds the sequence of
values from the input stream that match the expression
<? inline_code('(p0 p1 ...)') ?> to <? inline_code('s') ?>. For instance, the
expression <? inline_code('(foobar <- (/ "foo" "bar")') ?> will either bind
<? inline_code('foobar') ?> to <? inline_code('\'(#\f #\o #\o)') ?> or 
<? inline_code('\'(#\b #\a #\r)') ?>. If the subexpression consumes no input,
then the identifier will be bound to an empty list.
</p>

<p>
A common use-case for explicit assignment is for binding the match in a
character range or "any character" match. In other words, the expression
<? inline_code('(c <- (/ ("a" - "z") ("A" - "Z")))') ?> matches any alphabetic
character and the variable <? inline_code('c') ?> takes the list of matching
characters for the subexpression, a list of one character.
</p>

</body>
</html>
